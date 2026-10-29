# ==============================================================================
# app.R
# Multi-Asset Portfolio Studio: Custom HTML/CSS/JS Frontend + R-Shiny Backend
# No Bootstrap, no bslib, no shinydashboard, no htmlwidget layout dependencies
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(jsonlite)
  library(data.table)
  library(readxl)
  library(digest)
  library(cachem)
  library(promises)
  library(future)
})

r_modules <- c(
  "app_config.R",
  "data_import.R",
  "metadata.R",
  "fx_quotes.R",
  "return_transforms.R",
  "cash_returns.R",
  "risk_basis.R",
  "risk_diagnostics.R",
  "missing_history.R",
  "covariance_estimators.R",
  "exposures.R",
  "portfolio_metrics.R",
  "expectations_adapter.R",
  "scenario_paths.R",
  "validation.R",
  "exports.R",
  "cache_jobs.R",
  "session_state.R",
  "server_commands.R"
)

for (mod in r_modules) {
  source(file.path("R", mod), local = FALSE)
}

APP_CONFIG <- load_app_config()
GLOBAL_SHARED_CACHE <- create_cache_manager(max_size_mb = 256)
ASYNC_STATUS <- init_async_workers(max_workers = 1L)

# Pre-warm house dataset (Data.xlsx) for CHF/EUR/USD across all risk models in process-shared cache
if (file.exists(APP_CONFIG$default_workbook_path)) {
  tryCatch({
    warmup_state <- create_session_state(APP_CONFIG, shared_cache = GLOBAL_SHARED_CACHE)
    if (isTRUE(load_dataset_into_state(warmup_state, APP_CONFIG$default_workbook_path, is_private = FALSE)$valid)) {
      all_model_ids <- names(APP_CONFIG$risk_models) %||% APP_CONFIG$default_risk_model
      for (ccy in APP_CONFIG$supported_base_currencies) {
        for (mid in all_model_ids) {
          get_or_estimate_covariance(warmup_state, model_id = mid, base_ccy = ccy)
        }
      }
    }
  }, error = function(e) {
    message("[Startup Warmup Warning] ", conditionMessage(e))
  })
}

# Custom HTML Template UI without Bootstrap
ui <- function(req) {
  shiny::htmlTemplate("www/index.html")
}

server <- function(input, output, session) {
  state <- create_session_state(APP_CONFIG, shared_cache = GLOBAL_SHARED_CACHE)

  if (file.exists(APP_CONFIG$default_workbook_path)) {
    load_dataset_into_state(state, APP_CONFIG$default_workbook_path, is_private = FALSE)
  }

  # Automatically push initial bootstrap state once session finishes initial flush
  session$onFlushed(function() {
    tryCatch({
      init_req <- list(
        protocol_version = 1L,
        request_id = "srv_initial_bootstrap",
        type = "app.bootstrap",
        payload = list(
          base_currency = state$base_currency,
          model_id = state$active_model_id,
          portfolio_id = state$active_portfolio_id
        )
      )
      init_resp <- handle_app_command(init_req, state)
      session$sendCustomMessage("app_event", init_resp)
    }, error = function(e) {
      message("[Initial Bootstrap Warning] ", conditionMessage(e))
    })
  }, once = TRUE)

  # ExtendedTask for heavy Walk-Forward + Block-Bootstrap background validation
  validation_task <- shiny::ExtendedTask$new(function(basis_snap,
                                                      sample_snap,
                                                      a_vec_snap,
                                                      estimator_snap,
                                                      train_m,
                                                      test_m,
                                                      n_boot,
                                                      blk_len,
                                                      seed_val,
                                                      req_snapshot,
                                                      ctx_snapshot) {
    promises::future_promise({
      wf <- evaluate_walk_forward(
        basis = basis_snap,
        a_vector = a_vec_snap,
        train_months = train_m,
        test_months = test_m,
        step_months = test_m,
        methods = c("SAMPLE", "SHRINKAGE", "EWMA")
      )
      boot <- bootstrap_risk_uncertainty(
        sample = sample_snap,
        a_vector = a_vec_snap,
        method = if (estimator_snap %in% c("SAMPLE", "SHRINKAGE", "EWMA")) estimator_snap else "SHRINKAGE",
        n_boot = n_boot,
        block_length = blk_len,
        seed = seed_val
      )
      list(
        walk_forward = wf,
        bootstrap = boot,
        req_snapshot = req_snapshot,
        ctx_snapshot = ctx_snapshot
      )
    }, seed = TRUE)
  })

  # Observe ExtendedTask completion and guard against stale state overwrites
  shiny::observe({
    if (!identical(validation_task$status(), "success")) return()
    res <- tryCatch(validation_task$result(), error = function(e) NULL)
    if (!is.null(res)) {
      req_snap <- res$req_snapshot
      ctx_snap <- res$ctx_snapshot

      # Only apply if base currency and dataset have not changed while job ran
      is_still_current <- identical(ctx_snap$dataset_id, state$dataset$dataset_id) &&
                          identical(ctx_snap$base_currency, state$base_currency)

      if (is_still_current) {
        state$current_validation <- list(
          walk_forward = res$walk_forward,
          bootstrap = res$bootstrap
        )
        env <- build_response_envelope(
          request = req_snap,
          state = state,
          status = "ok",
          result = list(validation = state$current_validation)
        )
        session$sendCustomMessage("app_event", env)
      } else {
        env <- build_response_envelope(
          request = req_snap,
          state = state,
          status = "stale",
          issues = list(make_issue(
            code = "STALE_BACKGROUND_JOB_DISCARDED",
            severity = "INFO",
            message = "Ergebnis eines älteren Validierungs-Jobs wurde verworfen, da Basiswährung/Datensatz gewechselt wurde.",
            scope = "JOBS"
          ))
        )
        session$sendCustomMessage("app_event", env)
      }
    }
  })

  # Main Protocol Command Handler
  shiny::observeEvent(input$app_command, {
    req <- tryCatch(as.list(input$app_command), error = function(e) list())
    tryCatch({
      cmd_type <- as.character(req$type %||% "")

      if (identical(cmd_type, "validation.run")) {
        # Prevent duplicate job queue growth if already running
        if (identical(validation_task$status(), "running")) {
          return()
        }

        payload <- as.list(req$payload %||% list())
        basis_snap <- state$current_basis
        model_snap <- state$current_model
        port_snap <- state$current_portfolio_eval

        if (is.null(basis_snap) || is.null(model_snap) || is.null(port_snap)) {
          env_err <- build_response_envelope(
            request = req,
            state = state,
            status = "error",
            issues = list(make_issue("NO_ACTIVE_PORTFOLIO_FOR_VALIDATION", "ERROR", "Bitte zuerst Portfolio berechnen.", "VALIDATION"))
          )
          session$sendCustomMessage("app_event", env_err)
          return()
        }

        model_cfg <- state$config$risk_models[[state$active_model_id]] %||% state$config$risk_models$shrinkage_long
        sample_snap <- resolve_estimation_sample(basis_snap, settings = model_cfg)
        a_vec_snap <- port_snap$a_vector
        estimator_snap <- model_snap$estimator
        train_m <- as.integer(payload$train_months %||% 120L)
        test_m <- as.integer(payload$test_months %||% 12L)
        n_boot <- as.integer(payload$n_boot %||% 200L)
        blk_len <- as.integer(payload$block_length %||% 6L)
        seed_val <- as.integer(payload$seed %||% 42L)
        ctx_snapshot <- list(
          dataset_id = state$dataset$dataset_id,
          base_currency = state$base_currency,
          state_version = state$state_version
        )

        # Notify UI that job started
        session$sendCustomMessage(
          "app_event",
          build_response_envelope(request = req, state = state, status = "running")
        )

        if (isTRUE(ASYNC_STATUS$async_supported)) {
          validation_task$invoke(
            basis_snap = basis_snap,
            sample_snap = sample_snap,
            a_vec_snap = a_vec_snap,
            estimator_snap = estimator_snap,
            train_m = train_m,
            test_m = test_m,
            n_boot = n_boot,
            blk_len = blk_len,
            seed_val = seed_val,
            req_snapshot = req,
            ctx_snapshot = ctx_snapshot
          )
        } else {
          # Synchronous batch fallback when restricted environment blocks background workers
          wf <- evaluate_walk_forward(
            basis = basis_snap,
            a_vector = a_vec_snap,
            train_months = train_m,
            test_months = test_m,
            step_months = test_m
          )
          boot <- bootstrap_risk_uncertainty(
            sample = sample_snap,
            a_vector = a_vec_snap,
            method = if (estimator_snap %in% c("SAMPLE", "SHRINKAGE", "EWMA")) estimator_snap else "SHRINKAGE",
            n_boot = n_boot,
            block_length = blk_len,
            seed = seed_val
          )
          state$current_validation <- list(walk_forward = wf, bootstrap = boot)
          session$sendCustomMessage(
            "app_event",
            build_response_envelope(
              request = req,
              state = state,
              status = "ok",
              result = list(validation = state$current_validation)
            )
          )
        }
        return()
      }

      resp <- handle_app_command(req, state)
      session$sendCustomMessage("app_event", resp)
    }, error = function(e) {
      message("[Command Handler Error] ", conditionMessage(e))
      err_env <- build_response_envelope(
        request = req,
        state = state,
        status = "error",
        issues = list(make_issue(
          code = "SERVER_COMMAND_ERROR",
          severity = "ERROR",
          message = paste("Interner Serverfehler:", conditionMessage(e)),
          scope = "SERVER"
        ))
      )
      session$sendCustomMessage("app_event", err_env)
    })
  })

  # Interactive Workbook Upload via native Shiny fileInput binding (isolated to private session cache)
  shiny::observeEvent(input$workbook_upload, {
    up <- input$workbook_upload
    if (is.null(up) || is.null(up$datapath) || !file.exists(up$datapath)) return()

    ld <- load_dataset_into_state(state, up$datapath, is_private = TRUE)
    req_up <- list(
      protocol_version = 1L,
      request_id = paste0("upload_", as.integer(Sys.time())),
      type = "dataset.load",
      payload = list(base_currency = state$base_currency)
    )
    if (!isTRUE(ld$valid)) {
      session$sendCustomMessage(
        "app_event",
        build_response_envelope(request = req_up, state = state, status = "error", issues = ld$issues)
      )
      return()
    }

    resp <- handle_app_command(req_up, state)
    session$sendCustomMessage("app_event", resp)
  })

  # Official Shiny downloadHandlers stamped with the active configuration hash
  output$download_xlsx <- shiny::downloadHandler(
    filename = function() {
      h <- state$current_portfolio_eval$config_hash %||% "export"
      sprintf("MultiAsset_Analysis_%s_%s.xlsx", state$base_currency, h)
    },
    content = function(file) {
      export_analysis_bundle(
        dataset = state$dataset,
        model = state$current_model,
        portfolio_eval = state$current_portfolio_eval,
        scenario_eval = state$current_scenario_eval,
        comparison_eval = state$current_comparison_eval,
        format = "XLSX",
        output_path = file,
        state_version = state$state_version
      )
    }
  )

  output$download_json <- shiny::downloadHandler(
    filename = function() {
      h <- state$current_portfolio_eval$config_hash %||% "export"
      sprintf("MultiAsset_Report_%s_%s.json", state$base_currency, h)
    },
    content = function(file) {
      export_analysis_bundle(
        dataset = state$dataset,
        model = state$current_model,
        portfolio_eval = state$current_portfolio_eval,
        scenario_eval = state$current_scenario_eval,
        comparison_eval = state$current_comparison_eval,
        format = "JSON",
        output_path = file,
        state_version = state$state_version
      )
    }
  )

  output$download_csv <- shiny::downloadHandler(
    filename = function() {
      h <- state$current_portfolio_eval$config_hash %||% "export"
      sprintf("MultiAsset_RiskDecomp_%s_%s.csv", state$base_currency, h)
    },
    content = function(file) {
      export_analysis_bundle(
        dataset = state$dataset,
        model = state$current_model,
        portfolio_eval = state$current_portfolio_eval,
        scenario_eval = state$current_scenario_eval,
        comparison_eval = state$current_comparison_eval,
        format = "CSV",
        output_path = file,
        state_version = state$state_version
      )
    }
  )
}

shiny::shinyApp(ui = ui, server = server)
