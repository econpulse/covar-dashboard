# ==============================================================================
# R/server_commands.R
# Command router, JSON-safe envelope builder, and authoritative Shiny adapter
# ==============================================================================

serialize_covariance_model <- function(model, meta = NULL) {
  if (is.null(model) || !isTRUE(model$valid)) return(NULL)
  comp_ids <- model$component_ids
  meta_dt <- if (!is.null(meta)) data.table::as.data.table(meta) else NULL

  comp_labels <- vapply(comp_ids, function(cid) {
    if (grepl("^HEDGE_", cid)) {
      ccy <- sub("^HEDGE_", "", cid)
      sprintf("FX Hedge %s/%s", ccy, model$base_currency)
    } else if (!is.null(meta_dt) && cid %in% meta_dt$Code) {
      meta_dt[Code == cid, Name][1L]
    } else {
      cid
    }
  }, character(1))

  # Serialize row-major list of numeric vectors for unambiguous JS matrix indexing
  mat_to_rows <- function(M) {
    lapply(seq_len(nrow(M)), function(i) as.numeric(M[i, ]))
  }

  sanitize_for_json(list(
    model_id = model$model_id,
    label = model$label,
    model_hash = model$model_hash,
    basis_id = model$basis_id,
    base_currency = model$base_currency,
    return_mode = model$return_mode,
    estimator = model$estimator,
    target = model$target,
    shrinkage_intensity = model$shrinkage_intensity,
    model_supported_share = model$model_supported_share,
    n_obs = model$n_obs,
    n_eff = model$n_eff,
    start_month = model$start_month,
    end_month = model$end_month,
    unhedged_common_start = model$unhedged_common_start,
    hedged_common_start = model$hedged_common_start,
    dimension = length(comp_ids),
    asset_ids = model$asset_ids,
    hedge_ids = model$hedge_ids,
    component_ids = comp_ids,
    component_labels = as.character(comp_labels),
    required_hedge_currencies = model$required_hedge_currencies,
    available_hedge_currencies = model$available_hedge_currencies,
    missing_hedge_currencies = model$missing_hedge_currencies,
    component_availability = model$component_availability,
    vol_monthly = as.numeric(model$vol_monthly[comp_ids]),
    vol_annualized = as.numeric(model$vol_annualized[comp_ids]),
    mean_monthly = as.numeric(model$mean_monthly[comp_ids]),
    sigma_monthly = list(
      component_ids = comp_ids,
      dimension = length(comp_ids),
      rows = mat_to_rows(model$Sigma_monthly)
    ),
    sigma_annualized = list(
      component_ids = comp_ids,
      dimension = length(comp_ids),
      rows = mat_to_rows(model$Sigma_annualized)
    ),
    correlation = list(
      component_ids = comp_ids,
      dimension = length(comp_ids),
      rows = mat_to_rows(model$correlation_matrix)
    ),
    diagnostics = list(
      eigenvalues = model$diagnostics$eigenvalues,
      min_eigenvalue_raw = model$diagnostics$min_eigenvalue_raw,
      min_eigenvalue = model$diagnostics$min_eigenvalue,
      max_eigenvalue = model$diagnostics$max_eigenvalue,
      condition_number = model$diagnostics$condition_number,
      avg_pairwise_correlation = model$diagnostics$avg_pairwise_correlation,
      min_pairwise_correlation = model$diagnostics$min_pairwise_correlation,
      max_pairwise_correlation = model$diagnostics$max_pairwise_correlation,
      is_psd = model$diagnostics$is_psd,
      was_regularized = model$diagnostics$was_regularized,
      spectral_floor_applied = model$diagnostics$spectral_floor_applied
    ),
    factor_audit = model$factor_audit
  ))
}

serialize_portfolio_eval <- function(ev, hist_path = NULL) {
  if (is.null(ev)) return(NULL)
  if (!isTRUE(ev$valid)) {
    return(sanitize_for_json(list(
      valid = FALSE,
      portfolio_id = ev$portfolio_id,
      portfolio_name = ev$portfolio_name,
      base_currency = ev$base_currency,
      model_id = ev$model_id
    )))
  }

  mat_to_rows <- function(M) {
    lapply(seq_len(nrow(M)), function(i) as.numeric(M[i, ]))
  }

  sanitize_for_json(list(
    valid = TRUE,
    portfolio_id = ev$portfolio_id,
    portfolio_name = ev$portfolio_name,
    config_hash = ev$config_hash,
    base_currency = ev$base_currency,
    return_mode = ev$return_mode,
    model_id = ev$model_id,
    model_label = ev$model_label,
    model_hash = ev$model_hash,
    n_obs = ev$n_obs,
    start_month = ev$start_month,
    end_month = ev$end_month,
    metrics = ev$metrics,
    weights = as.list(ev$weights),
    g_vector = as.list(ev$g_vector),
    a_vector = as.list(ev$a_vector),
    exposures = list(
      active_mode = ev$exposures$active_mode,
      allocation_attribution_note = ev$exposures$allocation_attribution_note,
      weight_sum = ev$exposures$weight_sum,
      total_hedge_notional = ev$exposures$total_hedge_notional,
      foreign_exposure_before = ev$exposures$foreign_exposure_before,
      foreign_exposure_after = ev$exposures$foreign_exposure_after,
      hedgeable_foreign_before = ev$exposures$hedgeable_foreign_before,
      hedgeable_foreign_after = ev$exposures$hedgeable_foreign_after,
      emeq_exposure = ev$exposures$emeq_exposure,
      domestic_exposure = ev$exposures$domestic_exposure,
      currency_table = ev$exposures$currency_table,
      asset_table = ev$exposures$asset_table,
      derived_class_table = ev$exposures$derived_class_table,
      derived_currency_ratios = ev$exposures$derived_currency_ratios
    ),
    component_contributions = ev$component_contributions,
    asset_contributions = ev$asset_contributions,
    class_contributions = ev$class_contributions,
    asset_correlation = list(
      asset_ids = rownames(ev$asset_correlation_matrix),
      rows = mat_to_rows(ev$asset_correlation_matrix)
    ),
    historical_path = if (!is.null(hist_path) && isTRUE(hist_path$valid)) {
      list(
        n_months = hist_path$n_months,
        start_month = hist_path$start_month,
        end_month = hist_path$end_month,
        realized_vol_ann = hist_path$realized_vol_ann,
        realized_historical_cagr = hist_path$realized_historical_cagr,
        max_drawdown = hist_path$max_drawdown,
        series = hist_path$path
      )
    } else NULL
  ))
}

build_response_envelope <- function(request,
                                    state,
                                    status = c("ok", "error", "running", "stale"),
                                    result = NULL,
                                    issues = list()) {
  status <- match.arg(status)
  sp <- split_issues(issues)

  sanitize_for_json(list(
    protocol_version = 1L,
    request_id = request$request_id %||% paste0("srv_", as.integer(Sys.time())),
    type = request$type %||% "unknown",
    status = status,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    context = list(
      dataset_id = if (!is.null(state$dataset)) state$dataset$dataset_id else NULL,
      basis_id = if (!is.null(state$current_basis)) state$current_basis$basis_id else NULL,
      model_id = state$active_model_id,
      model_hash = if (!is.null(state$current_model)) state$current_model$model_hash else NULL,
      base_currency = state$base_currency,
      return_mode = state$return_mode,
      state_version = state$state_version
    ),
    result = result,
    errors = sp$errors,
    warnings = sp$warnings,
    info = sp$info
  ))
}

normalize_incoming_weights <- function(raw_weights, default_weights, asset_ids) {
  w_out <- stats::setNames(numeric(length(asset_ids)), asset_ids)
  src <- if (!is.null(raw_weights) && length(raw_weights) > 0L) as.list(raw_weights) else as.list(default_weights %||% list())
  if (length(src) > 0L) {
    for (aid in intersect(names(src), asset_ids)) {
      val <- suppressWarnings(as.numeric(src[[aid]]))
      if (length(val) == 1L && !is.na(val)) {
        w_out[aid] <- val
      }
    }
  }
  w_out
}

normalize_incoming_hedge_config <- function(raw_hc, state) {
  if (is.null(raw_hc) || length(raw_hc) == 0L) {
    mode <- state$hedge_mode %||% "ASSET_CLASS"
    return(state$hedge_configs[[mode]])
  }
  raw_hc <- as.list(raw_hc)
  mode <- toupper(raw_hc$mode %||% state$hedge_mode %||% "ASSET_CLASS")
  configs_container <- if (!is.null(raw_hc$configs)) as.list(raw_hc$configs) else NULL

  if (identical(mode, "ASSET_CLASS")) {
    out <- list(
      mode = "ASSET_CLASS",
      class_ratios = as.list(raw_hc$class_ratios %||% state$hedge_configs$ASSET_CLASS$class_ratios %||% list()),
      asset_overrides = as.list(raw_hc$asset_overrides %||% list())
    )
    if (!is.null(raw_hc$currency_ratios)) {
      out$currency_ratios <- as.list(raw_hc$currency_ratios)
    }
    if (!is.null(configs_container)) {
      out$configs <- configs_container
    }
    out
  } else {
    out <- list(
      mode = "CURRENCY_OVERLAY",
      currency_ratios = as.list(raw_hc$currency_ratios %||% state$hedge_configs$CURRENCY_OVERLAY$currency_ratios %||% list())
    )
    if (!is.null(raw_hc$class_ratios)) {
      out$class_ratios <- as.list(raw_hc$class_ratios)
    }
    if (!is.null(raw_hc$asset_overrides)) {
      out$asset_overrides <- as.list(raw_hc$asset_overrides)
    }
    if (!is.null(configs_container)) {
      out$configs <- configs_container
    }
    out
  }
}

run_full_evaluation_pipeline <- function(state,
                                         base_ccy = state$base_currency,
                                         return_mode = state$return_mode,
                                         model_id = state$active_model_id,
                                         portfolio_id = state$active_portfolio_id,
                                         weights = NULL,
                                         hedge_config = NULL,
                                         scenario_id = state$active_scenario_id,
                                         rebalancing_rule = state$rebalancing_rule,
                                         cost_bps_pa = state$cost_bps_pa,
                                         carry_overrides = NULL,
                                         custom_risk_settings = NULL,
                                         include_all_models_comparison = TRUE) {
  if (!identical(base_ccy, state$base_currency)) {
    state$carry_overrides <- list()
  }
  state$base_currency <- base_ccy
  if (!is.null(state$universe) && !is.null(state$dataset$meta)) {
    state$universe <- order_asset_universe(state$universe, state$dataset$meta, base_ccy = base_ccy)
  }
  state$return_mode <- return_mode
  state$active_model_id <- model_id
  state$active_portfolio_id <- portfolio_id
  state$active_scenario_id <- scenario_id
  state$rebalancing_rule <- rebalancing_rule
  state$cost_bps_pa <- as.numeric(cost_bps_pa %||% 0.0)
  if (!is.null(carry_overrides)) {
    state$carry_overrides <- as.list(carry_overrides)
  }

  cov_bundle <- get_or_estimate_covariance(
    state = state,
    model_id = model_id,
    base_ccy = base_ccy,
    return_mode = return_mode,
    universe = state$universe,
    custom_settings = custom_risk_settings
  )

  basis <- cov_bundle$basis
  model <- cov_bundle$model
  state$current_basis <- basis
  state$current_model <- model

  if (!isTRUE(model$valid)) {
    return(list(valid = FALSE, issues = model$issues))
  }

  asset_ids <- model$asset_ids
  tpl_weights <- state$portfolios[[portfolio_id]]$weights %||% state$portfolios$saa_50$weights
  w_vec <- normalize_incoming_weights(weights, tpl_weights, asset_ids)
  if (!is.null(weights) && portfolio_id %in% names(state$portfolios)) {
    state$portfolios[[portfolio_id]]$weights <- as.list(w_vec)
  }

  target_hc <- if (!is.null(hedge_config)) {
    hedge_config
  } else if (!is.null(state$portfolios[[portfolio_id]]$hedge_config)) {
    state$portfolios[[portfolio_id]]$hedge_config
  } else {
    state$hedge_configs[[state$hedge_mode]]
  }

  hc <- normalize_incoming_hedge_config(target_hc, state)
  state$hedge_mode <- hc$mode
  state$hedge_configs[[hc$mode]] <- hc
  if (identical(hc$mode, "ASSET_CLASS")) {
    state$saved_asset_class_config <- hc
  }
  if (!is.null(hc$configs)) {
    cfg_list <- as.list(hc$configs)
    if (!is.null(cfg_list$ASSET_CLASS)) {
      state$hedge_configs$ASSET_CLASS <- as.list(cfg_list$ASSET_CLASS)
      state$saved_asset_class_config <- as.list(cfg_list$ASSET_CLASS)
    }
    if (!is.null(cfg_list$CURRENCY_OVERLAY)) {
      state$hedge_configs$CURRENCY_OVERLAY <- as.list(cfg_list$CURRENCY_OVERLAY)
    }
  }

  port_name <- state$portfolios[[portfolio_id]]$name %||% portfolio_id
  cache <- get_active_cache(state)

  port_res <- cache$get_or_compute(
    tier = "portfolio",
    key_parts = list(
      model_hash = model$model_hash,
      portfolio_id = portfolio_id,
      weights = round(w_vec, 10),
      hedge_config = hc,
      cost_bps_pa = state$cost_bps_pa
    ),
    counter_name = "portfolio_evaluations",
    compute_fn = function() {
      evaluate_portfolio(
        weights = w_vec,
        hedge_config = hc,
        model = model,
        meta = state$dataset$meta,
        cost_bps_pa = state$cost_bps_pa,
        portfolio_id = portfolio_id,
        portfolio_name = port_name
      )
    }
  )

  port_eval <- port_res$value
  state$current_portfolio_eval <- port_eval

  if (!isTRUE(port_eval$valid)) {
    return(list(
      valid = FALSE,
      model = model,
      portfolio_eval = port_eval,
      issues = port_eval$issues
    ))
  }

  hist_path <- build_historical_portfolio_path(
    basis = basis,
    weights = port_eval$weights,
    g_vector = port_eval$g_vector,
    cost_bps_pa = state$cost_bps_pa,
    start_month = model$start_month,
    end_month = model$end_month
  )
  state$current_historical_path <- hist_path

  # Evaluate 5-year scenario path
  scen_res <- cache$get_or_compute(
    tier = "scenario",
    key_parts = list(
      expectations_id = state$expectations$expectations_id,
      scenario_id = scenario_id,
      base_currency = base_ccy,
      weights = round(w_vec, 10),
      hedge_config = hc,
      rebalancing_rule = rebalancing_rule,
      cost_bps_pa = state$cost_bps_pa,
      carry_overrides = state$carry_overrides
    ),
    counter_name = "scenario_evaluations",
    compute_fn = function() {
      evaluate_return_scenarios(
        expectations = state$expectations,
        weights = w_vec,
        hedge_config = hc,
        meta = state$dataset$meta,
        base_ccy = base_ccy,
        scenario_id = scenario_id,
        rebalancing_rule = rebalancing_rule,
        cost_bps_pa = state$cost_bps_pa,
        carry_overrides = state$carry_overrides,
        available_hedge_currencies = model$supported_hedge_currencies %||% model$available_hedge_currencies
      )
    }
  )
  scen_eval <- scen_res$value
  state$current_scenario_eval <- scen_eval

  # Multi-SAA & multi-model comparison: include both configured/hedged and unhedged (0%) variant for every portfolio
  unhedged_ratios <- list(CHF = 0, EUR = 0, USD = 0, GBP = 0, JPY = 0)
  port_ids <- names(state$portfolios)
  if (!portfolio_id %in% port_ids) {
    port_ids <- c(portfolio_id, port_ids)
  }

  saa_configs <- list()
  for (pid in port_ids) {
    p_obj <- state$portfolios[[pid]]
    p_name <- if (identical(pid, portfolio_id)) {
      port_name
    } else {
      p_obj$name %||% pid
    }
    w_p <- if (identical(pid, portfolio_id)) {
      w_vec
    } else {
      normalize_incoming_weights(p_obj$weights, p_obj$weights, asset_ids)
    }
    hc_p <- if (identical(pid, portfolio_id)) {
      hc
    } else if (!is.null(p_obj$hedge_config)) {
      normalize_incoming_hedge_config(p_obj$hedge_config, state)
    } else {
      hc
    }

    # 1. Configured / Hedged variant
    saa_configs[[length(saa_configs) + 1L]] <- list(
      id = pid,
      name = p_name,
      weights = w_p,
      hedge_config = hc_p,
      cost_bps_pa = state$cost_bps_pa
    )

    # 2. Unhedged variant (0% FX hedge)
    saa_configs[[length(saa_configs) + 1L]] <- list(
      id = paste0(pid, "_unhedged"),
      name = paste0(p_name, " (Ungesichert 0%)"),
      weights = w_p,
      hedge_config = list(mode = "CURRENCY_OVERLAY", currency_ratios = unhedged_ratios),
      cost_bps_pa = 0.0
    )
  }

  extra_models <- NULL
  if (isTRUE(include_all_models_comparison)) {
    all_m_ids <- names(state$config$risk_models)
    extra_models <- lapply(all_m_ids, function(mid) {
      get_or_estimate_covariance(state, model_id = mid, base_ccy = base_ccy, return_mode = return_mode, universe = state$universe)$model
    })
  }

  comp_eval <- compare_portfolios(
    configs = saa_configs,
    model = model,
    meta = state$dataset$meta,
    benchmark_index = which(vapply(saa_configs, function(x) identical(x$id, portfolio_id), logical(1)))[1L] %||% 1L,
    extra_models = extra_models,
    cache = cache,
    scenario_eval = scen_eval
  )
  state$current_comparison_eval <- comp_eval
  state$state_version <- state$state_version + 1L

  list(
    valid = TRUE,
    basis = basis,
    model = model,
    portfolio_eval = port_eval,
    historical_path = hist_path,
    scenario_eval = scen_eval,
    comparison_eval = comp_eval,
    cache_stats = cache$get_stats(),
    issues = unique(c( state$dataset$issues, model$issues, port_eval$issues, scen_eval$issues ))
  )
}

build_full_client_payload <- function(state, pipeline_res, include_static_context = TRUE) {
  reg <- state$dataset
  basis <- pipeline_res$basis
  model <- pipeline_res$model
  port_eval <- pipeline_res$portfolio_eval
  scen_eval <- pipeline_res$scenario_eval
  comp_eval <- pipeline_res$comparison_eval
  cache <- get_active_cache(state)

  out <- list(
    config = list(
      supported_base_currencies = state$config$supported_base_currencies,
      risk_models = state$config$risk_models,
      portfolios = state$portfolios,
      active_portfolio_id = state$active_portfolio_id,
      base_currency = state$base_currency,
      return_mode = state$return_mode,
      active_model_id = state$active_model_id,
      hedge_mode = state$hedge_mode,
      hedge_configs = state$hedge_configs,
      cost_bps_pa = state$cost_bps_pa,
      active_scenario_id = state$active_scenario_id,
      rebalancing_rule = state$rebalancing_rule
    ),
    portfolio_evaluation = serialize_portfolio_eval(port_eval, pipeline_res$historical_path),
    scenario_evaluation = if (!is.null(scen_eval) && isTRUE(scen_eval$valid)) {
      list(
        valid = TRUE,
        expectations_id = scen_eval$expectations_id,
        as_of = scen_eval$as_of,
        scenario_id = scen_eval$scenario_id,
        scenario_label = scen_eval$scenario_label,
        scenario_description = scen_eval$scenario_description,
        base_currency = scen_eval$base_currency,
        horizon = scen_eval$horizon,
        rebalancing_rule = scen_eval$rebalancing_rule,
        fee_basis = scen_eval$fee_basis,
        uncertainty_description = scen_eval$uncertainty_description,
        metrics = scen_eval$metrics,
        yearly_path = scen_eval$yearly_path,
        asset_table = scen_eval$asset_table,
        carry_overrides = state$carry_overrides,
        relevant_hedge_currencies = scen_eval$relevant_hedge_currencies,
        available_hedge_currencies = scen_eval$available_hedge_currencies,
        hedge_currency_summary = scen_eval$hedge_currency_summary
      )
    } else NULL,
    comparison = if (!is.null(comp_eval) && isTRUE(comp_eval$valid)) {
      list(
        benchmark_id = comp_eval$benchmark_id,
        summary_table = comp_eval$summary_table,
        delta_exposures = comp_eval$delta_exposures,
        pairwise_tracking_error = list(
          portfolio_ids = rownames(comp_eval$pairwise_tracking_error),
          matrix = lapply(seq_len(nrow(comp_eval$pairwise_tracking_error)), function(i) as.numeric(comp_eval$pairwise_tracking_error[i, ])),
          rows = lapply(seq_len(nrow(comp_eval$pairwise_tracking_error)), function(i) as.numeric(comp_eval$pairwise_tracking_error[i, ]))
        ),
        cross_model_comparison = comp_eval$cross_model_comparison
      )
    } else NULL,
    custom_cma_sets = state$custom_cma_sets,
    cache_stats = cache$get_stats()
  )

  if (isTRUE(include_static_context)) {
    out$dataset <- list(
      dataset_id = reg$dataset_id,
      file_hash = substr(reg$file_hash, 1L, 16L),
      data_hash = substr(reg$data_hash, 1L, 16L),
      source_path = basename(reg$source_path %||% "Data.xlsx"),
      is_private_upload = state$is_private_upload,
      active_assets = reg$active_assets,
      all_assets = reg$all_assets,
      metadata = reg$meta,
      coverage = reg$coverage,
      fx_availability = basis$component_availability,
      fx_audit_summary = basis$fx_audit_summary
    )
    out$config$expectations <- list(
      expectations_id = state$expectations$expectations_id,
      as_of = state$expectations$as_of,
      horizon = state$expectations$horizon,
      fee_basis = state$expectations$fee_basis,
      return_definition = state$expectations$return_definition,
      uncertainty_description = state$expectations$uncertainty_description,
      scenarios = lapply(state$expectations$scenarios, function(s) {
        list(
          scenario_id = s$scenario_id,
          label = s$label,
          description = s$description,
          usd_numeraire_paths = s$usd_numeraire_paths,
          short_rates_pa = s$short_rates_pa,
          asset_assumptions = s$asset_assumptions
        )
      })
    )
    out$covariance_model <- serialize_covariance_model(model, reg$meta)
  }

  sanitize_for_json(out)
}

handle_app_command <- function(request, state) {
  if (is.null(request) || (!is.list(request) && !is.vector(request))) {
    return(build_response_envelope(
      request = list(type = "invalid", request_id = "invalid"),
      state = state,
      status = "error",
      issues = list(make_issue("INVALID_REQUEST_FORMAT", "ERROR", "Ungültiges Command-Objekt.", "PROTOCOL"))
    ))
  }
  request <- as.list(request)

  cmd_type <- as.character(request$type %||% "")
  req_id <- as.character(request$request_id %||% paste0("req_", as.integer(Sys.time())))
  payload <- as.list(request$payload %||% list())
  ctx <- as.list(request$context %||% list())

  state$latest_request_by_type[[cmd_type]] <- req_id

  if (cmd_type %in% c("app.bootstrap", "dataset.load", "risk.estimate")) {
    base_ccy <- toupper(payload$base_currency %||% ctx$base_currency %||% state$base_currency)
    ret_mode <- toupper(payload$return_mode %||% ctx$return_mode %||% state$return_mode)
    mod_id <- as.character(payload$model_id %||% ctx$model_id %||% state$active_model_id)
    port_id <- as.character(payload$portfolio_id %||% state$active_portfolio_id)
    scen_id <- as.character(payload$scenario_id %||% state$active_scenario_id)
    rebal <- as.character(payload$rebalancing_rule %||% state$rebalancing_rule)
    cost_bps <- as.numeric(payload$cost_bps_pa %||% state$cost_bps_pa)

    # Optional active universe update from Data & Methodology view
    if (!is.null(payload$universe) && length(payload$universe) > 0L) {
      state$universe <- intersect(as.character(payload$universe), state$dataset$all_assets)
    }
    # Optional client-side custom portfolios sync (e.g. from localStorage)
    if (!is.null(payload$custom_portfolios) && is.list(payload$custom_portfolios)) {
      for (cp in payload$custom_portfolios) {
        if (!is.null(cp$id) && nzchar(cp$id) && !(cp$id %in% c("saa_25", "saa_50", "saa_75"))) {
          state$portfolios[[cp$id]] <- list(
            id = cp$id,
            name = cp$name %||% cp$id,
            weights = as.list(cp$weights %||% list()),
            is_custom = TRUE
          )
        }
      }
    }
    # Optional carry overrides
    if (!is.null(payload$carry_overrides)) {
      state$carry_overrides <- as.list(payload$carry_overrides)
    }
    # Optional EUR pre-1999 cash proxy policy toggle
    if (!is.null(payload$allow_eur_cash_proxy)) {
      state$config$cash_proxy_config$EUR$allow_pre_1999_proxy <- isTRUE(payload$allow_eur_cash_proxy)
      state$cash <- build_cash_returns(state$dataset, state$config)
      cash_tr <- build_cash_tr_series(state$cash, state$dataset, state$config)
      if (isTRUE(cash_tr$valid) && nrow(cash_tr$data) > 0L) {
        state$dataset <- attach_cash_tr_to_registry(state$dataset, cash_tr)
      }
    }

    res <- run_full_evaluation_pipeline(
      state = state,
      base_ccy = base_ccy,
      return_mode = ret_mode,
      model_id = mod_id,
      portfolio_id = port_id,
      weights = payload$weights,
      hedge_config = payload$hedge_config,
      scenario_id = scen_id,
      rebalancing_rule = rebal,
      cost_bps_pa = cost_bps,
      custom_risk_settings = payload$risk_settings,
      include_all_models_comparison = TRUE
    )

    if (!isTRUE(res$valid)) {
      return(build_response_envelope(
        request = request,
        state = state,
        status = "error",
        result = list(
          portfolio_evaluation = serialize_portfolio_eval(res$portfolio_eval),
          cache_stats = get_active_cache(state)$get_stats()
        ),
        issues = res$issues
      ))
    }

    full_payload <- build_full_client_payload(state, res)
    return(build_response_envelope(
      request = request,
      state = state,
      status = "ok",
      result = full_payload,
      issues = res$issues
    ))
  }

  if (identical(cmd_type, "portfolio.evaluate")) {
    # Crucial architectural requirement (Section 11 & 14):
    # Weight or hedge changes NEVER re-estimate the covariance matrix if the current model matches context!
    port_id <- as.character(payload$portfolio_id %||% state$active_portfolio_id)
    cost_bps <- as.numeric(payload$cost_bps_pa %||% state$cost_bps_pa)
    scen_id <- as.character(payload$scenario_id %||% state$active_scenario_id)
    rebal <- as.character(payload$rebalancing_rule %||% state$rebalancing_rule)

    res <- run_full_evaluation_pipeline(
      state = state,
      base_ccy = state$base_currency,
      return_mode = state$return_mode,
      model_id = state$active_model_id,
      portfolio_id = port_id,
      weights = payload$weights,
      hedge_config = payload$hedge_config,
      scenario_id = scen_id,
      rebalancing_rule = rebal,
      cost_bps_pa = cost_bps,
      include_all_models_comparison = TRUE
    )

    if (!isTRUE(res$valid)) {
      return(build_response_envelope(
        request = request,
        state = state,
        status = "error",
        result = list(
          portfolio_evaluation = serialize_portfolio_eval(res$portfolio_eval),
          cache_stats = get_active_cache(state)$get_stats()
        ),
        issues = res$issues
      ))
    }

    return(build_response_envelope(
      request = request,
      state = state,
      status = "ok",
      result = build_full_client_payload(state, res, include_static_context = FALSE),
      issues = res$issues
    ))
  }

  if (identical(cmd_type, "portfolio.save")) {
    port_id <- as.character(payload$portfolio_id %||% state$active_portfolio_id)
    port_name <- as.character(payload$name %||% "")
    raw_w <- payload$weights
    asset_ids <- state$current_model$asset_ids %||% state$dataset$active_assets
    tpl_w <- state$portfolios[[port_id]]$weights %||% state$portfolios[[state$active_portfolio_id]]$weights
    w_vec <- normalize_incoming_weights(raw_w, tpl_w, asset_ids)

    if (!nzchar(port_name)) {
      port_name <- state$portfolios[[port_id]]$name %||% port_id
    }

    raw_hc <- payload$hedge_config %||% state$hedge_configs[[state$hedge_mode]]
    hc_norm <- normalize_incoming_hedge_config(raw_hc, state)

    # Package comprehensive hedge configuration preserving active mode, ratios, and both mode configs
    p_hc <- list(
      mode = hc_norm$mode,
      class_ratios = hc_norm$class_ratios %||% state$hedge_configs$ASSET_CLASS$class_ratios %||% list(),
      asset_overrides = hc_norm$asset_overrides %||% state$hedge_configs$ASSET_CLASS$asset_overrides %||% list(),
      currency_ratios = hc_norm$currency_ratios %||% state$hedge_configs$CURRENCY_OVERLAY$currency_ratios %||% list(),
      configs = if (!is.null(raw_hc$configs)) as.list(raw_hc$configs) else list(
        ASSET_CLASS = state$hedge_configs$ASSET_CLASS,
        CURRENCY_OVERLAY = state$hedge_configs$CURRENCY_OVERLAY
      )
    )

    state$portfolios[[port_id]] <- list(
      id = port_id,
      name = port_name,
      weights = as.list(w_vec),
      hedge_config = p_hc,
      is_custom = !(port_id %in% c("saa_25", "saa_50", "saa_75"))
    )
    state$active_portfolio_id <- port_id

    res <- run_full_evaluation_pipeline(
      state = state,
      base_ccy = state$base_currency,
      return_mode = state$return_mode,
      model_id = state$active_model_id,
      portfolio_id = port_id,
      weights = as.list(w_vec),
      hedge_config = p_hc,
      scenario_id = state$active_scenario_id,
      rebalancing_rule = state$rebalancing_rule,
      cost_bps_pa = state$cost_bps_pa,
      include_all_models_comparison = TRUE
    )

    return(build_response_envelope(
      request = request,
      state = state,
      status = if (isTRUE(res$valid)) "ok" else "error",
      result = build_full_client_payload(state, res),
      issues = res$issues
    ))
  }

  if (identical(cmd_type, "portfolio.delete")) {
    port_id <- as.character(payload$portfolio_id)
    if (port_id %in% c("saa_25", "saa_50", "saa_75")) {
      return(build_response_envelope(
        request = request,
        state = state,
        status = "error",
        result = NULL,
        issues = list(make_issue("BUILTIN_PORTFOLIO_CANNOT_BE_DELETED", "ERROR", "Standard-Vorlagen (25%, 50%, 75%) können nicht gelöscht werden."))
      ))
    }

    state$portfolios[[port_id]] <- NULL
    if (identical(state$active_portfolio_id, port_id)) {
      state$active_portfolio_id <- "saa_50"
    }

    res <- run_full_evaluation_pipeline(
      state = state,
      base_ccy = state$base_currency,
      return_mode = state$return_mode,
      model_id = state$active_model_id,
      portfolio_id = state$active_portfolio_id,
      weights = state$portfolios[[state$active_portfolio_id]]$weights,
      hedge_config = state$hedge_configs[[state$hedge_mode]],
      include_all_models_comparison = TRUE
    )

    return(build_response_envelope(
      request = request,
      state = state,
      status = if (isTRUE(res$valid)) "ok" else "error",
      result = build_full_client_payload(state, res),
      issues = res$issues
    ))
  }

  if (identical(cmd_type, "universe.toggle_asset")) {
    aid <- as.character(payload$asset_id)
    reg <- state$dataset
    new_act <- TRUE
    if (!is.null(reg) && !is.null(reg$meta)) {
      idx <- which(reg$meta$Code == aid)
      if (length(idx) > 0L) {
        curr_act <- isTRUE(reg$meta$Active[idx[1L]])
        new_act <- !curr_act
        reg$meta[idx, Active := new_act]
      }
    }

    new_univ <- state$dataset$meta[Active == TRUE & Series_Type == "ASSET_TR", Code]
    state$universe <- order_asset_universe(new_univ, state$dataset$meta, base_ccy = state$base_currency)
    state$dataset$active_assets <- state$universe

    cur_weights <- state$portfolios[[state$active_portfolio_id]]$weights
    if (!is.null(cur_weights) && aid %in% names(cur_weights) && !new_act) {
      cur_weights[[aid]] <- 0.0
      state$portfolios[[state$active_portfolio_id]]$weights <- cur_weights
    }

    res <- run_full_evaluation_pipeline(
      state = state,
      base_ccy = state$base_currency,
      return_mode = state$return_mode,
      model_id = state$active_model_id,
      portfolio_id = state$active_portfolio_id,
      weights = state$portfolios[[state$active_portfolio_id]]$weights,
      hedge_config = state$hedge_configs[[state$hedge_mode]],
      include_all_models_comparison = TRUE
    )

    return(build_response_envelope(
      request = request,
      state = state,
      status = if (isTRUE(res$valid)) "ok" else "error",
      result = build_full_client_payload(state, res),
      issues = res$issues
    ))
  }

  if (identical(cmd_type, "hedge.convert_mode")) {
    target_mode <- toupper(payload$target_mode %||% "CURRENCY_OVERLAY")
    fallback_pol <- toupper(payload$fallback_policy %||% "RESTORE_SAVED")
    asset_ids <- state$current_model$asset_ids %||% state$dataset$active_assets
    w_vec <- normalize_incoming_weights(
      payload$weights,
      state$portfolios[[state$active_portfolio_id]]$weights,
      asset_ids
    )
    exp_pre <- derive_prehedge_exposures(
      weights = w_vec,
      meta = state$dataset$meta,
      base_ccy = state$base_currency,
      available_hedge_currencies = state$current_model$supported_hedge_currencies %||% state$current_model$available_hedge_currencies,
      fx_carry = state$current_model$latest_carry
    )
    curr_hc <- normalize_incoming_hedge_config(payload$hedge_config, state)
    converted <- convert_hedge_mode(
      exposures = exp_pre,
      current_config = curr_hc,
      target_mode = target_mode,
      saved_asset_class_config = state$saved_asset_class_config,
      fallback_policy = fallback_pol
    )

    new_hc <- list(
      mode = converted$mode,
      class_ratios = converted$class_ratios,
      asset_overrides = converted$asset_overrides,
      currency_ratios = converted$currency_ratios
    )
    res <- run_full_evaluation_pipeline(
      state = state,
      weights = w_vec,
      hedge_config = new_hc
    )
    out_payload <- build_full_client_payload(state, res)
    out_payload$mode_transition <- converted$transition_type

    return(build_response_envelope(
      request = request,
      state = state,
      status = if (isTRUE(res$valid)) "ok" else "error",
      result = out_payload,
      issues = res$issues
    ))
  }

  if (identical(cmd_type, "scenarios.evaluate")) {
    if (!is.null(payload$custom_expectations)) {
      val_exp <- validate_expectations(payload$custom_expectations, meta = state$dataset$meta)
      if (!isTRUE(val_exp$valid)) {
        return(build_response_envelope(
          request = request,
          state = state,
          status = "error",
          issues = val_exp$issues
        ))
      }
      state$expectations <- val_exp
    }
    if (!is.null(payload$asset_return_overrides)) {
      sc_id <- as.character(payload$scenario_id %||% state$active_scenario_id)
      ovr <- as.list(payload$asset_return_overrides)
      horizon <- state$expectations$horizon %||% 5L
      if (is.null(state$expectations$scenarios[[sc_id]]$asset_assumptions)) {
        state$expectations$scenarios[[sc_id]]$asset_assumptions <- list()
      }
      for (acode in names(ovr)) {
        val_num <- as.numeric(ovr[[acode]])
        if (length(val_num) == 1L && !is.na(val_num)) {
          r_vec <- rep(val_num, horizon)
        } else if (length(val_num) == horizon) {
          r_vec <- val_num
        } else {
          r_vec <- rep(0.0, horizon)
        }
        if (is.null(state$expectations$scenarios[[sc_id]]$asset_assumptions[[acode]])) {
          state$expectations$scenarios[[sc_id]]$asset_assumptions[[acode]] <- list(
            asset_id = acode,
            annual_local_returns = r_vec
          )
        } else {
          state$expectations$scenarios[[sc_id]]$asset_assumptions[[acode]]$annual_local_returns <- r_vec
        }
      }
      state$expectations$expectations_id <- paste0("custom_", substr(compute_object_hash(state$expectations$scenarios), 1L, 8L))
    }

    if (!is.null(payload$carry_overrides)) {
      state$carry_overrides <- as.list(payload$carry_overrides)
    }

    scen_id <- as.character(payload$scenario_id %||% state$active_scenario_id)
    rebal <- as.character(payload$rebalancing_rule %||% state$rebalancing_rule)
    res <- run_full_evaluation_pipeline(
      state = state,
      scenario_id = scen_id,
      rebalancing_rule = rebal
    )
    return(build_response_envelope(
      request = request,
      state = state,
      status = if (isTRUE(res$valid)) "ok" else "error",
      result = build_full_client_payload(state, res),
      issues = res$issues
    ))
  }

  if (identical(cmd_type, "cma.save") || identical(cmd_type, "cma_set.save")) {
    set_id <- as.character(payload$set_id %||% paste0("cma_custom_", format(Sys.time(), "%Y%m%d%H%M%S")))
    set_name <- as.character(payload$name %||% set_id)
    sc_id <- as.character(payload$scenario_id %||% state$active_scenario_id)

    # Store custom CMA set in session state
    state$custom_cma_sets[[set_id]] <- list(
      id = set_id,
      name = set_name,
      scenario_id = sc_id,
      asset_returns = payload$asset_returns,
      carry_overrides = payload$carry_overrides
    )

    # Apply asset return overrides if provided
    if (!is.null(payload$asset_returns)) {
      ovr <- as.list(payload$asset_returns)
      horizon <- state$expectations$horizon %||% 5L
      if (is.null(state$expectations$scenarios[[sc_id]]$asset_assumptions)) {
        state$expectations$scenarios[[sc_id]]$asset_assumptions <- list()
      }
      for (acode in names(ovr)) {
        val_num <- as.numeric(ovr[[acode]])
        if (length(val_num) == 1L && !is.na(val_num)) {
          r_vec <- rep(val_num, horizon)
        } else if (length(val_num) == horizon) {
          r_vec <- val_num
        } else {
          r_vec <- rep(0.0, horizon)
        }
        if (is.null(state$expectations$scenarios[[sc_id]]$asset_assumptions[[acode]])) {
          state$expectations$scenarios[[sc_id]]$asset_assumptions[[acode]] <- list(
            asset_id = acode,
            annual_local_returns = r_vec
          )
        } else {
          state$expectations$scenarios[[sc_id]]$asset_assumptions[[acode]]$annual_local_returns <- r_vec
        }
      }
      state$expectations$expectations_id <- paste0("custom_", substr(compute_object_hash(state$expectations$scenarios), 1L, 8L))
    }

    if (!is.null(payload$carry_overrides)) {
      state$carry_overrides <- as.list(payload$carry_overrides)
    }

    rebal <- as.character(payload$rebalancing_rule %||% state$rebalancing_rule)
    res <- run_full_evaluation_pipeline(
      state = state,
      scenario_id = sc_id,
      rebalancing_rule = rebal
    )
    return(build_response_envelope(
      request = request,
      state = state,
      status = if (isTRUE(res$valid)) "ok" else "error",
      result = build_full_client_payload(state, res),
      issues = res$issues
    ))
  }

  if (identical(cmd_type, "cma.delete") || identical(cmd_type, "cma_set.delete")) {
    set_id <- as.character(payload$set_id)
    if (set_id %in% c("cma_base", "cma_stagflation", "cma_disinflation")) {
      return(build_response_envelope(
        request = request,
        state = state,
        status = "error",
        result = NULL,
        issues = list(make_issue("BUILTIN_CMA_SET_CANNOT_BE_DELETED", "ERROR", "Standard-CMA-Sets können nicht gelöscht werden."))
      ))
    }
    state$custom_cma_sets[[set_id]] <- NULL
    res <- run_full_evaluation_pipeline(state = state)
    return(build_response_envelope(
      request = request,
      state = state,
      status = if (isTRUE(res$valid)) "ok" else "error",
      result = build_full_client_payload(state, res),
      issues = res$issues
    ))
  }

  if (identical(cmd_type, "dataset.validate")) {
    reg <- state$dataset
    return(build_response_envelope(
      request = request,
      state = state,
      status = if (isTRUE(reg$valid)) "ok" else "error",
      result = list(
        dataset_id = reg$dataset_id,
        file_hash = reg$file_hash,
        data_hash = reg$data_hash,
        coverage = reg$coverage,
        metadata = reg$meta
      ),
      issues = reg$issues
    ))
  }

  if (identical(cmd_type, "config.import")) {
    cfg_in <- payload$config_data
    if (is.null(cfg_in) || !is.list(cfg_in)) {
      return(build_response_envelope(
        request = request,
        state = state,
        status = "error",
        issues = list(make_issue("INVALID_CONFIG_JSON", "ERROR", "Ungültige JSON-Konfigurationsdatei.", "CONFIG"))
      ))
    }
    base_ccy <- toupper(cfg_in$base_currency %||% state$base_currency)
    ret_mode <- toupper(cfg_in$return_mode %||% state$return_mode)
    mod_id <- as.character(cfg_in$active_model_id %||% state$active_model_id)
    port_id <- as.character(cfg_in$active_portfolio_id %||% state$active_portfolio_id)
    scen_id <- as.character(cfg_in$active_scenario_id %||% state$active_scenario_id)
    rebal <- as.character(cfg_in$rebalancing_rule %||% state$rebalancing_rule)
    cost_bps <- as.numeric(cfg_in$cost_bps_pa %||% state$cost_bps_pa)

    if (!is.null(cfg_in$portfolios) && is.list(cfg_in$portfolios)) {
      state$portfolios <- cfg_in$portfolios
    }
    res <- run_full_evaluation_pipeline(
      state = state,
      base_ccy = base_ccy,
      return_mode = ret_mode,
      model_id = mod_id,
      portfolio_id = port_id,
      weights = cfg_in$weights,
      hedge_config = cfg_in$hedge_config,
      scenario_id = scen_id,
      rebalancing_rule = rebal,
      cost_bps_pa = cost_bps
    )
    return(build_response_envelope(
      request = request,
      state = state,
      status = if (isTRUE(res$valid)) "ok" else "error",
      result = build_full_client_payload(state, res),
      issues = res$issues
    ))
  }

  build_response_envelope(
    request = request,
    state = state,
    status = "error",
    issues = list(make_issue(
      code = "UNKNOWN_COMMAND_TYPE",
      severity = "ERROR",
      message = sprintf("Unbekannter Befehl: '%s'", cmd_type),
      scope = "PROTOCOL"
    ))
  )
}
