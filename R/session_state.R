# ==============================================================================
# R/session_state.R
# Per-session authoritative state container, cache orchestration, and
# serialization helpers for custom HTML/JS frontend
# ==============================================================================

create_session_state <- function(config, shared_cache = NULL) {
  session_cache <- create_cache_manager(max_size_mb = 128)
  state <- new.env(parent = emptyenv())

  state$state_version <- 1L
  state$config <- config
  state$shared_cache <- shared_cache
  state$session_cache <- session_cache
  state$is_private_upload <- FALSE

  state$dataset <- NULL
  state$spots <- NULL
  state$forwards <- NULL
  state$cash <- NULL

  state$base_currency <- config$default_base_currency %||% "CHF"
  state$return_mode <- config$default_return_mode %||% "TOTAL"
  state$active_model_id <- config$default_risk_model %||% "shrinkage_long"
  state$active_portfolio_id <- "saa_50"
  state$active_scenario_id <- config$default_expectations_scenario %||% "base_5y"
  state$rebalancing_rule <- config$default_expectations$rebalancing_rule %||% "ANNUAL_REBALANCED"
  state$cost_bps_pa <- as.numeric(config$fx_policy$default_hedge_cost_bps_pa %||% 0.0)

  state$universe <- NULL
  state$portfolios <- config$portfolio_templates
  state$hedge_mode <- config$default_hedge_mode %||% "ASSET_CLASS"
  state$hedge_configs <- config$default_hedge_configs
  state$saved_asset_class_config <- config$default_hedge_configs$ASSET_CLASS
  state$expectations <- validate_expectations(config$default_expectations)
  state$carry_overrides <- list()

  # Active computed artifacts
  state$current_basis <- NULL
  state$current_model <- NULL
  state$current_portfolio_eval <- NULL
  state$current_scenario_eval <- NULL
  state$current_comparison_eval <- NULL
  state$current_historical_path <- NULL
  state$current_validation <- NULL
  state$last_export <- NULL

  # Job tracking for stale-response rejection
  state$latest_request_by_type <- list()
  state$active_jobs <- list()

  state
}

get_active_cache <- function(state) {
  if (isTRUE(state$is_private_upload) || is.null(state$shared_cache)) {
    state$session_cache
  } else {
    state$shared_cache
  }
}

load_dataset_into_state <- function(state, workbook_path, is_private = FALSE) {
  cache <- if (isTRUE(is_private) || is.null(state$shared_cache)) state$session_cache else state$shared_cache
  file_hash <- compute_file_hash(workbook_path)

  imp_res <- cache$get_or_compute(
    tier = "import",
    key_parts = list(
      file_hash = file_hash,
      import_version = "1.0.0",
      cash_proxy = state$config$cash_proxy_config
    ),
    counter_name = "import_calls",
    compute_fn = function() {
      wb <- read_input_workbook(workbook_path, state$config)
      if (!isTRUE(wb$valid)) {
        return(wb)
      }
      reg <- build_series_registry(
        data = wb$long_data,
        meta = wb$meta_raw,
        policy = state$config,
        file_hash = wb$file_hash,
        source_path = wb$path
      )
      spots <- normalize_spot_quotes(reg, state$config)
      fwds <- normalize_forward_quotes(reg, spots, state$config)
      cash <- build_cash_returns(reg, state$config)
      cash_tr <- build_cash_tr_series(cash, reg, state$config)
      if (isTRUE(cash_tr$valid) && nrow(cash_tr$data) > 0L) {
        reg <- attach_cash_tr_to_registry(reg, cash_tr)
      }
      list(
        valid = reg$valid && spots$valid && fwds$valid,
        registry = reg,
        spots = spots,
        forwards = fwds,
        cash = cash,
        issues = c(wb$issues, reg$issues, spots$issues, fwds$issues, cash$issues, cash_tr$issues)
      )
    }
  )

  bundle <- imp_res$value
  if (!isTRUE(bundle$valid)) {
    return(list(valid = FALSE, issues = bundle$issues, cache_hit = imp_res$cache_hit))
  }

  state$is_private_upload <- isTRUE(is_private)
  state$dataset <- bundle$registry
  state$spots <- bundle$spots
  state$forwards <- bundle$forwards
  state$cash <- bundle$cash
  state$universe <- bundle$registry$active_assets
  state$state_version <- state$state_version + 1L

  list(
    valid = TRUE,
    dataset = state$dataset,
    cache_hit = imp_res$cache_hit,
    issues = bundle$issues
  )
}

get_or_build_risk_basis <- function(state,
                                    base_ccy = state$base_currency,
                                    return_mode = state$return_mode,
                                    universe = state$universe,
                                    cost_bps_pa = state$cost_bps_pa) {
  cache <- get_active_cache(state)
  reg <- state$dataset

  fx_ret_res <- cache$get_or_compute(
    tier = "fx_returns",
    key_parts = list(
      data_hash = reg$data_hash,
      base_currency = base_ccy,
      return_mode = return_mode,
      universe = sort(as.character(universe)),
      cost_bps_pa = as.numeric(cost_bps_pa),
      convention_version = "1.0.0"
    ),
    counter_name = "fx_return_calls",
    compute_fn = function() {
      cr <- cross_fx_levels(state$spots, state$forwards, base_ccy = base_ccy)
      r_loc <- index_levels_to_returns(reg$data, reg$meta)
      u_b <- translate_asset_returns(r_loc, cr, reg$meta)
      z_b <- build_forward_pnl(cr, cost_bps_pa = cost_bps_pa)
      build_risk_basis(
        u = u_b,
        z = z_b,
        meta = reg$meta,
        universe = universe,
        base_ccy = base_ccy,
        return_mode = return_mode,
        cash_returns = state$cash
      )
    }
  )
  fx_ret_res
}

get_or_estimate_covariance <- function(state,
                                       model_id = state$active_model_id,
                                       base_ccy = state$base_currency,
                                       return_mode = state$return_mode,
                                       universe = state$universe,
                                       custom_settings = NULL) {
  cache <- get_active_cache(state)
  basis_res <- get_or_build_risk_basis(
    state = state,
    base_ccy = base_ccy,
    return_mode = return_mode,
    universe = universe,
    cost_bps_pa = state$cost_bps_pa
  )
  basis <- basis_res$value
  if (!isTRUE(basis$valid)) {
    return(list(value = basis, cache_hit = FALSE))
  }

  model_cfg <- state$config$risk_models[[model_id]] %||% state$config$risk_models$shrinkage_long
  if (!is.null(custom_settings) && is.list(custom_settings)) {
    for (nm in names(custom_settings)) {
      if (!is.null(custom_settings[[nm]])) {
        model_cfg[[nm]] <- custom_settings[[nm]]
      }
    }
  }

  # Notice: Portfolio weights and hedge ratios are strictly excluded from covariance cache key!
  cov_res <- cache$get_or_compute(
    tier = "covariance",
    key_parts = list(
      basis_hash = basis$basis_hash,
      model_id = model_id,
      estimator = model_cfg$estimator,
      shrinkage_target = model_cfg$shrinkage_target,
      window_mode = model_cfg$window_mode,
      window_months = model_cfg$window_months,
      halflife_months = model_cfg$halflife_months,
      regularization_gamma = model_cfg$regularization_gamma,
      missing_mode = model_cfg$missing_mode,
      factor_selection = model_cfg$factor_selection,
      start_month = model_cfg$start_month,
      end_month = model_cfg$end_month,
      engine_version = state$config$engine_version %||% "1.0.0"
    ),
    counter_name = "covariance_estimations",
    compute_fn = function() {
      sample_obj <- resolve_estimation_sample(basis, settings = model_cfg)
      if (!isTRUE(sample_obj$valid)) {
        return(sample_obj)
      }
      estimate_covariance(
        sample = sample_obj,
        method = model_cfg$estimator,
        settings = model_cfg,
        model_id = model_id,
        label = model_cfg$label
      )
    }
  )

  list(
    basis = basis,
    model = cov_res$value,
    basis_cache_hit = basis_res$cache_hit,
    model_cache_hit = cov_res$cache_hit
  )
}
