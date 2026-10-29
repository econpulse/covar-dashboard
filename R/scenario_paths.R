# ==============================================================================
# R/scenario_paths.R
# Deterministic 5-year scenario path evaluation under explicit rebalancing
# rules and USD-numeraire currency triangulation
# Pure R module - no Shiny dependencies
# ==============================================================================

evaluate_return_scenarios <- function(expectations,
                                      weights,
                                      hedge_config,
                                      meta,
                                      base_ccy = "CHF",
                                      scenario_id = NULL,
                                      rebalancing_rule = NULL,
                                      cost_bps_pa = 0.0,
                                      carry_overrides = NULL,
                                      fx_spot_overrides = NULL,
                                      available_hedge_currencies = NULL) {
  exp_obj <- if (inherits(expectations, "maps_expectations")) {
    expectations
  } else {
    validate_expectations(expectations, meta = meta)
  }

  if (!isTRUE(exp_obj$valid)) {
    return(list(valid = FALSE, issues = exp_obj$issues))
  }

  scen_id <- scenario_id %||% names(exp_obj$scenarios)[1L]
  if (!(scen_id %in% names(exp_obj$scenarios))) {
    return(list(
      valid = FALSE,
      issues = list(make_issue(
        code = "UNKNOWN_SCENARIO_ID",
        severity = "ERROR",
        message = sprintf("Szenario '%s' nicht in Erwartungsdaten gefunden.", scen_id),
        scope = "SCENARIOS"
      ))
    ))
  }

  scen <- exp_obj$scenarios[[scen_id]]
  horizon <- as.integer(exp_obj$horizon %||% 5L)
  rebal_rule <- toupper(rebalancing_rule %||% exp_obj$rebalancing_rule %||% "ANNUAL_REBALANCED")
  if (!(rebal_rule %in% c("ANNUAL_REBALANCED", "MONTHLY_REBALANCED", "BUY_AND_HOLD"))) {
    rebal_rule <- "ANNUAL_REBALANCED"
  }

  # Ensure all assets defined in metadata (ASSET_TR) are included so that inactive assets
  # and assets not currently held in the portfolio are fully represented and editable in the CMA list
  meta_dt <- data.table::as.data.table(meta)
  all_asset_codes <- meta_dt[Series_Type == "ASSET_TR", Code]
  all_asset_codes <- order_asset_universe(all_asset_codes, meta_dt, base_ccy = base_ccy)

  full_weights <- numeric(length(all_asset_codes))
  names(full_weights) <- all_asset_codes
  if (!is.null(weights) && length(weights) > 0L) {
    m_w <- match(names(weights), all_asset_codes)
    valid_m <- !is.na(m_w)
    full_weights[m_w[valid_m]] <- as.numeric(weights)[valid_m]
    # Retain any codes in weights that are not in all_asset_codes (e.g. invalid code tests)
    unknown_codes <- setdiff(names(weights), all_asset_codes)
    if (length(unknown_codes) > 0L) {
      for (uc in unknown_codes) {
        full_weights[uc] <- as.numeric(weights[[uc]])
      }
    }
  }

  # Resolve exposures and hedge notionals
  exp_pre <- derive_prehedge_exposures(
    weights = full_weights,
    meta = meta,
    base_ccy = base_ccy,
    available_hedge_currencies = available_hedge_currencies
  )
  hedges <- resolve_hedge_notionals(exp_pre, hedge_config = hedge_config)
  if (!isTRUE(hedges$valid)) {
    return(list(valid = FALSE, issues = hedges$issues))
  }

  asset_dt <- hedges$asset_table
  asset_ids <- asset_dt$asset_id
  n_assets <- length(asset_ids)

  # Triangulate annual currency return factors S_y^(B/C) / S_{y-1}^(B/C) from USD numeraire paths
  # usd_numeraire_paths[[C]][y] is V_{C,y} / V_{C,y-1} (annual appreciation factor of C vs USD)
  num_paths <- scen$usd_numeraire_paths
  short_rates <- scen$short_rates_pa %||% list()
  v_factor_base <- as.numeric(num_paths[[base_ccy]] %||% rep(1.0, horizon))
  r_short_base <- as.numeric(short_rates[[base_ccy]] %||% rep(0.0, horizon))

  cost_dec_pa <- as.numeric(cost_bps_pa %||% 0.0) / 10000.0

  # Construct annual matrices (horizon x n_assets) for local return, FX return, unhedged u^B, and hedge z^B
  r_local_mat <- matrix(0.0, nrow = horizon, ncol = n_assets, dimnames = list(paste0("Y", seq_len(horizon)), asset_ids))
  fx_ret_mat <- matrix(0.0, nrow = horizon, ncol = n_assets, dimnames = list(paste0("Y", seq_len(horizon)), asset_ids))
  u_base_mat <- matrix(0.0, nrow = horizon, ncol = n_assets, dimnames = list(paste0("Y", seq_len(horizon)), asset_ids))
  z_ccy_list <- list()

  # Identify all foreign currencies that can be hedged (including available forwards CHF, EUR, GBP, USD)
  all_fwd_supported <- unique(c("CHF", "EUR", "USD", "GBP", available_hedge_currencies))
  all_fwd_supported <- setdiff(all_fwd_supported, c(base_ccy, "EMEQ", "EMFI"))

  all_ccys <- unique(c(base_ccy, asset_dt$return_currency, asset_dt$fx_exposure, all_fwd_supported))
  all_ccys <- setdiff(all_ccys, c("EMEQ", "EMFI"))

  for (ccy in all_ccys) {
    v_fac_c <- as.numeric(num_paths[[ccy]] %||% rep(1.0, horizon))
    r_short_c <- as.numeric(short_rates[[ccy]] %||% rep(0.0, horizon))
    # Cross rate growth factor S_y^(B/C) / S_{y-1}^(B/C) = (V_{C,y}/V_{C,y-1}) / (V_{B,y}/V_{B,y-1})
    s_growth <- if (identical(ccy, base_ccy)) rep(1.0, horizon) else (v_fac_c / v_factor_base)
    fx_ret <- if (identical(ccy, base_ccy)) {
      rep(0.0, horizon)
    } else if (!is.null(fx_spot_overrides) && ccy %in% names(fx_spot_overrides) && !is.na(as.numeric(fx_spot_overrides[[ccy]]))) {
      rep(as.numeric(fx_spot_overrides[[ccy]]), horizon)
    } else {
      s_growth - 1.0
    }
    # Covered interest parity forward carry or explicit user override:
    carry_vec <- if (identical(ccy, base_ccy)) {
      rep(0.0, horizon)
    } else if (!is.null(carry_overrides) && ccy %in% names(carry_overrides) && !is.na(as.numeric(carry_overrides[[ccy]]))) {
      rep(as.numeric(carry_overrides[[ccy]]), horizon)
    } else {
      ((1.0 + r_short_base) / (1.0 + r_short_c) - 1.0)
    }
    spot_offset_vec <- -fx_ret
    tc_vec <- if (identical(ccy, base_ccy)) rep(0.0, horizon) else rep(cost_dec_pa, horizon)
    z_vec <- carry_vec + spot_offset_vec - tc_vec

    z_ccy_list[[ccy]] <- list(
      currency = ccy,
      fx_return = fx_ret,
      carry = carry_vec,
      spot_offset = spot_offset_vec,
      tc_drag = tc_vec,
      z_base = z_vec
    )
  }

  for (i in seq_len(n_assets)) {
    aid <- asset_ids[i]
    ret_ccy <- asset_dt$return_currency[i]
    aa <- scen$asset_assumptions[[aid]]
    r_loc <- if (!is.null(aa)) {
      as.numeric(aa$annual_local_returns)
    } else if (identical(asset_dt$asset_class[i], "CASH") && ret_ccy %in% names(short_rates)) {
      as.numeric(short_rates[[ret_ccy]])
    } else {
      rep(0.0, horizon)
    }
    fx_r <- z_ccy_list[[ret_ccy]]$fx_return %||% rep(0.0, horizon)
    # Multiplicative translation: u_{i,y}^B = (1 + r_{L,i,y}) * (1 + fx_{C,y}^B) - 1
    u_b <- (1.0 + r_loc) * (1.0 + fx_r) - 1.0

    r_local_mat[, aid] <- r_loc
    fx_ret_mat[, aid] <- fx_r
    u_base_mat[, aid] <- u_b
  }

  # Asset-level scenario summary table (5Y geometric CAGRs per asset, local vs unhedged vs hedged)
  sc_cagr_loc <- numeric(n_assets)
  sc_cagr_unh <- numeric(n_assets)
  sc_cagr_hed <- numeric(n_assets)
  sc_avg_carry <- numeric(n_assets)
  sc_arith_loc <- numeric(n_assets)
  sc_arith_hed <- numeric(n_assets)
  sc_is_act <- if (!is.null(asset_dt$is_active)) as.logical(asset_dt$is_active) else rep(TRUE, n_assets)

  for (i in seq_len(n_assets)) {
    aid <- asset_ids[i]
    exp_ccy <- asset_dt$fx_exposure[i]
    h_i <- asset_dt$applied_h[i]
    z_vec <- if (exp_ccy %in% names(z_ccy_list)) z_ccy_list[[exp_ccy]]$z_base else rep(0.0, horizon)
    carry_vec <- if (exp_ccy %in% names(z_ccy_list)) z_ccy_list[[exp_ccy]]$carry else rep(0.0, horizon)

    r_loc_vec <- r_local_mat[, aid]
    u_vec <- u_base_mat[, aid]
    r_hedged_vec <- u_vec + h_i * z_vec

    sc_cagr_loc[i] <- prod(1.0 + r_loc_vec)^(1.0 / horizon) - 1.0
    sc_cagr_unh[i] <- prod(1.0 + u_vec)^(1.0 / horizon) - 1.0
    sc_cagr_hed[i] <- prod(1.0 + r_hedged_vec)^(1.0 / horizon) - 1.0
    sc_avg_carry[i] <- if (asset_dt$is_domestic[i] || asset_dt$is_emeq[i]) 0.0 else mean(carry_vec)
    sc_arith_loc[i] <- mean(r_loc_vec)
    sc_arith_hed[i] <- mean(r_hedged_vec)
  }

  asset_scen_dt <- data.table::data.table(
    asset_id = asset_ids,
    name = asset_dt$name,
    asset_class = asset_dt$asset_class,
    return_currency = asset_dt$return_currency,
    fx_exposure = asset_dt$fx_exposure,
    is_domestic = asset_dt$is_domestic,
    is_active = sc_is_act,
    forward_available = asset_dt$forward_available,
    hedge_eligible = asset_dt$hedge_eligible,
    weight = asset_dt$weight,
    applied_h = asset_dt$applied_h,
    cagr_local_5y = sc_cagr_loc,
    cagr_unhedged_base_5y = sc_cagr_unh,
    cagr_hedged_base_5y = sc_cagr_hed,
    avg_forward_carry_pa = sc_avg_carry,
    arith_mean_local_pa = sc_arith_loc,
    arith_mean_hedged_base_pa = sc_arith_hed
  )

  # Evaluate portfolio 5-year path according to explicit rebalancing rule
  # IMPORTANT (Section 8): NEVER weight individual 5Y asset CAGRs and call that Portfolio CAGR!
  w_init <- asset_dt$weight
  names(w_init) <- asset_ids
  h_vec <- asset_dt$applied_h
  names(h_vec) <- asset_ids

  port_hedged_annual <- numeric(horizon)
  port_unhedged_annual <- numeric(horizon)
  port_local_annual <- numeric(horizon)

  w_curr_hedged <- w_init
  w_curr_unhedged <- w_init

  for (y in seq_len(horizon)) {
    u_y <- u_base_mat[y, ]
    r_loc_y <- r_local_mat[y, ]
    z_y <- vapply(asset_dt$fx_exposure, function(ccy) {
      if (ccy %in% names(z_ccy_list)) z_ccy_list[[ccy]]$z_base[y] else 0.0
    }, numeric(1))

    if (identical(rebal_rule, "MONTHLY_REBALANCED")) {
      # Monthly rebalancing within year y (12 sub-periods)
      u_m <- (1.0 + u_y)^(1.0 / 12.0) - 1.0
      z_m <- z_y / 12.0
      r_p_m_hedged <- sum(w_init * (u_m + h_vec * z_m))
      r_p_m_unhedged <- sum(w_init * u_m)
      port_hedged_annual[y] <- (1.0 + r_p_m_hedged)^12 - 1.0
      port_unhedged_annual[y] <- (1.0 + r_p_m_unhedged)^12 - 1.0
      port_local_annual[y] <- sum(w_init * r_loc_y)
    } else if (identical(rebal_rule, "BUY_AND_HOLD")) {
      r_asset_h_y <- u_y + h_vec * z_y
      port_hedged_annual[y] <- sum(w_curr_hedged * r_asset_h_y)
      port_unhedged_annual[y] <- sum(w_curr_unhedged * u_y)
      port_local_annual[y] <- sum(w_curr_hedged * r_loc_y)
      # Drift weights to end of year y
      w_curr_hedged <- w_curr_hedged * (1.0 + r_asset_h_y) / (1.0 + port_hedged_annual[y])
      w_curr_unhedged <- w_curr_unhedged * (1.0 + u_y) / (1.0 + port_unhedged_annual[y])
    } else {
      # Standard ANNUAL_REBALANCED: r_{P,y} = w' u_y + g' z_y
      port_hedged_annual[y] <- sum(w_init * (u_y + h_vec * z_y))
      port_unhedged_annual[y] <- sum(w_init * u_y)
      port_local_annual[y] <- sum(w_init * r_loc_y)
    }
  }

  wealth_hedged <- c(100.0, 100.0 * cumprod(1.0 + port_hedged_annual))
  wealth_unhedged <- c(100.0, 100.0 * cumprod(1.0 + port_unhedged_annual))
  wealth_local <- c(100.0, 100.0 * cumprod(1.0 + port_local_annual))

  portfolio_scenario_cagr_hedged <- (tail(wealth_hedged, 1L) / 100.0)^(1.0 / horizon) - 1.0
  portfolio_scenario_cagr_unhedged <- (tail(wealth_unhedged, 1L) / 100.0)^(1.0 / horizon) - 1.0
  portfolio_scenario_cagr_local <- (tail(wealth_local, 1L) / 100.0)^(1.0 / horizon) - 1.0

  # Single-period arithmetic expectation E[r_P] = w' E[u] + g' E[z] (Year 1 and 5Y average)
  arith_expected_y1_hedged <- port_hedged_annual[1L]
  arith_expected_avg_hedged <- mean(port_hedged_annual)

  yearly_path_dt <- data.table::data.table(
    year_index = 0:horizon,
    label = c("Start", paste0("Jahr ", seq_len(horizon))),
    return_hedged_base = c(0.0, port_hedged_annual),
    return_unhedged_base = c(0.0, port_unhedged_annual),
    return_local = c(0.0, port_local_annual),
    wealth_hedged_base = wealth_hedged,
    wealth_unhedged_base = wealth_unhedged,
    wealth_local = wealth_local
  )

  # Build structured summary of all foreign hedge currencies vs base_ccy
  hedge_currency_summary <- lapply(all_fwd_supported, function(ccy) {
    z_info <- z_ccy_list[[ccy]]
    port_sub <- asset_dt[fx_exposure == ccy]
    port_exp <- sum(port_sub$weight, na.rm = TRUE)
    is_eligible <- any(port_sub$hedge_eligible == TRUE)
    is_needed <- (port_exp > 1e-6) && is_eligible
    applied_h_ratio <- if (nrow(port_sub) > 0L && sum(port_sub$weight, na.rm = TRUE) > 0) {
      sum(port_sub$hedge_notional, na.rm = TRUE) / sum(port_sub$weight, na.rm = TRUE)
    } else 0.0

    avg_c <- if (!is.null(z_info)) mean(z_info$carry) else 0.0
    avg_spot <- if (!is.null(z_info)) mean(z_info$fx_return) else 0.0
    is_ovr <- !is.null(carry_overrides) && ccy %in% names(carry_overrides) && !is.na(as.numeric(carry_overrides[[ccy]]))
    is_spot_ovr <- !is.null(fx_spot_overrides) && ccy %in% names(fx_spot_overrides) && !is.na(as.numeric(fx_spot_overrides[[ccy]]))

    list(
      currency = ccy,
      is_domestic = FALSE,
      is_needed = is_needed,
      forward_available = TRUE,
      portfolio_exposure = port_exp,
      applied_hedge_ratio = applied_h_ratio,
      avg_forward_carry_pa = avg_c,
      is_overridden = is_ovr,
      avg_spot_return_pa = avg_spot,
      is_spot_overridden = is_spot_ovr
    )
  })

  relevant_hedge_currencies <- vapply(
    hedge_currency_summary[vapply(hedge_currency_summary, function(x) isTRUE(x$is_needed), logical(1))],
    function(x) x$currency,
    character(1)
  )

  list(
    valid = TRUE,
    expectations_id = exp_obj$expectations_id,
    as_of = exp_obj$as_of,
    scenario_id = scen_id,
    scenario_label = scen$label,
    scenario_description = scen$description,
    base_currency = base_ccy,
    horizon = horizon,
    rebalancing_rule = rebal_rule,
    fee_basis = exp_obj$fee_basis,
    uncertainty_description = exp_obj$uncertainty_description,
    metrics = list(
      scenario_cagr_hedged_5y = portfolio_scenario_cagr_hedged,
      scenario_cagr_unhedged_5y = portfolio_scenario_cagr_unhedged,
      scenario_cagr_local_5y = portfolio_scenario_cagr_local,
      scenario_hedge_effect_pa = portfolio_scenario_cagr_hedged - portfolio_scenario_cagr_unhedged,
      cumulative_return_hedged_5y = (tail(wealth_hedged, 1L) / 100.0) - 1.0,
      cumulative_return_unhedged_5y = (tail(wealth_unhedged, 1L) / 100.0) - 1.0,
      single_period_arith_y1 = arith_expected_y1_hedged,
      single_period_arith_avg = arith_expected_avg_hedged
    ),
    yearly_path = yearly_path_dt,
    asset_table = asset_scen_dt,
    u_base_mat = u_base_mat,
    z_ccy_list = z_ccy_list,
    asset_dt = asset_dt,
    carry_overrides = carry_overrides,
    relevant_hedge_currencies = relevant_hedge_currencies,
    available_hedge_currencies = all_fwd_supported,
    hedge_currency_summary = hedge_currency_summary,
    issues = exp_obj$issues
  )
}
