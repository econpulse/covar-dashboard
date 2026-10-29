# ==============================================================================
# R/portfolio_metrics.R
# Portfolio risk evaluation (a = (w, g), Var_P = a' Sigma_B a), component &
# asset-level risk contributions, multi-SAA tracking error comparisons, and
# historical portfolio path backtest
# Pure R module - no Shiny dependencies
# ==============================================================================

assemble_exposure_vector <- function(weights, g_vector, component_ids) {
  a_vec <- stats::setNames(numeric(length(component_ids)), component_ids)
  w_common <- intersect(names(weights), component_ids)
  if (length(w_common) > 0L) {
    a_vec[w_common] <- as.numeric(weights[w_common])
  }
  if (!is.null(g_vector) && length(g_vector) > 0L) {
    g_common <- intersect(names(g_vector), component_ids)
    if (length(g_common) > 0L) {
      a_vec[g_common] <- as.numeric(g_vector[g_common])
    }
  }
  a_vec
}

evaluate_portfolio <- function(weights,
                               hedge_config,
                               model,
                               meta,
                               cost_bps_pa = 0.0,
                               portfolio_id = "saa_active",
                               portfolio_name = "Aktives Portfolio") {
  comp_ids <- model$component_ids
  asset_ids <- model$asset_ids
  Sigma_m <- model$Sigma_monthly

  # Normalize weights vector over model$asset_ids (allowing 0-weight universe assets)
  w_full <- stats::setNames(numeric(length(asset_ids)), asset_ids)
  matched_w <- intersect(names(weights), asset_ids)
  w_full[matched_w] <- as.numeric(weights[matched_w])

  exp_pre <- derive_prehedge_exposures(
    weights = w_full,
    meta = meta,
    base_ccy = model$base_currency,
    available_hedge_currencies = model$supported_hedge_currencies %||% model$available_hedge_currencies,
    fx_carry = model$latest_carry
  )
  hedges <- resolve_hedge_notionals(exp_pre, hedge_config = hedge_config)

  issues <- c(model$issues, hedges$issues)
  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))

  if (has_errors) {
    return(list(
      valid = FALSE,
      portfolio_id = portfolio_id,
      portfolio_name = portfolio_name,
      base_currency = model$base_currency,
      model_id = model$model_id,
      exposures = hedges,
      issues = issues
    ))
  }

  # Full exposure vector a = (w, g); Sum(w) = 1, g is NOT added to capital weights
  a_vec <- assemble_exposure_vector(w_full, hedges$g_vector, comp_ids)
  a_unhedged <- assemble_exposure_vector(w_full, NULL, comp_ids)

  # Portfolio variance and volatility (monthly and annualized 12 * Sigma convention)
  Sigma_a <- as.numeric(Sigma_m %*% a_vec)
  var_m <- max(0.0, as.numeric(crossprod(a_vec, Sigma_a)))
  vol_m <- sqrt(var_m)
  var_ann <- 12.0 * var_m
  vol_ann <- sqrt(var_ann)

  # Unhedged portfolio variance and volatility
  Sigma_a_unh <- as.numeric(Sigma_m %*% a_unhedged)
  var_unh_m <- max(0.0, as.numeric(crossprod(a_unhedged, Sigma_a_unh)))
  vol_unh_m <- sqrt(var_unh_m)
  vol_unh_ann <- sqrt(12.0 * var_unh_m)

  # Component-level marginal and total risk contributions (negative contributions preserved!)
  mrc_ann <- if (vol_ann > 1e-14) (12.0 * Sigma_a) / vol_ann else rep(0.0, length(comp_ids))
  rc_ann <- a_vec * mrc_ann
  pcr <- if (vol_ann > 1e-14) rc_ann / vol_ann else rep(0.0, length(comp_ids))

  comp_type <- ifelse(comp_ids %in% asset_ids, "ASSET_TR", "FX_HEDGE")
  comp_dt <- data.table::data.table(
    component_id = comp_ids,
    component_type = comp_type,
    exposure = as.numeric(a_vec),
    standalone_vol_ann = as.numeric(model$vol_annualized[comp_ids]),
    marginal_risk_ann = as.numeric(mrc_ann),
    risk_contribution_ann = as.numeric(rc_ann),
    pct_risk_contribution = as.numeric(pcr)
  )

  # Asset-level transformed covariance matrix: Sigma_{assets,B,h} = A(h) Sigma_B A(h)'
  trans <- build_asset_transform(hedges, comp_ids)
  A_mat <- trans$A_matrix
  Sigma_assets_m <- A_mat %*% Sigma_m %*% t(A_mat)
  Sigma_assets_ann <- 12.0 * Sigma_assets_m

  # Verify analytical equality w' Sigma_{assets,B,h} w == a' Sigma_B a
  var_assets_m <- max(0.0, as.numeric(crossprod(w_full, Sigma_assets_m %*% w_full)))
  Sigma_assets_w_ann <- as.numeric(Sigma_assets_ann %*% w_full)
  asset_mrc_ann <- if (vol_ann > 1e-14) Sigma_assets_w_ann / vol_ann else rep(0.0, length(asset_ids))
  asset_rc_ann <- w_full * asset_mrc_ann
  asset_pcr <- if (vol_ann > 1e-14) asset_rc_ann / vol_ann else rep(0.0, length(asset_ids))
  asset_hedged_vol_ann <- sqrt(pmax(0.0, diag(Sigma_assets_ann)))
  asset_unhedged_vol_ann <- as.numeric(model$vol_annualized[asset_ids])

  asset_rc_dt <- merge(
    hedges$asset_table[, .(asset_id, name, asset_class, fx_exposure, weight, applied_h, hedge_notional, post_hedge_exposure, eligibility_reason)],
    data.table::data.table(
      asset_id = asset_ids,
      unhedged_vol_ann = asset_unhedged_vol_ann,
      hedged_vol_ann = asset_hedged_vol_ann,
      marginal_risk_ann = asset_mrc_ann,
      risk_contribution_ann = asset_rc_ann,
      pct_risk_contribution = asset_pcr
    ),
    by = "asset_id",
    sort = FALSE
  )

  # Asset-class risk contribution summary
  class_rc_dt <- asset_rc_dt[, .(
    weight = sum(weight, na.rm = TRUE),
    hedge_notional = sum(hedge_notional, na.rm = TRUE),
    standalone_vol_ann = ifelse(sum(weight, na.rm = TRUE) > 1e-12, sum(weight * hedged_vol_ann, na.rm = TRUE) / sum(weight, na.rm = TRUE), 0.0),
    marginal_risk_ann = ifelse(sum(weight, na.rm = TRUE) > 1e-12, sum(risk_contribution_ann, na.rm = TRUE) / sum(weight, na.rm = TRUE), 0.0),
    risk_contribution_ann = sum(risk_contribution_ann, na.rm = TRUE),
    pct_risk_contribution = sum(pct_risk_contribution, na.rm = TRUE)
  ), by = asset_class]

  # Historical mean return projection (diagnostic only, not forward expectation)
  mu_m <- model$mean_monthly[comp_ids]
  tc_monthly <- (as.numeric(cost_bps_pa %||% 0.0) / 10000.0) / 12.0 * sum(abs(hedges$g_vector), na.rm = TRUE)
  hist_mean_m <- if (all(!is.na(mu_m))) sum(a_vec * mu_m) - tc_monthly else NA_real_
  hist_mean_ann <- if (!is.na(hist_mean_m)) 12.0 * hist_mean_m else NA_real_

  # Diversification ratio = (Sum w_i * vol_i_hedged) / vol_P
  weighted_avg_vol_ann <- sum(w_full * asset_hedged_vol_ann, na.rm = TRUE)
  div_ratio <- if (vol_ann > 1e-14) weighted_avg_vol_ann / vol_ann else 1.0
  div_benefit_ann <- vol_ann - weighted_avg_vol_ann

  config_hash <- compute_object_hash(list(
    model_hash = model$model_hash,
    base_currency = model$base_currency,
    weights = round(w_full, 10),
    g_vector = round(hedges$g_vector, 10),
    hedge_mode = hedges$active_mode,
    cost_bps_pa = as.numeric(cost_bps_pa %||% 0.0)
  ))

  out <- list(
    valid = TRUE,
    portfolio_id = portfolio_id,
    portfolio_name = portfolio_name,
    config_hash = substr(config_hash, 1L, 16L),
    base_currency = model$base_currency,
    return_mode = model$return_mode,
    model_id = model$model_id,
    model_label = model$label,
    model_hash = model$model_hash,
    n_obs = model$n_obs,
    start_month = model$start_month,
    end_month = model$end_month,
    weights = w_full,
    g_vector = hedges$g_vector,
    a_vector = a_vec,
    metrics = list(
      var_monthly = var_m,
      vol_monthly = vol_m,
      var_annualized = var_ann,
      vol_annualized = vol_ann,
      vol_unhedged_annualized = vol_unh_ann,
      hedge_vol_effect_annualized = vol_ann - vol_unh_ann,
      weighted_avg_asset_vol_ann = weighted_avg_vol_ann,
      diversification_ratio = div_ratio,
      diversification_benefit_ann = div_benefit_ann,
      historical_arith_mean_ann = hist_mean_ann,
      hedge_cost_drag_ann = 12.0 * tc_monthly,
      transform_identity_diff = abs(var_m - var_assets_m),
      weight_sum = hedges$weight_sum,
      equity_share = exp_pre$equity_share,
      foreign_exposure_before = hedges$foreign_exposure_before,
      foreign_exposure_after = hedges$foreign_exposure_after,
      total_hedge_notional = hedges$total_hedge_notional,
      emeq_exposure = hedges$emeq_exposure
    ),
    exposures = hedges,
    component_contributions = comp_dt,
    asset_contributions = asset_rc_dt,
    class_contributions = class_rc_dt,
    Sigma_assets_monthly = Sigma_assets_m,
    Sigma_assets_annualized = Sigma_assets_ann,
    asset_correlation_matrix = cov_to_corr_safe(Sigma_assets_m),
    A_matrix = A_mat,
    issues = issues
  )
  class(out) <- c("maps_portfolio_eval", "list")
  out
}

compare_portfolios <- function(configs,
                               model,
                               meta,
                               benchmark_index = 1L,
                               extra_models = NULL,
                               cache = NULL,
                               scenario_eval = NULL) {
  if (length(configs) == 0L) {
    return(list(valid = FALSE, message = "Keine Portfoliokonfigurationen zum Vergleich übergeben."))
  }

  evals <- lapply(seq_along(configs), function(i) {
    cfg <- configs[[i]]
    pid <- cfg$id %||% paste0("saa_", i)
    pname <- cfg$name %||% paste0("SAA ", i)
    if (!is.null(cache)) {
      res_c <- cache$get_or_compute(
        tier = "portfolio",
        key_parts = list(
          model_hash = model$model_hash,
          portfolio_id = pid,
          weights = round(as.numeric(cfg$weights), 10),
          hedge_config = cfg$hedge_config,
          cost_bps_pa = as.numeric(cfg$cost_bps_pa %||% 0.0)
        ),
        compute_fn = function() {
          evaluate_portfolio(
            weights = cfg$weights,
            hedge_config = cfg$hedge_config,
            model = model,
            meta = meta,
            cost_bps_pa = cfg$cost_bps_pa %||% 0.0,
            portfolio_id = pid,
            portfolio_name = pname
          )
        }
      )
      res_c$value
    } else {
      evaluate_portfolio(
        weights = cfg$weights,
        hedge_config = cfg$hedge_config,
        model = model,
        meta = meta,
        cost_bps_pa = cfg$cost_bps_pa %||% 0.0,
        portfolio_id = pid,
        portfolio_name = pname
      )
    }
  })

  valid_mask <- vapply(evals, function(x) isTRUE(x$valid), logical(1))
  if (!all(valid_mask)) {
    return(list(valid = FALSE, evaluations = evals))
  }

  b_idx <- min(max(1L, as.integer(benchmark_index)), length(evals))
  bench <- evals[[b_idx]]
  Sigma_m <- model$Sigma_monthly
  comp_ids <- model$component_ids

  summary_rows <- vector("list", length(evals))
  delta_rows <- vector("list", length(evals))
  pairwise_te <- matrix(0.0, nrow = length(evals), ncol = length(evals))
  p_ids <- vapply(evals, function(x) x$portfolio_id, character(1))
  dimnames(pairwise_te) <- list(p_ids, p_ids)

  for (i in seq_along(evals)) {
    ev <- evals[[i]]
    delta_a <- ev$a_vector - bench$a_vector
    te_var_m <- max(0.0, as.numeric(crossprod(delta_a, Sigma_m %*% delta_a)))
    te_ann <- sqrt(12.0 * te_var_m)

    # Compute portfolio return metrics under current CMA scenario
    cma_ret <- NA_real_
    cma_unh <- NA_real_
    cma_sharpe <- NA_real_

    if (!is.null(scenario_eval) && isTRUE(scenario_eval$valid) && !is.null(scenario_eval$u_base_mat)) {
      u_mat <- scenario_eval$u_base_mat
      horizon <- nrow(u_mat)
      asset_ids_scen <- colnames(u_mat)
      z_list <- scenario_eval$z_ccy_list
      asset_dt_scen <- scenario_eval$asset_dt

      w_vec_scen <- as.numeric(ev$weights[asset_ids_scen])
      w_vec_scen[is.na(w_vec_scen)] <- 0.0

      h_vec_scen <- if (!is.null(ev$applied_h)) {
        as.numeric(ev$applied_h[asset_ids_scen])
      } else if (!is.null(ev$h_vector)) {
        as.numeric(ev$h_vector[asset_ids_scen])
      } else if (!is.null(ev$exposures$asset_table)) {
        m_at <- match(asset_ids_scen, ev$exposures$asset_table$asset_id)
        as.numeric(ev$exposures$asset_table$applied_h[m_at])
      } else {
        rep(0.0, length(asset_ids_scen))
      }
      h_vec_scen[is.na(h_vec_scen)] <- 0.0

      port_h <- numeric(horizon)
      port_u <- numeric(horizon)
      for (y in seq_len(horizon)) {
        u_y <- u_mat[y, ]
        z_y <- vapply(asset_dt_scen$fx_exposure, function(ccy) {
          if (ccy %in% names(z_list)) z_list[[ccy]]$z_base[y] else 0.0
        }, numeric(1))
        port_h[y] <- sum(w_vec_scen * (u_y + h_vec_scen * z_y))
        port_u[y] <- sum(w_vec_scen * u_y)
      }
      cma_ret <- prod(1.0 + port_h)^(1.0 / horizon) - 1.0
      cma_unh <- prod(1.0 + port_u)^(1.0 / horizon) - 1.0
      if (!is.na(cma_ret) && ev$metrics$vol_annualized > 1e-6) {
        cma_sharpe <- cma_ret / ev$metrics$vol_annualized
      }
    }

    summary_rows[[i]] <- data.table::data.table(
      portfolio_id = ev$portfolio_id,
      portfolio_name = ev$portfolio_name,
      config_hash = ev$config_hash,
      base_currency = ev$base_currency,
      model_id = ev$model_id,
      equity_share = ev$metrics$equity_share,
      foreign_before = ev$metrics$foreign_exposure_before,
      hedge_notional = ev$metrics$total_hedge_notional,
      foreign_after = ev$metrics$foreign_exposure_after,
      emeq_exposure = ev$metrics$emeq_exposure,
      cma_return_hedged_ann = cma_ret,
      cma_return_unhedged_ann = cma_unh,
      vol_unhedged_ann = ev$metrics$vol_unhedged_annualized,
      vol_hedged_ann = ev$metrics$vol_annualized,
      hedge_vol_effect_ann = ev$metrics$hedge_vol_effect_annualized,
      cma_sharpe_ratio = cma_sharpe,
      diversification_ratio = ev$metrics$diversification_ratio,
      tracking_error_vs_bench_ann = te_ann,
      vol_diff_vs_bench_ann = ev$metrics$vol_annualized - bench$metrics$vol_annualized
    )

    delta_rows[[i]] <- data.table::data.table(
      portfolio_id = ev$portfolio_id,
      benchmark_id = bench$portfolio_id,
      component_id = comp_ids,
      exposure_portfolio = as.numeric(ev$a_vector),
      exposure_benchmark = as.numeric(bench$a_vector),
      delta_exposure = as.numeric(delta_a)
    )

    for (j in seq_along(evals)) {
      if (i < j) {
        da_ij <- evals[[i]]$a_vector - evals[[j]]$a_vector
        te_ij <- sqrt(12.0 * max(0.0, as.numeric(crossprod(da_ij, Sigma_m %*% da_ij))))
        pairwise_te[i, j] <- te_ij
        pairwise_te[j, i] <- te_ij
      }
    }
  }

  # Fast cross-model sensitivity table using pre-computed exposure vectors a_vec and a_unhedged
  cross_model_dt <- data.table::data.table()
  if (!is.null(extra_models) && length(extra_models) > 0L) {
    cm_list <- list()
    for (m_obj in extra_models) {
      if (!isTRUE(m_obj$valid)) next
      S_m <- m_obj$Sigma_monthly
      m_cids <- m_obj$component_ids
      for (ev_i in evals) {
        a_v <- ev_i$a_vector[m_cids]
        a_u <- assemble_exposure_vector(ev_i$weights, NULL, m_cids)
        v_h <- sqrt(12.0 * max(0.0, as.numeric(crossprod(a_v, S_m %*% a_v))))
        v_u <- sqrt(12.0 * max(0.0, as.numeric(crossprod(a_u, S_m %*% a_u))))
        cm_list[[length(cm_list) + 1L]] <- data.table::data.table(
          portfolio_id = ev_i$portfolio_id,
          portfolio_name = ev_i$portfolio_name,
          model_id = m_obj$model_id,
          model_label = m_obj$label,
          n_obs = m_obj$n_obs,
          start_month = m_obj$start_month,
          end_month = m_obj$end_month,
          vol_annualized = v_h,
          vol_unhedged_annualized = v_u,
          hedge_vol_effect_annualized = v_h - v_u
        )
      }
    }
    if (length(cm_list) > 0L) {
      cross_model_dt <- data.table::rbindlist(cm_list, fill = TRUE)
    }
  }

  list(
    valid = TRUE,
    benchmark_id = bench$portfolio_id,
    base_currency = model$base_currency,
    model_id = model$model_id,
    summary_table = data.table::rbindlist(summary_rows, fill = TRUE),
    delta_exposures = data.table::rbindlist(delta_rows, fill = TRUE),
    pairwise_tracking_error = pairwise_te,
    cross_model_comparison = cross_model_dt,
    evaluations = stats::setNames(evals, p_ids)
  )
}

build_historical_portfolio_path <- function(basis,
                                            weights,
                                            g_vector = NULL,
                                            cost_bps_pa = 0.0,
                                            start_month = NULL,
                                            end_month = NULL) {
  comp_ids <- basis$component_ids
  X_mat <- basis$X_matrix
  month_keys <- basis$month_keys
  dates <- basis$dates

  a_vec <- assemble_exposure_vector(weights, g_vector, comp_ids)
  active_cols <- names(a_vec)[abs(a_vec) > 1e-12]
  if (length(active_cols) == 0L) {
    active_cols <- basis$asset_ids
  }

  valid_rows <- which(rowSums(is.na(X_mat[, active_cols, drop = FALSE])) == 0L)
  if (!is.null(start_month) && nzchar(start_month)) {
    valid_rows <- valid_rows[month_keys[valid_rows] >= start_month]
  }
  if (!is.null(end_month) && nzchar(end_month)) {
    valid_rows <- valid_rows[month_keys[valid_rows] <= end_month]
  }

  if (length(valid_rows) == 0L) {
    return(list(valid = FALSE, path = data.table::data.table()))
  }

  sub_X <- X_mat[valid_rows, active_cols, drop = FALSE]
  sub_a <- a_vec[active_cols]

  tc_monthly <- (as.numeric(cost_bps_pa %||% 0.0) / 10000.0) / 12.0 * sum(abs(g_vector %||% 0.0), na.rm = TRUE)
  r_port <- as.numeric(sub_X %*% sub_a) - tc_monthly

  # Unhedged comparison path on the exact same months
  w_only <- assemble_exposure_vector(weights, NULL, comp_ids)[active_cols]
  r_unh <- as.numeric(sub_X %*% w_only)

  wealth_hedged <- cumprod(1.0 + r_port) * 100.0
  wealth_unhedged <- cumprod(1.0 + r_unh) * 100.0
  peak_hedged <- cummax(c(100.0, wealth_hedged))[-1L]
  drawdown_hedged <- (wealth_hedged / peak_hedged) - 1.0

  n_m <- length(r_port)
  realized_vol_ann <- if (n_m > 1L) stats::sd(r_port) * sqrt(12.0) else NA_real_
  realized_cagr <- if (n_m > 0L && tail(wealth_hedged, 1L) > 0) (tail(wealth_hedged, 1L) / 100.0)^(12.0 / n_m) - 1.0 else NA_real_

  path_dt <- data.table::data.table(
    date = dates[valid_rows],
    month_key = month_keys[valid_rows],
    return_hedged = r_port,
    return_unhedged = r_unh,
    wealth_hedged = wealth_hedged,
    wealth_unhedged = wealth_unhedged,
    drawdown_hedged = drawdown_hedged
  )

  list(
    valid = TRUE,
    n_months = n_m,
    start_month = month_keys[valid_rows[1L]],
    end_month = month_keys[tail(valid_rows, 1L)],
    realized_vol_ann = realized_vol_ann,
    realized_historical_cagr = realized_cagr,
    max_drawdown = min(drawdown_hedged, na.rm = TRUE),
    path = path_dt
  )
}
