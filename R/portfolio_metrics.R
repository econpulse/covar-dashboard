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

  at_ref <- hedges$asset_table
  m_at <- match(asset_ids, at_ref$asset_id)
  ac_col <- at_ref$asset_class[m_at]
  w_col <- at_ref$weight[m_at]
  hn_col <- at_ref$hedge_notional[m_at]

  asset_rc_dt <- data.table::data.table(
    asset_id = asset_ids,
    name = at_ref$name[m_at],
    asset_class = ac_col,
    fx_exposure = at_ref$fx_exposure[m_at],
    weight = w_col,
    applied_h = at_ref$applied_h[m_at],
    hedge_notional = hn_col,
    post_hedge_exposure = at_ref$post_hedge_exposure[m_at],
    eligibility_reason = at_ref$eligibility_reason[m_at],
    unhedged_vol_ann = asset_unhedged_vol_ann,
    hedged_vol_ann = asset_hedged_vol_ann,
    marginal_risk_ann = asset_mrc_ann,
    risk_contribution_ann = asset_rc_ann,
    pct_risk_contribution = asset_pcr
  )

  # Asset-class risk contribution summary
  uniq_ac <- unique(ac_col)
  n_ac <- length(uniq_ac)
  ac_w <- numeric(n_ac)
  ac_hn <- numeric(n_ac)
  ac_svol <- numeric(n_ac)
  ac_mrc <- numeric(n_ac)
  ac_rc <- numeric(n_ac)
  ac_pcr <- numeric(n_ac)
  for (k in seq_len(n_ac)) {
    idx_k <- which(ac_col == uniq_ac[k])
    w_s <- sum(w_col[idx_k], na.rm = TRUE)
    rc_s <- sum(asset_rc_ann[idx_k], na.rm = TRUE)
    ac_w[k] <- w_s
    ac_hn[k] <- sum(hn_col[idx_k], na.rm = TRUE)
    ac_svol[k] <- if (w_s > 1e-12) sum(w_col[idx_k] * asset_hedged_vol_ann[idx_k], na.rm = TRUE) / w_s else 0.0
    ac_mrc[k] <- if (w_s > 1e-12) rc_s / w_s else 0.0
    ac_rc[k] <- rc_s
    ac_pcr[k] <- sum(asset_pcr[idx_k], na.rm = TRUE)
  }
  class_rc_dt <- data.table::data.table(
    asset_class = uniq_ac,
    weight = ac_w,
    hedge_notional = ac_hn,
    standalone_vol_ann = ac_svol,
    marginal_risk_ann = ac_mrc,
    risk_contribution_ann = ac_rc,
    pct_risk_contribution = ac_pcr
  )

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
    if (!is.null(cfg$precomputed_eval) && isTRUE(cfg$precomputed_eval$valid)) {
      return(cfg$precomputed_eval)
    }
    pid <- cfg$id %||% paste0("saa_", i)
    pname <- cfg$name %||% paste0("SAA ", i)
    if (!is.null(cache)) {
      res_c <- cache$get_or_compute(
        tier = "portfolio",
        key_parts = list(
          model_hash = model$model_hash,
          portfolio_id = pid,
          weights = round(cfg$weights, 10),
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

  n_evals <- length(evals)
  b_idx <- min(max(1L, as.integer(benchmark_index)), n_evals)
  bench <- evals[[b_idx]]
  Sigma_m <- model$Sigma_monthly
  comp_ids <- model$component_ids
  n_comp <- length(comp_ids)

  pairwise_te <- matrix(0.0, nrow = n_evals, ncol = n_evals)
  p_ids <- vapply(evals, function(x) x$portfolio_id, character(1))
  dimnames(pairwise_te) <- list(p_ids, p_ids)

  # Precompute scenario z matrix once if scenario_eval is provided
  has_scen <- !is.null(scenario_eval) && isTRUE(scenario_eval$valid) && !is.null(scenario_eval$u_base_mat)
  if (has_scen) {
    u_mat <- scenario_eval$u_base_mat
    horizon <- nrow(u_mat)
    asset_ids_scen <- colnames(u_mat)
    z_list <- scenario_eval$z_ccy_list
    fx_exp_scen <- scenario_eval$asset_dt$fx_exposure
    z_mat <- matrix(0.0, nrow = horizon, ncol = length(asset_ids_scen))
    for (y in seq_len(horizon)) {
      z_mat[y, ] <- vapply(fx_exp_scen, function(ccy) {
        if (ccy %in% names(z_list)) z_list[[ccy]]$z_base[y] else 0.0
      }, numeric(1))
    }
  }

  sum_pid <- character(n_evals)
  sum_pname <- character(n_evals)
  sum_hash <- character(n_evals)
  sum_bccy <- character(n_evals)
  sum_mid <- character(n_evals)
  sum_eq <- numeric(n_evals)
  sum_fb <- numeric(n_evals)
  sum_hn <- numeric(n_evals)
  sum_fa <- numeric(n_evals)
  sum_em <- numeric(n_evals)
  sum_cma_h <- rep(NA_real_, n_evals)
  sum_cma_u <- rep(NA_real_, n_evals)
  sum_vol_u <- numeric(n_evals)
  sum_vol_h <- numeric(n_evals)
  sum_h_eff <- numeric(n_evals)
  sum_sharpe <- rep(NA_real_, n_evals)
  sum_div <- numeric(n_evals)
  sum_te <- numeric(n_evals)
  sum_vdiff <- numeric(n_evals)

  delta_pid <- character(n_evals * n_comp)
  delta_exp_p <- numeric(n_evals * n_comp)
  delta_exp_d <- numeric(n_evals * n_comp)

  bench_a_num <- as.numeric(bench$a_vector)

  for (i in seq_len(n_evals)) {
    ev <- evals[[i]]
    delta_a <- ev$a_vector - bench$a_vector
    te_var_m <- max(0.0, as.numeric(crossprod(delta_a, Sigma_m %*% delta_a)))
    te_ann <- sqrt(12.0 * te_var_m)

    if (has_scen) {
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
      wh_vec <- w_vec_scen * h_vec_scen

      port_u <- as.numeric(u_mat %*% w_vec_scen)
      port_h <- port_u + as.numeric(z_mat %*% wh_vec)
      cma_ret <- prod(1.0 + port_h)^(1.0 / horizon) - 1.0
      cma_unh <- prod(1.0 + port_u)^(1.0 / horizon) - 1.0
      sum_cma_h[i] <- cma_ret
      sum_cma_u[i] <- cma_unh
      if (!is.na(cma_ret) && ev$metrics$vol_annualized > 1e-6) {
        sum_sharpe[i] <- cma_ret / ev$metrics$vol_annualized
      }
    }

    sum_pid[i] <- ev$portfolio_id
    sum_pname[i] <- ev$portfolio_name
    sum_hash[i] <- ev$config_hash
    sum_bccy[i] <- ev$base_currency
    sum_mid[i] <- ev$model_id
    sum_eq[i] <- ev$metrics$equity_share
    sum_fb[i] <- ev$metrics$foreign_exposure_before
    sum_hn[i] <- ev$metrics$total_hedge_notional
    sum_fa[i] <- ev$metrics$foreign_exposure_after
    sum_em[i] <- ev$metrics$emeq_exposure
    sum_vol_u[i] <- ev$metrics$vol_unhedged_annualized
    sum_vol_h[i] <- ev$metrics$vol_annualized
    sum_h_eff[i] <- ev$metrics$hedge_vol_effect_annualized
    sum_div[i] <- ev$metrics$diversification_ratio
    sum_te[i] <- te_ann
    sum_vdiff[i] <- ev$metrics$vol_annualized - bench$metrics$vol_annualized

    idx_range <- ((i - 1L) * n_comp + 1L):(i * n_comp)
    delta_pid[idx_range] <- ev$portfolio_id
    delta_exp_p[idx_range] <- as.numeric(ev$a_vector)
    delta_exp_d[idx_range] <- as.numeric(delta_a)

    if (i < n_evals) {
      for (j in (i + 1L):n_evals) {
        da_ij <- evals[[i]]$a_vector - evals[[j]]$a_vector
        te_ij <- sqrt(12.0 * max(0.0, as.numeric(crossprod(da_ij, Sigma_m %*% da_ij))))
        pairwise_te[i, j] <- te_ij
        pairwise_te[j, i] <- te_ij
      }
    }
  }

  summary_dt <- data.table::data.table(
    portfolio_id = sum_pid,
    portfolio_name = sum_pname,
    config_hash = sum_hash,
    base_currency = sum_bccy,
    model_id = sum_mid,
    equity_share = sum_eq,
    foreign_before = sum_fb,
    hedge_notional = sum_hn,
    foreign_after = sum_fa,
    emeq_exposure = sum_em,
    cma_return_hedged_ann = sum_cma_h,
    cma_return_unhedged_ann = sum_cma_u,
    vol_unhedged_ann = sum_vol_u,
    vol_hedged_ann = sum_vol_h,
    hedge_vol_effect_ann = sum_h_eff,
    cma_sharpe_ratio = sum_sharpe,
    diversification_ratio = sum_div,
    tracking_error_vs_bench_ann = sum_te,
    vol_diff_vs_bench_ann = sum_vdiff
  )

  delta_dt <- data.table::data.table(
    portfolio_id = delta_pid,
    benchmark_id = rep(bench$portfolio_id, n_evals * n_comp),
    component_id = rep(comp_ids, times = n_evals),
    exposure_portfolio = delta_exp_p,
    exposure_benchmark = rep(bench_a_num, times = n_evals),
    delta_exposure = delta_exp_d
  )

  # Fast cross-model sensitivity table using pre-computed exposure vectors a_vec and a_unhedged
  cross_model_dt <- data.table::data.table()
  if (!is.null(extra_models) && length(extra_models) > 0L) {
    valid_models <- Filter(function(m) isTRUE(m$valid), extra_models)
    n_vm <- length(valid_models)
    if (n_vm > 0L) {
      n_tot <- n_vm * n_evals
      cm_pid <- character(n_tot)
      cm_pname <- character(n_tot)
      cm_mid <- character(n_tot)
      cm_mlabel <- character(n_tot)
      cm_nobs <- integer(n_tot)
      cm_smonth <- character(n_tot)
      cm_emonth <- character(n_tot)
      cm_vol_h <- numeric(n_tot)
      cm_vol_u <- numeric(n_tot)

      k_idx <- 0L
      for (m_obj in valid_models) {
        S_m <- m_obj$Sigma_monthly
        m_cids <- m_obj$component_ids
        for (ev_i in evals) {
          k_idx <- k_idx + 1L
          a_v <- ev_i$a_vector[m_cids]
          a_u <- assemble_exposure_vector(ev_i$weights, NULL, m_cids)
          v_h <- sqrt(12.0 * max(0.0, as.numeric(crossprod(a_v, S_m %*% a_v))))
          v_u <- sqrt(12.0 * max(0.0, as.numeric(crossprod(a_u, S_m %*% a_u))))
          cm_pid[k_idx] <- ev_i$portfolio_id
          cm_pname[k_idx] <- ev_i$portfolio_name
          cm_mid[k_idx] <- m_obj$model_id
          cm_mlabel[k_idx] <- m_obj$label
          cm_nobs[k_idx] <- as.integer(m_obj$n_obs)
          cm_smonth[k_idx] <- as.character(m_obj$start_month)
          cm_emonth[k_idx] <- as.character(m_obj$end_month)
          cm_vol_h[k_idx] <- v_h
          cm_vol_u[k_idx] <- v_u
        }
      }
      cross_model_dt <- data.table::data.table(
        portfolio_id = cm_pid,
        portfolio_name = cm_pname,
        model_id = cm_mid,
        model_label = cm_mlabel,
        n_obs = cm_nobs,
        start_month = cm_smonth,
        end_month = cm_emonth,
        vol_annualized = cm_vol_h,
        vol_unhedged_annualized = cm_vol_u,
        hedge_vol_effect_annualized = cm_vol_h - cm_vol_u
      )
    }
  }

  list(
    valid = TRUE,
    benchmark_id = bench$portfolio_id,
    base_currency = model$base_currency,
    model_id = model$model_id,
    summary_table = summary_dt,
    delta_exposures = delta_dt,
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
