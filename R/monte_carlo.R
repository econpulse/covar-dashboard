# ==============================================================================
# R/monte_carlo.R
# Forward-looking 5-year Monte Carlo risk simulation under explicit rebalancing
# rules, multivariate Student-t / Gaussian / Block-Bootstrap shocks, and
# empirical downside metrics (5Y-VaR, 5Y-CVaR, Max Drawdown, Shortfall Probability).
# Pure R module - no Shiny dependencies
# ==============================================================================

simulate_cma_paths <- function(expectations,
                               model,
                               weights,
                               hedge_config,
                               meta = NULL,
                               base_ccy = "CHF",
                               scenario_id = NULL,
                               rebalancing_rule = c("ANNUAL_REBALANCED", "MONTHLY_REBALANCED", "BUY_AND_HOLD"),
                               n_sims = 2000L,
                               horizon_years = 5L,
                               method = c("PARAMETRIC_STUDENT_T", "PARAMETRIC_GAUSSIAN", "BOOTSTRAP_MEAN_SHIFT"),
                               df_t = 6.0,
                               seed = 42L,
                               cost_bps_pa = 0.0,
                               carry_overrides = NULL,
                               fx_spot_overrides = NULL,
                               available_hedge_currencies = NULL,
                               target_cagr = 0.03,
                               basis = NULL) {
  rebalancing_rule <- match.arg(rebalancing_rule)
  method <- match.arg(method)

  n_sims <- as.integer(max(100L, min(10000L, n_sims %||% 2000L)))
  horizon_years <- as.integer(max(1L, min(10L, horizon_years %||% 5L)))
  T_months <- horizon_years * 12L
  df_t <- max(3.0, as.numeric(df_t %||% 6.0))
  target_cagr <- as.numeric(target_cagr %||% 0.0)

  issues <- list()

  # 1. Evaluate deterministic CMA baseline to obtain annual expected returns and currency triangulations
  scen_eval <- evaluate_return_scenarios(
    expectations = expectations,
    weights = weights,
    hedge_config = hedge_config,
    meta = meta,
    base_ccy = base_ccy,
    scenario_id = scenario_id,
    rebalancing_rule = rebalancing_rule,
    cost_bps_pa = cost_bps_pa,
    carry_overrides = carry_overrides,
    fx_spot_overrides = fx_spot_overrides,
    available_hedge_currencies = available_hedge_currencies
  )

  if (!isTRUE(scen_eval$valid)) {
    return(list(
      valid = FALSE,
      issues = scen_eval$issues
    ))
  }

  comp_ids <- model$component_ids
  asset_ids <- model$asset_ids
  Sigma_m <- model$Sigma_monthly
  p <- length(comp_ids)

  if (p < 1L || nrow(Sigma_m) != p || ncol(Sigma_m) != p) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "INVALID_COVARIANCE_DIMENSIONS",
      severity = "ERROR",
      message = "Dimensionen der Kovarianzmatrix stimmen nicht mit Komponenten-IDs überein.",
      scope = "MONTE_CARLO"
    )
    return(list(valid = FALSE, issues = issues))
  }

  # 2. Build aligned capital weights and hedge notionals
  w_full <- stats::setNames(numeric(length(asset_ids)), asset_ids)
  matched_w <- intersect(names(weights), asset_ids)
  w_full[matched_w] <- as.numeric(weights[matched_w])

  # Normalize weights if sum != 1.0 but > 0
  w_sum <- sum(w_full)
  if (w_sum > 1e-8 && abs(w_sum - 1.0) > 1e-6) {
    w_full <- w_full / w_sum
  }

  exp_pre <- derive_prehedge_exposures(
    weights = w_full,
    meta = meta,
    base_ccy = base_ccy,
    available_hedge_currencies = model$supported_hedge_currencies %||% model$available_hedge_currencies,
    fx_carry = model$latest_carry
  )
  hedges <- resolve_hedge_notionals(exp_pre, hedge_config = hedge_config)

  a_hedged <- assemble_exposure_vector(w_full, hedges$g_vector, comp_ids)
  a_unhedged <- assemble_exposure_vector(w_full, NULL, comp_ids)

  # 3. Assemble monthly forward expected drift matrix: horizon_years x p
  u_base_mat <- scen_eval$u_base_mat
  z_ccy_list <- scen_eval$z_ccy_list
  actual_horizon <- min(horizon_years, nrow(u_base_mat))

  mu_ann_mat <- matrix(0.0, nrow = horizon_years, ncol = p, dimnames = list(paste0("Y", seq_len(horizon_years)), comp_ids))
  for (y in seq_len(horizon_years)) {
    y_idx <- min(y, actual_horizon)
    for (aid in intersect(asset_ids, colnames(u_base_mat))) {
      mu_ann_mat[y, aid] <- u_base_mat[y_idx, aid]
    }
    for (cid in comp_ids) {
      if (startsWith(cid, "HEDGE_")) {
        ccy <- sub("^HEDGE_", "", cid)
        if (ccy %in% names(z_ccy_list)) {
          mu_ann_mat[y, cid] <- z_ccy_list[[ccy]]$z_base[y_idx]
        }
      }
    }
  }

  # Convert annual compound return factors to monthly compound returns: (1 + mu_ann)^(1/12) - 1
  mu_m_mat <- (1.0 + mu_ann_mat)^(1.0 / 12.0) - 1.0

  # 4. Generate stochastic shocks (preserving RNG state)
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(as.integer(seed))

  # Ensure Sigma_m is positive definite
  eig <- eigen(Sigma_m, symmetric = TRUE)
  if (any(eig$values < 1e-10)) {
    vals <- pmax(eig$values, 1e-8)
    Sigma_m <- eig$vectors %*% diag(vals, nrow = p, ncol = p) %*% t(eig$vectors)
    dimnames(Sigma_m) <- list(comp_ids, comp_ids)
  }
  L <- t(chol(Sigma_m))

  shocks_array <- array(0.0, dim = c(p, T_months, n_sims), dimnames = list(comp_ids, NULL, NULL))

  if (identical(method, "BOOTSTRAP_MEAN_SHIFT") && !is.null(basis) && !is.null(basis$X_mat)) {
    # Non-parametric stationary block-bootstrap centered and shifted to CMA drift
    X_hist <- basis$X_mat[, comp_ids, drop = FALSE]
    cc_rows <- which(rowSums(is.na(X_hist)) == 0L)
    if (length(cc_rows) >= 24L) {
      X_cc <- X_hist[cc_rows, , drop = FALSE]
      T_hist <- nrow(X_cc)
      hist_means <- colMeans(X_cc)
      X_centered <- sweep(X_cc, 2L, hist_means, "-")

      blk_len <- 6L
      n_blocks <- ceiling(T_months / blk_len)
      max_start <- max(1L, T_hist - blk_len + 1L)

      for (s in seq_len(n_sims)) {
        block_starts <- sample.int(max_start, size = n_blocks, replace = TRUE)
        idx_vec <- unlist(lapply(block_starts, function(st_idx) st_idx:(st_idx + blk_len - 1L)), use.names = FALSE)
        idx_vec <- idx_vec[seq_len(T_months)]
        shocks_array[, , s] <- t(X_centered[idx_vec, , drop = FALSE])
      }
    } else {
      method <- "PARAMETRIC_STUDENT_T"
      issues[[length(issues) + 1L]] <- make_issue(
        code = "BOOTSTRAP_FALLBACK_TO_STUDENT_T",
        severity = "WARNING",
        message = "Zu wenige vollständige historische Beobachtungen für Block-Bootstrap. Automatisch auf Student-t umgestellt.",
        scope = "MONTE_CARLO"
      )
    }
  }

  if (identical(method, "PARAMETRIC_STUDENT_T")) {
    # Multivariate Student-t with degrees of freedom df_t
    w_chi <- stats::rchisq(T_months * n_sims, df = df_t) / df_t
    scale_fac <- sqrt((df_t - 2.0) / df_t) / sqrt(w_chi)
    Z <- matrix(stats::rnorm(p * T_months * n_sims), nrow = p, ncol = T_months * n_sims)
    Z <- sweep(Z, 2L, scale_fac, "*")
    raw_shocks <- L %*% Z
    dim(raw_shocks) <- c(p, T_months, n_sims)
    shocks_array <- raw_shocks
  } else if (identical(method, "PARAMETRIC_GAUSSIAN")) {
    # Multivariate Gaussian (Normal)
    Z <- matrix(stats::rnorm(p * T_months * n_sims), nrow = p, ncol = T_months * n_sims)
    raw_shocks <- L %*% Z
    dim(raw_shocks) <- c(p, T_months, n_sims)
    shocks_array <- raw_shocks
  }

  # 5. Simulate Wealth Paths: 61 months (Month 0 to Month 60)
  # Wealth start at 100.0
  W_hedged_mat <- matrix(100.0, nrow = T_months + 1L, ncol = n_sims)
  W_unhedged_mat <- matrix(100.0, nrow = T_months + 1L, ncol = n_sims)

  n_assets <- length(asset_ids)
  hedge_ids <- setdiff(comp_ids, asset_ids)
  n_hedges <- length(hedge_ids)

  w_target <- w_full[asset_ids]
  g_target <- as.numeric(a_hedged[hedge_ids])
  names(g_target) <- hedge_ids

  if (identical(rebalancing_rule, "MONTHLY_REBALANCED")) {
    # Exact linear monthly rebalancing: r_P,m = a' X_m
    for (t in seq_len(T_months)) {
      y <- min(horizon_years, (t - 1L) %/% 12L + 1L)
      mu_t <- mu_m_mat[y, ]

      # Shocks at month t: p x n_sims
      X_t <- sweep(shocks_array[, t, , drop = FALSE], 1L, mu_t, "+")
      dim(X_t) <- c(p, n_sims)

      r_hedged_t <- as.numeric(crossprod(a_hedged, X_t))
      r_unhedged_t <- as.numeric(crossprod(a_unhedged, X_t))

      W_hedged_mat[t + 1L, ] <- W_hedged_mat[t, ] * (1.0 + r_hedged_t)
      W_unhedged_mat[t + 1L, ] <- W_unhedged_mat[t, ] * (1.0 + r_unhedged_t)
    }
  } else if (identical(rebalancing_rule, "BUY_AND_HOLD")) {
    # Continuous drift over all 60 months without rebalancing
    V_assets_h <- matrix(100.0 * w_target, nrow = n_assets, ncol = n_sims)
    V_assets_u <- matrix(100.0 * w_target, nrow = n_assets, ncol = n_sims)
    G_notional <- matrix(100.0 * g_target, nrow = n_hedges, ncol = n_sims)
    cum_h_pnl <- numeric(n_sims)

    for (t in seq_len(T_months)) {
      y <- min(horizon_years, (t - 1L) %/% 12L + 1L)
      mu_t <- mu_m_mat[y, ]

      X_t <- sweep(shocks_array[, t, , drop = FALSE], 1L, mu_t, "+")
      dim(X_t) <- c(p, n_sims)

      R_assets <- X_t[seq_len(n_assets), , drop = FALSE]
      R_hedges <- if (n_hedges > 0L) X_t[(n_assets + 1L):p, , drop = FALSE] else matrix(0.0, 0L, n_sims)

      V_assets_h <- V_assets_h * (1.0 + R_assets)
      V_assets_u <- V_assets_u * (1.0 + R_assets)

      if (n_hedges > 0L) {
        h_pnl <- colSums(G_notional * R_hedges)
        cum_h_pnl <- cum_h_pnl + h_pnl
      }

      W_hedged_mat[t + 1L, ] <- colSums(V_assets_h) + cum_h_pnl
      W_unhedged_mat[t + 1L, ] <- colSums(V_assets_u)
    }
  } else {
    # Standard ANNUAL_REBALANCED: Weights drift within year, reset to target at month 12, 24, 36, 48
    V_assets_h <- matrix(100.0 * w_target, nrow = n_assets, ncol = n_sims)
    V_assets_u <- matrix(100.0 * w_target, nrow = n_assets, ncol = n_sims)
    G_notional <- matrix(100.0 * g_target, nrow = n_hedges, ncol = n_sims)
    cum_h_pnl_year <- numeric(n_sims)

    for (t in seq_len(T_months)) {
      y <- min(horizon_years, (t - 1L) %/% 12L + 1L)
      y_month <- (t - 1L) %% 12L + 1L
      mu_t <- mu_m_mat[y, ]

      X_t <- sweep(shocks_array[, t, , drop = FALSE], 1L, mu_t, "+")
      dim(X_t) <- c(p, n_sims)

      R_assets <- X_t[seq_len(n_assets), , drop = FALSE]
      R_hedges <- if (n_hedges > 0L) X_t[(n_assets + 1L):p, , drop = FALSE] else matrix(0.0, 0L, n_sims)

      V_assets_h <- V_assets_h * (1.0 + R_assets)
      V_assets_u <- V_assets_u * (1.0 + R_assets)

      if (n_hedges > 0L) {
        h_pnl <- colSums(G_notional * R_hedges)
        cum_h_pnl_year <- cum_h_pnl_year + h_pnl
      }

      tot_h <- colSums(V_assets_h) + cum_h_pnl_year
      tot_u <- colSums(V_assets_u)

      W_hedged_mat[t + 1L, ] <- tot_h
      W_unhedged_mat[t + 1L, ] <- tot_u

      # Annual reset at month 12, 24, 36, 48
      if (y_month == 12L && t < T_months) {
        V_assets_h <- tcrossprod(w_target, tot_h)
        V_assets_u <- tcrossprod(w_target, tot_u)
        G_notional <- tcrossprod(g_target, tot_h)
        cum_h_pnl_year <- numeric(n_sims)
      }
    }
  }

  # 6. Empirical Downside and Distribution Metrics
  cagr_hedged <- (W_hedged_mat[T_months + 1L, ] / 100.0)^(1.0 / horizon_years) - 1.0
  cagr_unhedged <- (W_unhedged_mat[T_months + 1L, ] / 100.0)^(1.0 / horizon_years) - 1.0

  probs <- c(0.05, 0.25, 0.50, 0.75, 0.95)

  # Max Drawdown per simulation path
  calc_max_dd <- function(W_mat) {
    running_max <- apply(W_mat, 2L, cummax)
    dd_mat <- (W_mat - running_max) / running_max
    apply(dd_mat, 2L, min)
  }
  max_dd_hedged <- calc_max_dd(W_hedged_mat)
  max_dd_unhedged <- calc_max_dd(W_unhedged_mat)

  build_metrics_block <- function(cagr_vec, max_dd_vec, end_wealth) {
    q_cagr <- stats::quantile(cagr_vec, probs = probs, na.rm = TRUE)
    q_dd <- stats::quantile(max_dd_vec, probs = probs, na.rm = TRUE)
    q_wealth <- stats::quantile(end_wealth, probs = probs, na.rm = TRUE)

    # 5Y-VaR (95% confidence level): loss relative to 0% CAGR
    var_95_cagr <- -as.numeric(q_cagr[1L])
    # 5Y-Expected Shortfall (CVaR 95%): average CAGR in worst 5% of cases
    worst_05_cagr <- cagr_vec[cagr_vec <= q_cagr[1L]]
    cvar_95_cagr <- if (length(worst_05_cagr) > 0L) -mean(worst_05_cagr) else var_95_cagr

    # Shortfall Probability: P(W_60 < 100)
    shortfall_prob <- mean(end_wealth < 100.0)
    prob_target <- mean(cagr_vec >= target_cagr)

    list(
      cagr_p05 = as.numeric(q_cagr[1L]),
      cagr_p25 = as.numeric(q_cagr[2L]),
      cagr_p50 = as.numeric(q_cagr[3L]),
      cagr_p75 = as.numeric(q_cagr[4L]),
      cagr_p95 = as.numeric(q_cagr[5L]),
      cagr_mean = mean(cagr_vec),
      cagr_sd = stats::sd(cagr_vec),
      var_95_cagr = var_95_cagr,
      cvar_95_cagr = cvar_95_cagr,
      wealth_p05 = as.numeric(q_wealth[1L]),
      wealth_p50 = as.numeric(q_wealth[3L]),
      wealth_p95 = as.numeric(q_wealth[5L]),
      max_drawdown_median = as.numeric(q_dd[3L]),
      max_drawdown_p05 = as.numeric(q_dd[1L]),
      shortfall_prob = shortfall_prob,
      prob_achieve_target = prob_target,
      target_cagr = target_cagr
    )
  }

  metrics_hedged <- build_metrics_block(cagr_hedged, max_dd_hedged, W_hedged_mat[T_months + 1L, ])
  metrics_unhedged <- build_metrics_block(cagr_unhedged, max_dd_unhedged, W_unhedged_mat[T_months + 1L, ])

  # 7. Monthly Fan Chart Paths (Month 0 to Month 60)
  # Deterministic reference path from scenario evaluation (interpolated to months)
  det_yearly <- scen_eval$yearly_path
  det_hedged_monthly <- numeric(T_months + 1L)
  det_unhedged_monthly <- numeric(T_months + 1L)

  # Interpolate deterministic yearly wealth to 61 months
  det_hedged_monthly[1L] <- 100.0
  det_unhedged_monthly[1L] <- 100.0
  for (y in seq_len(horizon_years)) {
    w_h_start <- det_yearly$wealth_hedged_base[y]
    w_h_end <- det_yearly$wealth_hedged_base[y + 1L]
    w_u_start <- det_yearly$wealth_unhedged_base[y]
    w_u_end <- det_yearly$wealth_unhedged_base[y + 1L]

    for (m in seq_len(12L)) {
      idx <- (y - 1L) * 12L + m + 1L
      frac <- m / 12.0
      det_hedged_monthly[idx] <- w_h_start * (w_h_end / w_h_start)^frac
      det_unhedged_monthly[idx] <- w_u_start * (w_u_end / w_u_start)^frac
    }
  }

  fan_rows <- vector("list", T_months + 1L)
  for (t in 0:T_months) {
    q_h <- stats::quantile(W_hedged_mat[t + 1L, ], probs = probs, na.rm = TRUE)
    q_u <- stats::quantile(W_unhedged_mat[t + 1L, ], probs = probs, na.rm = TRUE)

    fan_rows[[t + 1L]] <- list(
      month = t,
      year = round(t / 12.0, 2),
      label = if (t == 0L) "Start" else if (t %% 12L == 0L) paste0("J", t %/% 12L) else paste0("M", t),
      hedged_p05 = round(as.numeric(q_h[1L]), 2),
      hedged_p25 = round(as.numeric(q_h[2L]), 2),
      hedged_p50 = round(as.numeric(q_h[3L]), 2),
      hedged_p75 = round(as.numeric(q_h[4L]), 2),
      hedged_p95 = round(as.numeric(q_h[5L]), 2),
      hedged_mean = round(mean(W_hedged_mat[t + 1L, ]), 2),
      unhedged_p50 = round(as.numeric(q_u[3L]), 2),
      unhedged_p05 = round(as.numeric(q_u[1L]), 2),
      unhedged_p95 = round(as.numeric(q_u[5L]), 2),
      deterministic_hedged = round(det_hedged_monthly[t + 1L], 2),
      deterministic_unhedged = round(det_unhedged_monthly[t + 1L], 2)
    )
  }

  # 8. Histogram / Density Distribution of 5Y CAGR
  build_histogram <- function(vec, n_bins = 25L) {
    min_v <- min(vec, na.rm = TRUE)
    max_v <- max(vec, na.rm = TRUE)
    if (abs(max_v - min_v) < 1e-8) {
      min_v <- min_v - 0.01
      max_v <- max_v + 0.01
    }
    span <- max_v - min_v
    breaks <- seq(from = min_v - span * 1e-4, to = max_v + span * 1e-4, length.out = n_bins + 1L)
    h <- graphics::hist(vec, breaks = breaks, plot = FALSE)
    data.table::data.table(
      mid = round(h$mids, 4),
      count = h$counts,
      density = round(h$density, 4),
      pct = round(h$counts / sum(h$counts) * 100.0, 2)
    )
  }

  cagr_hist_hedged <- build_histogram(cagr_hedged, n_bins = 25L)
  cagr_hist_unhedged <- build_histogram(cagr_unhedged, n_bins = 25L)

  list(
    valid = TRUE,
    scenario_id = scen_eval$scenario_id,
    scenario_label = scen_eval$scenario_label,
    base_currency = base_ccy,
    n_sims = n_sims,
    horizon_years = horizon_years,
    horizon_months = T_months,
    method = method,
    rebalancing_rule = rebalancing_rule,
    df_t = if (identical(method, "PARAMETRIC_STUDENT_T")) df_t else NULL,
    seed = as.integer(seed),
    metrics_hedged = metrics_hedged,
    metrics_unhedged = metrics_unhedged,
    fan_chart_path = fan_rows,
    cagr_histogram_hedged = cagr_hist_hedged,
    cagr_histogram_unhedged = cagr_hist_unhedged,
    issues = c(scen_eval$issues, issues)
  )
}
