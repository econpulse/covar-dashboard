# ==============================================================================
# R/covariance_estimators.R
# Sample covariance, Ledoit-Wolf constant-correlation/diagonal shrinkage,
# EWMA with documented regularization, crisis stress, and factor completion
# Pure R module - no Shiny dependencies
# ==============================================================================

shrink_cov_constant_correlation <- function(X, target_type = c("CONSTANT_CORRELATION", "DIAGONAL")) {
  target_type <- match.arg(target_type)
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  comp_ids <- colnames(X)

  if (n < 2L) {
    stop("At least 2 observations required for covariance estimation", call. = FALSE)
  }

  # Unbiased sample covariance S
  X_c <- sweep(X, 2L, colMeans(X), "-")
  S <- crossprod(X_c) / (n - 1.0)
  rownames(S) <- comp_ids
  colnames(S) <- comp_ids

  if (p == 1L) {
    return(list(Sigma = S, target = S, lambda = 0.0, target_type = target_type))
  }

  var_vec <- pmax(diag(S), 0)
  sd_vec <- sqrt(var_vec)
  corr_mat <- cov_to_corr_safe(S)

  if (identical(target_type, "DIAGONAL")) {
    F_target <- diag(var_vec, nrow = p, ncol = p)
    r_bar <- 0.0
  } else {
    # Ledoit-Wolf (2004) constant-correlation target
    r_bar <- sum(corr_mat[upper.tri(corr_mat)]) * (2.0 / (p * (p - 1.0)))
    r_bar <- max(min(r_bar, 0.99), -1.0 / (p - 1.0) + 1e-4)
    F_target <- r_bar * tcrossprod(sd_vec)
    diag(F_target) <- var_vec
  }
  rownames(F_target) <- comp_ids
  colnames(F_target) <- comp_ids

  # Compute optimal analytical shrinkage intensity lambda in [0, 1]
  # Following Ledoit & Wolf (2004) / Schaefer & Strimmer (2005) on standardized data
  pos_sd <- ifelse(sd_vec > 0, sd_vec, 1.0)
  Z <- sweep(X_c, 2L, pos_sd, "/")
  R_sample <- crossprod(Z) / (n - 1.0)

  # Variance of sample correlations
  Z2 <- Z^2
  M_second <- crossprod(Z2) / n
  Var_R <- (n / ((n - 1.0)^2)) * (M_second - (R_sample^2) * ((n - 1.0)^2 / (n^2)))
  diag(Var_R) <- 0.0

  if (identical(target_type, "DIAGONAL")) {
    Target_R <- diag(1.0, nrow = p, ncol = p)
  } else {
    Target_R <- matrix(r_bar, nrow = p, ncol = p)
    diag(Target_R) <- 1.0
  }

  num <- sum(Var_R)
  den <- sum((R_sample - Target_R)^2)
  lambda <- if (den > 1e-16) min(1.0, max(0.0, num / den)) else 0.0

  Sigma_shrunk <- (1.0 - lambda) * S + lambda * F_target
  rownames(Sigma_shrunk) <- comp_ids
  colnames(Sigma_shrunk) <- comp_ids

  list(
    Sigma = Sigma_shrunk,
    sample_Sigma = S,
    target = F_target,
    lambda = as.numeric(lambda),
    avg_correlation = as.numeric(r_bar),
    target_type = target_type
  )
}

estimate_ewma_covariance <- function(X,
                                     halflife_months = 24,
                                     max_window_months = 120,
                                     regularization_gamma = 0.05) {
  X <- as.matrix(X)
  if (!is.null(max_window_months) && is.numeric(max_window_months) && nrow(X) > max_window_months) {
    X <- utils::tail(X, as.integer(max_window_months))
  }

  T_obs <- nrow(X)
  p <- ncol(X)
  comp_ids <- colnames(X)

  hl <- max(2.0, as.numeric(halflife_months %||% 24.0))
  gamma_reg <- min(1.0, max(0.0, as.numeric(regularization_gamma %||% 0.05)))
  decay <- 0.5^(1.0 / hl)

  # Exponential weights: most recent observation T_obs gets highest weight
  lags <- seq(from = T_obs - 1L, to = 0L, by = -1L)
  raw_w <- decay^lags
  w <- raw_w / sum(raw_w)

  # Effective observation count N_eff = 1 / sum(w^2)
  n_eff <- 1.0 / sum(w^2)

  # Weighted mean and unbiased weighted covariance
  mu_w <- as.numeric(crossprod(w, X))
  X_c <- sweep(X, 2L, mu_w, "-")
  denom <- 1.0 - sum(w^2)
  S_ewma <- if (denom > 1e-8) {
    crossprod(X_c * sqrt(w)) / denom
  } else {
    crossprod(X_c * sqrt(w))
  }

  # Documented diagonal ridge/target regularization (do not apply i.i.d. Ledoit-Wolf to EWMA!)
  D_target <- diag(diag(S_ewma), nrow = p, ncol = p)
  Sigma_reg <- (1.0 - gamma_reg) * S_ewma + gamma_reg * D_target
  rownames(Sigma_reg) <- comp_ids
  colnames(Sigma_reg) <- comp_ids

  list(
    Sigma = Sigma_reg,
    raw_ewma_Sigma = S_ewma,
    target = D_target,
    halflife_months = hl,
    decay_factor = decay,
    regularization_gamma = gamma_reg,
    n_obs = T_obs,
    n_eff = n_eff,
    mean_vector = stats::setNames(mu_w, comp_ids)
  )
}

estimate_covariance <- function(sample,
                                method = c("SHRINKAGE", "SAMPLE", "EWMA", "FACTOR_COMPLETED", "CRISIS_STRESS"),
                                settings = list(),
                                model_id = NULL,
                                label = NULL) {
  method <- match.arg(toupper(method), c("SHRINKAGE", "SAMPLE", "EWMA", "FACTOR_COMPLETED", "CRISIS_STRESS"))
  issues <- sample$issues %||% list()
  comp_ids <- sample$component_ids

  X_mat <- sample$X_sample
  n_obs <- nrow(X_mat)
  n_eff <- as.numeric(n_obs)
  start_m <- sample$start_month
  end_m <- sample$end_month
  target_desc <- "NONE"
  shrink_lambda <- 0.0
  model_share <- 0.0
  factor_audit <- data.table::data.table()
  mean_vec <- stats::setNames(rep(NA_real_, length(comp_ids)), comp_ids)

  if (identical(method, "SAMPLE")) {
    Sigma_est <- stats::cov(X_mat)
    mean_vec <- colMeans(X_mat)
    target_desc <- "NONE (Unbiased Sample Covariance)"
  } else if (identical(method, "SHRINKAGE")) {
    t_type <- toupper(settings$shrinkage_target %||% "CONSTANT_CORRELATION")
    if (!(t_type %in% c("CONSTANT_CORRELATION", "DIAGONAL"))) {
      t_type <- "CONSTANT_CORRELATION"
    }
    sh_res <- shrink_cov_constant_correlation(X_mat, target_type = t_type)
    Sigma_est <- sh_res$Sigma
    mean_vec <- colMeans(X_mat)
    shrink_lambda <- sh_res$lambda
    target_desc <- sprintf("LEDOIT_WOLF_%s (r_bar=%.3f)", t_type, sh_res$avg_correlation)
  } else if (identical(method, "EWMA")) {
    hl <- as.numeric(settings$halflife_months %||% 24.0)
    max_w <- as.integer(settings$window_months %||% 120L)
    gamma_reg <- as.numeric(settings$regularization_gamma %||% 0.05)
    ew_res <- estimate_ewma_covariance(
      X = X_mat,
      halflife_months = hl,
      max_window_months = max_w,
      regularization_gamma = gamma_reg
    )
    Sigma_est <- ew_res$Sigma
    mean_vec <- ew_res$mean_vector
    n_obs <- ew_res$n_obs
    n_eff <- ew_res$n_eff
    shrink_lambda <- gamma_reg
    target_desc <- sprintf("DIAGONAL_VARIANCE_RIDGE (HL=%dM, gamma=%.2f)", as.integer(hl), gamma_reg)
  } else if (identical(method, "FACTOR_COMPLETED")) {
    fac_res <- reconstruct_factor_moments(sample, settings = settings)
    Sigma_est <- fac_res$Sigma
    mean_vec <- fac_res$mean_vector
    n_obs <- fac_res$n_obs_long
    n_eff <- as.numeric(fac_res$n_obs_common)
    start_m <- fac_res$start_month
    end_m <- fac_res$end_month
    shrink_lambda <- fac_res$shrinkage_intensity %||% 0.0
    model_share <- fac_res$model_supported_share
    factor_audit <- fac_res$factor_audit
    issues <- fac_res$issues
    target_desc <- "FACTOR_MOMENT_RECONSTRUCTION_PLUS_CC_SHRINKAGE"
  } else if (identical(method, "CRISIS_STRESS")) {
    gamma_reg <- as.numeric(settings$regularization_gamma %||% 0.05)
    S_raw <- stats::cov(X_mat)
    D_target <- diag(diag(S_raw), nrow = ncol(X_mat), ncol = ncol(X_mat))
    Sigma_est <- (1.0 - gamma_reg) * S_raw + gamma_reg * D_target
    rownames(Sigma_est) <- comp_ids
    colnames(Sigma_est) <- comp_ids
    mean_vec <- colMeans(X_mat)
    shrink_lambda <- gamma_reg
    target_desc <- sprintf("OBSERVED_CRISIS_WINDOW_DIAGONAL_REG (N=%d, gamma=%.2f)", n_obs, gamma_reg)
  }

  reg_info <- regularize_covariance(Sigma_est, policy = settings)
  Sigma_clean <- reg_info$Sigma
  Sigma_annualized <- 12.0 * Sigma_clean

  diag_info <- diagnose_covariance(
    Sigma = Sigma_clean,
    component_ids = comp_ids,
    n_obs = n_obs,
    n_eff = n_eff,
    start_month = start_m,
    end_month = end_m,
    estimator = method,
    target = target_desc,
    shrinkage_intensity = shrink_lambda,
    regularization_info = reg_info
  )

  model_hash <- compute_object_hash(list(
    basis_id = sample$basis_id,
    base_currency = sample$base_currency,
    return_mode = sample$return_mode,
    method = method,
    start_month = start_m,
    end_month = end_m,
    n_obs = n_obs,
    component_ids = comp_ids,
    Sigma = Sigma_clean
  ))

  out <- list(
    valid = TRUE,
    model_id = model_id %||% tolower(method),
    label = label %||% method,
    model_hash = substr(model_hash, 1L, 16L),
    basis_id = sample$basis_id,
    base_currency = sample$base_currency,
    return_mode = sample$return_mode,
    estimator = method,
    target = target_desc,
    shrinkage_intensity = shrink_lambda,
    model_supported_share = model_share,
    n_obs = as.integer(n_obs),
    n_eff = as.numeric(n_eff),
    start_month = start_m,
    end_month = end_m,
    unhedged_common_start = sample$unhedged_common_start,
    hedged_common_start = sample$hedged_common_start,
    asset_ids = sample$asset_ids,
    hedge_ids = sample$hedge_ids,
    component_ids = comp_ids,
    required_hedge_currencies = sample$required_hedge_currencies,
    available_hedge_currencies = sample$available_hedge_currencies,
    supported_hedge_currencies = sample$supported_hedge_currencies %||% sample$available_hedge_currencies,
    missing_hedge_currencies = sample$missing_hedge_currencies,
    component_availability = sample$component_availability,
    Sigma_monthly = Sigma_clean,
    Sigma_annualized = Sigma_annualized,
    correlation_matrix = diag_info$correlation_matrix,
    vol_monthly = diag_info$vol_monthly,
    vol_annualized = diag_info$vol_annualized,
    mean_monthly = mean_vec,
    latest_carry = sample$latest_carry,
    diagnostics = diag_info,
    factor_audit = factor_audit,
    issues = issues
  )
  class(out) <- c("maps_covariance_model", "list")
  out
}
