# ==============================================================================
# R/risk_diagnostics.R
# Covariance matrix diagnostics (PSD, eigenvalues, condition number, vol/corr)
# and documented spectral regularization
# Pure R module - no Shiny dependencies
# ==============================================================================

regularize_covariance <- function(Sigma, policy = list()) {
  min_eig_floor <- as.numeric(policy$min_eigenvalue_floor %||% 1e-10)
  sym_Sigma <- 0.5 * (Sigma + t(Sigma))
  eig <- eigen(sym_Sigma, symmetric = TRUE)
  vals <- eig$values
  vecs <- eig$vectors

  min_raw <- min(vals)
  regularized <- FALSE
  floor_applied <- 0.0

  if (min_raw < min_eig_floor) {
    regularized <- TRUE
    floor_applied <- min_eig_floor - min_raw
    vals_clipped <- pmax(vals, min_eig_floor)
    # Rescale trace so total variance is preserved after spectral floor
    raw_trace <- sum(diag(sym_Sigma))
    new_Sigma <- vecs %*% diag(vals_clipped, nrow = length(vals_clipped)) %*% t(vecs)
    if (raw_trace > 0 && sum(diag(new_Sigma)) > 0) {
      d_scale <- sqrt(pmax(diag(sym_Sigma), min_eig_floor) / diag(new_Sigma))
      new_Sigma <- diag(d_scale, nrow = length(d_scale)) %*% new_Sigma %*% diag(d_scale, nrow = length(d_scale))
    }
    sym_Sigma <- 0.5 * (new_Sigma + t(new_Sigma))
    rownames(sym_Sigma) <- rownames(Sigma)
    colnames(sym_Sigma) <- colnames(Sigma)
    eig <- eigen(sym_Sigma, symmetric = TRUE)
    vals <- eig$values
  }

  max_val <- max(vals)
  min_val <- min(vals)
  cond_num <- if (min_val > 0) max_val / min_val else Inf

  list(
    Sigma = sym_Sigma,
    eigenvalues = vals,
    min_eigenvalue_raw = min_raw,
    min_eigenvalue = min_val,
    max_eigenvalue = max_val,
    condition_number = cond_num,
    is_psd = min_val >= -1e-12,
    was_regularized = regularized,
    spectral_floor_applied = floor_applied
  )
}

cov_to_corr_safe <- function(Sigma) {
  d <- sqrt(pmax(diag(Sigma), 0))
  n <- length(d)
  corr <- matrix(0.0, nrow = n, ncol = n, dimnames = dimnames(Sigma))
  pos <- which(d > 0)
  if (length(pos) > 0L) {
    inv_d <- rep(0.0, n)
    inv_d[pos] <- 1.0 / d[pos]
    corr <- diag(inv_d, nrow = n) %*% Sigma %*% diag(inv_d, nrow = n)
    corr[pos, pos] <- pmin(pmax(corr[pos, pos], -1.0), 1.0)
    diag(corr)[pos] <- 1.0
  }
  rownames(corr) <- rownames(Sigma)
  colnames(corr) <- colnames(Sigma)
  corr
}

diagnose_covariance <- function(Sigma,
                                component_ids = colnames(Sigma),
                                n_obs = NA_integer_,
                                n_eff = NA_real_,
                                start_month = NA_character_,
                                end_month = NA_character_,
                                estimator = "SAMPLE",
                                target = "NONE",
                                shrinkage_intensity = 0.0,
                                regularization_info = NULL) {
  if (is.null(regularization_info)) {
    regularization_info <- regularize_covariance(Sigma)
  }
  S_clean <- regularization_info$Sigma
  vol_monthly <- sqrt(pmax(diag(S_clean), 0))
  vol_annualized <- vol_monthly * sqrt(12.0)
  corr_mat <- cov_to_corr_safe(S_clean)

  off_diag_corr <- corr_mat[upper.tri(corr_mat)]

  list(
    component_ids = component_ids,
    dimension = length(component_ids),
    n_obs = as.integer(n_obs),
    n_eff = as.numeric(n_eff %||% n_obs),
    start_month = start_month,
    end_month = end_month,
    estimator = estimator,
    target = target,
    shrinkage_intensity = as.numeric(shrinkage_intensity),
    vol_monthly = stats::setNames(as.numeric(vol_monthly), component_ids),
    vol_annualized = stats::setNames(as.numeric(vol_annualized), component_ids),
    correlation_matrix = corr_mat,
    avg_pairwise_correlation = if (length(off_diag_corr) > 0L) mean(off_diag_corr, na.rm = TRUE) else NA_real_,
    min_pairwise_correlation = if (length(off_diag_corr) > 0L) min(off_diag_corr, na.rm = TRUE) else NA_real_,
    max_pairwise_correlation = if (length(off_diag_corr) > 0L) max(off_diag_corr, na.rm = TRUE) else NA_real_,
    eigenvalues = as.numeric(regularization_info$eigenvalues),
    min_eigenvalue_raw = as.numeric(regularization_info$min_eigenvalue_raw),
    min_eigenvalue = as.numeric(regularization_info$min_eigenvalue),
    max_eigenvalue = as.numeric(regularization_info$max_eigenvalue),
    condition_number = as.numeric(regularization_info$condition_number),
    is_psd = isTRUE(regularization_info$is_psd),
    was_regularized = isTRUE(regularization_info$was_regularized),
    spectral_floor_applied = as.numeric(regularization_info$spectral_floor_applied)
  )
}
