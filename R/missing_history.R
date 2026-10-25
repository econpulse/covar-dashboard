# ==============================================================================
# R/missing_history.R
# Direct joint factor-moment estimation (Stambaugh / conditional moment
# reconstruction) for short-history series without fabricating fake time series
# Pure R module - no Shiny dependencies
# ==============================================================================

reconstruct_factor_moments <- function(sample, settings = list()) {
  issues <- sample$issues %||% list()
  X_full <- sample$X_full
  comp_ids <- sample$component_ids
  asset_ids <- sample$asset_ids
  month_keys <- sample$all_month_keys

  # Keep rows where at least the core long-history components are observed
  non_na_counts <- colSums(!is.na(X_full))
  max_obs <- max(non_na_counts)

  # Define long-history donors as components with >= 95% of max_obs and complete on their common span
  donor_candidates <- names(non_na_counts)[non_na_counts >= floor(0.95 * max_obs)]
  donor_complete_rows <- which(rowSums(is.na(X_full[, donor_candidates, drop = FALSE])) == 0L)

  if (length(donor_complete_rows) < 24L) {
    # Fallback to complete-case if donor span is too short
    cc_rows <- which(rowSums(is.na(X_full)) == 0L)
    X_cc <- X_full[cc_rows, comp_ids, drop = FALSE]
    S_cc <- stats::cov(X_cc)
    return(list(
      Sigma = S_cc,
      mean_vector = colMeans(X_cc),
      n_obs_long = nrow(X_cc),
      n_obs_common = nrow(X_cc),
      start_month = month_keys[min(cc_rows)],
      end_month = month_keys[max(cc_rows)],
      donor_ids = comp_ids,
      short_ids = character(0),
      factor_audit = data.table::data.table(),
      model_supported_share = 0.0,
      issues = issues
    ))
  }

  X_sub <- X_full[donor_complete_rows, comp_ids, drop = FALSE]
  sub_months <- month_keys[donor_complete_rows]
  T_long <- nrow(X_sub)

  # Identify components fully observed on donor_complete_rows vs short-history components
  missing_in_sub <- colSums(is.na(X_sub))
  donor_ids <- names(missing_in_sub)[missing_in_sub == 0L]
  short_ids <- names(missing_in_sub)[missing_in_sub > 0L]

  # Economically admissible donor mapping for known asset types (with automatic fallback)
  preferred_donors_map <- list(
    "SPCHICC(RI)" = c("BMSW10Y(RI)", "MSSWITL(RI)", "BMUS10Y(RI)"),
    "SWZKBIM(RI)" = c("MSSWITL(RI)", "BMSW10Y(RI)"),
    "DJUBSTR"     = c("GSGCTOT", "MSUSAM$(RI)", "MSEMKF$(RI)"),
    "SWISSMI(RI)" = c("MSSWITL(RI)"),
    "SWSPIXD"     = c("MSSWITL(RI)")
  )

  # Estimate long-run donor moments (with optional constant-correlation shrinkage)
  F_mat <- X_sub[, donor_ids, drop = FALSE]
  mu_F <- colMeans(F_mat)
  Sigma_FF_raw <- stats::cov(F_mat)
  sh_F <- shrink_cov_constant_correlation(F_mat)
  Sigma_FF <- sh_F$Sigma

  if (length(short_ids) == 0L) {
    return(list(
      Sigma = Sigma_FF[comp_ids, comp_ids, drop = FALSE],
      mean_vector = mu_F[comp_ids],
      n_obs_long = T_long,
      n_obs_common = T_long,
      start_month = sub_months[1L],
      end_month = sub_months[length(sub_months)],
      donor_ids = donor_ids,
      short_ids = character(0),
      factor_audit = data.table::data.table(),
      model_supported_share = 0.0,
      shrinkage_intensity = sh_F$lambda,
      issues = issues
    ))
  }

  n_donors <- length(donor_ids)
  n_short <- length(short_ids)
  B_mat <- matrix(0.0, nrow = n_short, ncol = n_donors, dimnames = list(short_ids, donor_ids))
  alpha_vec <- stats::setNames(numeric(n_short), short_ids)
  resid_mat <- matrix(NA_real_, nrow = T_long, ncol = n_short, dimnames = list(sub_months, short_ids))
  resid_var <- stats::setNames(numeric(n_short), short_ids)
  audit_rows <- vector("list", n_short)

  for (j in seq_along(short_ids)) {
    sid <- short_ids[j]
    y_vec <- X_sub[, sid]
    obs_idx <- which(!is.na(y_vec))
    T_obs <- length(obs_idx)

    cand_donors <- intersect(preferred_donors_map[[sid]] %||% intersect(donor_ids, asset_ids), donor_ids)
    if (length(cand_donors) == 0L) {
      cand_donors <- donor_ids[seq_len(min(3L, length(donor_ids)))]
    }

    F_obs <- F_mat[obs_idx, cand_donors, drop = FALSE]
    y_obs <- y_vec[obs_idx]

    # Center on overlap window and apply light ridge stabilization to prevent overfitting
    y_bar <- mean(y_obs)
    F_bar <- colMeans(F_obs)
    F_c <- sweep(F_obs, 2L, F_bar, "-")
    y_c <- y_obs - y_bar

    k_d <- ncol(F_c)
    XtX <- crossprod(F_c)
    ridge_pen <- 1e-4 * mean(diag(XtX))
    beta_sub <- tryCatch(
      as.numeric(solve(XtX + diag(ridge_pen, nrow = k_d), crossprod(F_c, y_c))),
      error = function(e) rep(0.0, k_d)
    )
    names(beta_sub) <- cand_donors

    alpha_j <- y_bar - sum(beta_sub * F_bar)
    y_hat_obs <- alpha_j + as.numeric(F_obs %*% beta_sub)
    eps_obs <- y_obs - y_hat_obs

    dof <- max(1L, T_obs - k_d - 1L)
    s2_eps <- sum(eps_obs^2) / dof
    tss <- sum(y_c^2)
    r_sq <- if (tss > 0) max(0.0, 1.0 - sum(eps_obs^2) / tss) else 0.0

    B_mat[sid, cand_donors] <- beta_sub
    alpha_vec[sid] <- alpha_j
    resid_mat[obs_idx, sid] <- eps_obs
    resid_var[sid] <- s2_eps

    audit_rows[[j]] <- data.table::data.table(
      series_code = sid,
      true_obs = T_obs,
      donor_obs = T_long,
      reconstructed_months = T_long - T_obs,
      overlap_start = sub_months[min(obs_idx)],
      overlap_end = sub_months[max(obs_idx)],
      donors = paste(cand_donors, collapse = ", "),
      betas = paste(sprintf("%s=%.3f", cand_donors, beta_sub), collapse = "; "),
      r_squared = r_sq,
      residual_vol_ann = sqrt(s2_eps * 12.0)
    )
  }

  # Estimate joint residual covariance Sigma_eps on pairwise overlap, shrunk toward diagonal
  Sigma_eps <- diag(resid_var, nrow = n_short, ncol = n_short)
  rownames(Sigma_eps) <- short_ids
  colnames(Sigma_eps) <- short_ids

  if (n_short > 1L) {
    for (i in 1:(n_short - 1L)) {
      for (k in (i + 1L):n_short) {
        ov_idx <- which(!is.na(resid_mat[, i]) & !is.na(resid_mat[, k]))
        if (length(ov_idx) >= 24L) {
          ri <- resid_mat[ov_idx, i]
          rk <- resid_mat[ov_idx, k]
          rho_ik <- stats::cor(ri, rk)
          if (!is.na(rho_ik)) {
            # Shrink residual correlation by overlap ratio (n_ov / T_long) * 0.5 for PSD stability
            shrunk_rho <- 0.5 * (length(ov_idx) / T_long) * max(min(rho_ik, 0.90), -0.90)
            cov_ik <- shrunk_rho * sqrt(resid_var[i] * resid_var[k])
            Sigma_eps[i, k] <- cov_ik
            Sigma_eps[k, i] <- cov_ik
          }
        }
      }
    }
    Sigma_eps <- regularize_covariance(Sigma_eps)$Sigma
  }

  # Assemble full joint covariance matrix preserving PSD block structure:
  # Sigma_FF = Sigma_FF
  # Sigma_YF = B %*% Sigma_FF
  # Sigma_YY = B %*% Sigma_FF %*% t(B) + Sigma_eps
  Sigma_YF <- B_mat %*% Sigma_FF
  Sigma_YY <- B_mat %*% Sigma_FF %*% t(B_mat) + Sigma_eps

  all_ordered <- c(donor_ids, short_ids)
  Sigma_joint <- rbind(
    cbind(Sigma_FF, t(Sigma_YF)),
    cbind(Sigma_YF, Sigma_YY)
  )
  rownames(Sigma_joint) <- all_ordered
  colnames(Sigma_joint) <- all_ordered

  # Reorder to exact component_ids order
  Sigma_final <- Sigma_joint[comp_ids, comp_ids, drop = FALSE]
  mu_Y <- alpha_vec + as.numeric(B_mat %*% mu_F)
  names(mu_Y) <- short_ids
  mu_final <- c(mu_F, mu_Y)[comp_ids]

  total_cells <- T_long * length(comp_ids)
  missing_cells <- sum(missing_in_sub)
  model_share <- missing_cells / total_cells

  issues[[length(issues) + 1L]] <- make_issue(
    code = "FACTOR_MOMENT_RECONSTRUCTION_ACTIVE",
    severity = "INFO",
    message = sprintf(
      "Faktor-Momentenschätzung für %d kürzere Reihe(n) (%s) aktiv. Echte Beobachtungen bleiben separat ausgewiesen (Modellanteil: %.1f%%).",
      n_short, paste(short_ids, collapse = ", "), 100.0 * model_share
    ),
    scope = "ESTIMATION",
    details = list(short_ids = short_ids, model_share = model_share)
  )

  list(
    Sigma = Sigma_final,
    mean_vector = mu_final,
    n_obs_long = T_long,
    n_obs_common = sum(rowSums(is.na(X_sub)) == 0L),
    start_month = sub_months[1L],
    end_month = sub_months[length(sub_months)],
    donor_ids = donor_ids,
    short_ids = short_ids,
    factor_audit = data.table::rbindlist(audit_rows, fill = TRUE),
    model_supported_share = model_share,
    shrinkage_intensity = sh_F$lambda,
    issues = issues
  )
}
