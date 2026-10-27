# ==============================================================================
# R/missing_history.R
# Direct joint factor-moment estimation (Stambaugh / conditional moment
# reconstruction) for short-history series without fabricating fake time series
# Pure R module - no Shiny dependencies
# ==============================================================================

#' Automatic parsimonious factor donor selection via forward stepwise BIC
#'
#' Evaluates candidate donors for a short target series, selecting the most
#' informative donors while controlling multi-collinearity and preventing
#' in-sample overfitting through Bayesian Information Criterion (BIC).
#'
#' @param y_obs Vector of observed returns for target series on overlap window
#' @param F_obs Matrix of observed returns for candidate donors on overlap window
#' @param max_factors Maximum number of factors to select (default: 3)
#' @param min_delta_bic Minimum reduction in BIC required to add another factor (default: 2.0)
#' @param max_collinearity Maximum pairwise correlation allowed between selected donors (default: 0.95)
#' @return Character vector of selected donor column names
select_best_donors <- function(y_obs,
                               F_obs,
                               max_factors = 3L,
                               min_delta_bic = 2.0,
                               max_collinearity = 0.95) {
  cand_names <- colnames(F_obs)
  n <- length(y_obs)
  if (length(cand_names) == 0L || n < 10L) {
    return(character(0))
  }

  y_bar <- mean(y_obs)
  tss <- sum((y_obs - y_bar)^2)
  if (tss <= 1e-12) {
    return(character(0))
  }

  # Step 0: Base null model (intercept only)
  best_bic <- n * log(tss / n)
  selected <- character(0)

  for (step in seq_len(max_factors)) {
    remaining <- setdiff(cand_names, selected)
    if (length(remaining) == 0L) break

    step_best_cand <- NULL
    step_best_bic <- best_bic

    for (cand in remaining) {
      trial_set <- c(selected, cand)
      X_trial <- F_obs[, trial_set, drop = FALSE]

      # Check collinearity against already selected donors
      if (length(selected) > 0L) {
        c_cors <- abs(stats::cor(F_obs[, cand], F_obs[, selected, drop = FALSE]))
        if (any(c_cors > max_collinearity, na.rm = TRUE)) {
          next
        }
      }

      # Fast ridge-stabilized OLS fit
      X_c <- sweep(X_trial, 2L, colMeans(X_trial), "-")
      XtX <- crossprod(X_c)
      k <- ncol(X_trial)
      pen <- 1e-4 * mean(diag(XtX))
      beta <- tryCatch(
        solve(XtX + diag(pen, nrow = k), crossprod(X_c, y_obs - y_bar)),
        error = function(e) NULL
      )
      if (is.null(beta)) next

      y_hat <- y_bar + as.numeric(X_c %*% beta)
      rss <- sum((y_obs - y_hat)^2)
      bic_val <- n * log(max(rss, 1e-12) / n) + (k + 1L) * log(n)

      if (bic_val < step_best_bic) {
        step_best_bic <- bic_val
        step_best_cand <- cand
      }
    }

    # Require significant BIC reduction to prevent adding noisy extra factors
    if (!is.null(step_best_cand) && (best_bic - step_best_bic) >= min_delta_bic) {
      selected <- c(selected, step_best_cand)
      best_bic <- step_best_bic
    } else {
      break
    }
  }

  # Fallback if no factor met BIC threshold: pick single highest absolute correlation donor
  if (length(selected) == 0L) {
    cors <- abs(stats::cor(y_obs, F_obs))
    if (length(cors) > 0L && any(!is.na(cors))) {
      best_single <- cand_names[which.max(cors)]
      selected <- best_single
    }
  }

  selected
}

reconstruct_factor_moments <- function(sample, settings = list()) {
  issues <- sample$issues %||% list()
  X_full <- sample$X_full
  comp_ids <- sample$component_ids
  asset_ids <- sample$asset_ids
  month_keys <- sample$all_month_keys
  preferred_donors_meta <- sample$preferred_donors_meta %||% list()

  # Anchor long-history horizon by the longest-history portfolio components in comp_ids
  comp_obs <- colSums(!is.na(X_full[, comp_ids, drop = FALSE]))
  max_comp_obs <- max(comp_obs)
  core_long_comp <- comp_ids[comp_obs >= floor(0.95 * max_comp_obs)]
  donor_complete_rows <- which(rowSums(is.na(X_full[, core_long_comp, drop = FALSE])) == 0L)

  if (length(donor_complete_rows) < 24L) {
    # Fallback to complete-case if donor span is too short
    cc_rows <- which(rowSums(is.na(X_full[, comp_ids, drop = FALSE])) == 0L)
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

  X_sub <- X_full[donor_complete_rows, , drop = FALSE]
  sub_months <- month_keys[donor_complete_rows]
  T_long <- nrow(X_sub)

  # All series complete on donor_complete_rows are valid candidate donors (active, hedges, and inactive factor series)
  all_missing_sub <- colSums(is.na(X_sub))
  donor_ids <- names(all_missing_sub)[all_missing_sub == 0L]

  # Short series needing reconstruction are those in comp_ids with missing observations in X_sub
  comp_missing_sub <- colSums(is.na(X_sub[, comp_ids, drop = FALSE]))
  short_ids <- names(comp_missing_sub)[comp_missing_sub > 0L]

  # Economically admissible donor mapping for known asset types (with automatic fallback)
  preferred_donors_map <- list(
    "SPCHICC(RI)" = c("BMSW10Y(RI)", "MSSWITL(RI)", "BMUS10Y(RI)"),
    "SWZKBIM(RI)" = c("MSSWITL(RI)", "BMSW10Y(RI)"),
    "DJUBSTR"     = c("GSGCTOT", "MSUSAM$(RI)", "MSEMKF$(RI)"),
    "SWISSMI(RI)" = c("MSSWITL(RI)"),
    "SWSPIXD"     = c("MSSWITL(RI)")
  )

  fac_cfg <- settings$factor_selection %||% list()
  sel_mode <- toupper(fac_cfg$mode %||% "AUTO")
  max_factors <- as.integer(fac_cfg$max_factors %||% 3L)
  min_delta_bic <- as.numeric(fac_cfg$min_delta_bic %||% 2.0)
  max_collinearity <- as.numeric(fac_cfg$max_collinearity %||% 0.95)

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
      donor_ids = intersect(donor_ids, comp_ids),
      all_donor_ids = donor_ids,
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
    y_obs <- y_vec[obs_idx]

    cand_donors <- character(0)
    selection_type <- "NONE"

    # Priority 1: User-specified Preferred_Donors in metadata sheet
    if (!is.null(preferred_donors_meta[[sid]])) {
      cand_donors <- intersect(preferred_donors_meta[[sid]], donor_ids)
      if (length(cand_donors) > 0L) {
        selection_type <- "METADATA_SPECIFIED"
      }
    }

    # Priority 2: Preset map if PRESET_FIRST requested
    if (length(cand_donors) == 0L && identical(sel_mode, "PRESET_FIRST") && !is.null(preferred_donors_map[[sid]])) {
      cand_donors <- intersect(preferred_donors_map[[sid]], donor_ids)
      if (length(cand_donors) > 0L) {
        selection_type <- "PRESET_MAP"
      }
    }

    # Priority 3: Automatic selection using BIC from all available candidate donors in donor_ids
    if (length(cand_donors) == 0L && sel_mode %in% c("AUTO", "FORCE_AUTO")) {
      pool_donors <- setdiff(donor_ids, sid)
      if (length(pool_donors) > 0L && T_obs >= 12L) {
        F_cand_obs <- F_mat[obs_idx, pool_donors, drop = FALSE]
        cand_donors <- select_best_donors(
          y_obs = y_obs,
          F_obs = F_cand_obs,
          max_factors = max_factors,
          min_delta_bic = min_delta_bic,
          max_collinearity = max_collinearity
        )
        if (length(cand_donors) > 0L) {
          selection_type <- "AUTO_BIC"
        }
      }
    }

    # Priority 4: Preset fallback or first 3 available donors
    if (length(cand_donors) == 0L && !is.null(preferred_donors_map[[sid]])) {
      cand_donors <- intersect(preferred_donors_map[[sid]], donor_ids)
      if (length(cand_donors) > 0L) {
        selection_type <- "PRESET_MAP_FALLBACK"
      }
    }
    if (length(cand_donors) == 0L) {
      cand_donors <- donor_ids[seq_len(min(3L, length(donor_ids)))]
      selection_type <- "FALLBACK_FIRST3"
    }

    F_obs <- F_mat[obs_idx, cand_donors, drop = FALSE]

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
      selection_mode = selection_type,
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

  # Reorder and slice to exact component_ids order (excluding non-component donor factors)
  Sigma_final <- Sigma_joint[comp_ids, comp_ids, drop = FALSE]
  mu_Y <- alpha_vec + as.numeric(B_mat %*% mu_F)
  names(mu_Y) <- short_ids
  mu_final <- c(mu_F, mu_Y)[comp_ids]

  total_cells <- T_long * length(comp_ids)
  missing_cells <- sum(comp_missing_sub)
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
    n_obs_common = sum(rowSums(is.na(X_sub[, comp_ids, drop = FALSE])) == 0L),
    start_month = sub_months[1L],
    end_month = sub_months[length(sub_months)],
    donor_ids = intersect(donor_ids, comp_ids),
    all_donor_ids = donor_ids,
    short_ids = short_ids,
    factor_audit = data.table::rbindlist(audit_rows, fill = TRUE),
    model_supported_share = model_share,
    shrinkage_intensity = sh_F$lambda,
    issues = issues
  )
}
