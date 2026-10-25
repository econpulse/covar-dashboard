# ==============================================================================
# R/validation.R
# Out-of-sample rolling walk-forward validation of risk estimators and
# joint monthly block-bootstrap uncertainty bands
# Pure R module - no Shiny dependencies
# ==============================================================================

evaluate_walk_forward <- function(basis,
                                  a_vector,
                                  train_months = 120L,
                                  test_months = 12L,
                                  step_months = 12L,
                                  methods = c("SAMPLE", "SHRINKAGE", "EWMA")) {
  X_mat <- basis$X_matrix
  comp_ids <- basis$component_ids
  month_keys <- basis$month_keys

  # Use complete-case rows for walk-forward out-of-sample evaluation
  cc_rows <- which(rowSums(is.na(X_mat[, comp_ids, drop = FALSE])) == 0L)
  X_cc <- X_mat[cc_rows, comp_ids, drop = FALSE]
  m_cc <- month_keys[cc_rows]
  T_total <- nrow(X_cc)

  train_len <- as.integer(train_months)
  test_len <- as.integer(test_months)
  step_len <- as.integer(step_months)

  if (T_total < (train_len + test_len)) {
    return(list(
      valid = FALSE,
      message = sprintf(
        "Zu wenige vollständige Beobachtungen (%d) für Walk-Forward (Training=%d + Test=%d Monate).",
        T_total, train_len, test_len
      ),
      windows = data.table::data.table(),
      summary = data.table::data.table()
    ))
  }

  a_vec <- as.numeric(a_vector[comp_ids])
  names(a_vec) <- comp_ids

  starts <- seq(from = 1L, to = T_total - train_len - test_len + 1L, by = step_len)
  win_rows <- list()

  for (s_idx in starts) {
    tr_idx <- s_idx:(s_idx + train_len - 1L)
    te_idx <- (s_idx + train_len):(s_idx + train_len + test_len - 1L)

    X_train <- X_cc[tr_idx, , drop = FALSE]
    X_test <- X_cc[te_idx, , drop = FALSE]

    # Realized out-of-sample annualized portfolio volatility
    r_test <- as.numeric(X_test %*% a_vec)
    realized_vol_ann <- stats::sd(r_test) * sqrt(12.0)

    for (mth in methods) {
      Sigma_tr <- if (identical(mth, "SAMPLE")) {
        stats::cov(X_train)
      } else if (identical(mth, "SHRINKAGE")) {
        shrink_cov_constant_correlation(X_train)$Sigma
      } else if (identical(mth, "EWMA")) {
        estimate_ewma_covariance(X_train, halflife_months = 24, max_window_months = train_len, regularization_gamma = 0.05)$Sigma
      } else {
        stats::cov(X_train)
      }
      pred_var_m <- max(0.0, as.numeric(crossprod(a_vec, Sigma_tr %*% a_vec)))
      pred_vol_ann <- sqrt(12.0 * pred_var_m)
      err <- pred_vol_ann - realized_vol_ann

      win_rows[[length(win_rows) + 1L]] <- data.table::data.table(
        method = mth,
        train_start = m_cc[tr_idx[1L]],
        train_end = m_cc[tail(tr_idx, 1L)],
        test_start = m_cc[te_idx[1L]],
        test_end = m_cc[tail(te_idx, 1L)],
        predicted_vol_ann = pred_vol_ann,
        realized_vol_ann = realized_vol_ann,
        error_ann = err,
        abs_error_ann = abs(err),
        sq_error_ann = err^2
      )
    }
  }

  win_dt <- data.table::rbindlist(win_rows, fill = TRUE)
  sum_dt <- win_dt[, .(
    n_windows = .N,
    mean_predicted_vol = mean(predicted_vol_ann, na.rm = TRUE),
    mean_realized_vol = mean(realized_vol_ann, na.rm = TRUE),
    bias = mean(error_ann, na.rm = TRUE),
    mae = mean(abs_error_ann, na.rm = TRUE),
    rmse = sqrt(mean(sq_error_ann, na.rm = TRUE))
  ), by = method]

  list(
    valid = TRUE,
    train_months = train_len,
    test_months = test_len,
    step_months = step_len,
    n_windows = length(starts),
    windows = win_dt,
    summary = sum_dt
  )
}

bootstrap_risk_uncertainty <- function(sample,
                                       a_vector,
                                       method = c("SHRINKAGE", "SAMPLE", "EWMA"),
                                       settings = list(),
                                       n_boot = 200L,
                                       block_length = 6L,
                                       seed = 42L) {
  method <- match.arg(toupper(method), c("SHRINKAGE", "SAMPLE", "EWMA"))
  X_mat <- sample$X_sample
  comp_ids <- sample$component_ids
  T_obs <- nrow(X_mat)
  p <- ncol(X_mat)

  if (T_obs < 18L) {
    return(list(
      valid = FALSE,
      message = "Zu wenige Beobachtungen für Block-Bootstrap."
    ))
  }

  a_vec <- as.numeric(a_vector[comp_ids])
  names(a_vec) <- comp_ids

  n_boot <- as.integer(max(20L, min(2000L, n_boot)))
  blk_len <- as.integer(max(1L, min(24L, block_length)))
  n_blocks <- ceiling(T_obs / blk_len)
  max_start <- max(1L, T_obs - blk_len + 1L)

  # Preserve and restore RNG state so function has no global side effects
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
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

  boot_vols <- numeric(n_boot)
  boot_rc_mat <- matrix(0.0, nrow = n_boot, ncol = p, dimnames = list(NULL, comp_ids))

  for (b in seq_len(n_boot)) {
    block_starts <- sample.int(max_start, size = n_blocks, replace = TRUE)
    idx_vec <- unlist(lapply(block_starts, function(s) s:(s + blk_len - 1L)), use.names = FALSE)
    idx_vec <- idx_vec[seq_len(T_obs)]
    X_b <- X_mat[idx_vec, , drop = FALSE]

    Sigma_b <- if (identical(method, "SHRINKAGE")) {
      shrink_cov_constant_correlation(X_b)$Sigma
    } else if (identical(method, "EWMA")) {
      estimate_ewma_covariance(X_b, halflife_months = settings$halflife_months %||% 24, max_window_months = T_obs)$Sigma
    } else {
      stats::cov(X_b)
    }

    Sa_b <- as.numeric(12.0 * (Sigma_b %*% a_vec))
    var_b <- max(0.0, sum(a_vec * Sa_b))
    vol_b <- sqrt(var_b)
    boot_vols[b] <- vol_b
    if (vol_b > 1e-14) {
      boot_rc_mat[b, ] <- (a_vec * Sa_b) / vol_b
    }
  }

  probs <- c(0.05, 0.25, 0.50, 0.75, 0.95)
  vol_q <- stats::quantile(boot_vols, probs = probs, na.rm = TRUE)

  rc_bands <- lapply(seq_len(p), function(j) {
    qs <- stats::quantile(boot_rc_mat[, j], probs = probs, na.rm = TRUE)
    data.table::data.table(
      component_id = comp_ids[j],
      exposure = a_vec[j],
      rc_p05 = as.numeric(qs[1L]),
      rc_p25 = as.numeric(qs[2L]),
      rc_p50 = as.numeric(qs[3L]),
      rc_p75 = as.numeric(qs[4L]),
      rc_p95 = as.numeric(qs[5L]),
      rc_sd = stats::sd(boot_rc_mat[, j], na.rm = TRUE)
    )
  })

  list(
    valid = TRUE,
    method = method,
    n_boot = n_boot,
    block_length = blk_len,
    seed = as.integer(seed),
    n_obs = T_obs,
    vol_quantiles = list(
      p05 = as.numeric(vol_q[1L]),
      p25 = as.numeric(vol_q[2L]),
      p50 = as.numeric(vol_q[3L]),
      p75 = as.numeric(vol_q[4L]),
      p95 = as.numeric(vol_q[5L]),
      mean = mean(boot_vols, na.rm = TRUE),
      sd = stats::sd(boot_vols, na.rm = TRUE)
    ),
    vol_distribution = as.numeric(boot_vols),
    component_rc_bands = data.table::rbindlist(rc_bands, fill = TRUE)
  )
}
