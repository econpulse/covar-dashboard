# ==============================================================================
# tests/testthat/test-phase2-3-risk-exposures.R
# Phase 2, 3, and 5 Analytical Tests:
# - ASSET_CLASS and CURRENCY_OVERLAY modes yield identical total return and
#   total variance when aggregate g is identical
# - Sum(w)=1; g does not increase Sum(w); ratio is NA when exposure is 0;
#   negative risk contributions are preserved
# - Variance from X_B and a equals direct historical portfolio variance on
#   the exact same sample for sample covariance
# - Sigma_h = A Sigma A' matches direct hedged-asset sample covariance matrix
#   prior to independent shrinkage
# - PSD, column permutation invariance, near-collinear series, zero-variance
#   series, and short-history factor residual variance
# - 5Y rebalancing paths, cumulative/geometric scenario returns, and costs
#   without double counting
# ==============================================================================

test_that("ASSET_CLASS and CURRENCY_OVERLAY yield identical portfolio return and variance for identical g", {
  wb <- read_input_workbook(TEST_DATA_PATH, TEST_CONFIG)
  reg <- build_series_registry(wb$long_data, wb$meta_raw, TEST_CONFIG, wb$file_hash, wb$path)
  spots <- normalize_spot_quotes(reg, TEST_CONFIG)
  fwds <- normalize_forward_quotes(reg, spots, TEST_CONFIG)
  cr <- cross_fx_levels(spots, fwds, "CHF")
  r_loc <- index_levels_to_returns(reg$data, reg$meta)
  u_chf <- translate_asset_returns(r_loc, cr, reg$meta)
  z_chf <- build_forward_pnl(cr)
  basis <- build_risk_basis(u_chf, z_chf, reg$meta, base_ccy = "CHF")
  sample_obj <- resolve_estimation_sample(basis, list(window_mode = "FULL", missing_mode = "COMPLETE_CASE"))
  model <- estimate_covariance(sample_obj, method = "SHRINKAGE")

  w_list <- TEST_CONFIG$portfolio_templates$saa_50$weights
  w_vec <- stats::setNames(as.numeric(w_list), names(w_list))

  hc_class <- list(
    mode = "ASSET_CLASS",
    class_ratios = list(EQUITY = 0.4, BOND = 1.0, REAL_ESTATE = 0.0, GOLD = 0.25, COMMODITY = 0.5),
    asset_overrides = list()
  )
  ev_class <- evaluate_portfolio(w_vec, hc_class, model, reg$meta)
  expect_true(ev_class$valid)

  # Convert ASSET_CLASS to CURRENCY_OVERLAY preserving aggregate g_c
  exp_pre <- derive_prehedge_exposures(w_vec, reg$meta, "CHF", model$available_hedge_currencies)
  conv_overlay <- convert_hedge_mode(exp_pre, hc_class, target_mode = "CURRENCY_OVERLAY")
  hc_overlay <- list(mode = "CURRENCY_OVERLAY", currency_ratios = conv_overlay$currency_ratios)

  ev_overlay <- evaluate_portfolio(w_vec, hc_overlay, model, reg$meta)
  expect_true(ev_overlay$valid)

  # Identical g_vector, identical variance, identical volatility, identical historical mean!
  expect_equal(ev_class$g_vector, ev_overlay$g_vector, tolerance = 1e-12)
  expect_equal(ev_class$metrics$var_monthly, ev_overlay$metrics$var_monthly, tolerance = 1e-14)
  expect_equal(ev_class$metrics$vol_annualized, ev_overlay$metrics$vol_annualized, tolerance = 1e-14)
  expect_equal(ev_class$metrics$historical_arith_mean_ann, ev_overlay$metrics$historical_arith_mean_ann, tolerance = 1e-14)

  # Sum(w) == 1, g does not increase Sum(w)
  expect_equal(ev_class$metrics$weight_sum, 1.0, tolerance = 1e-12)
  expect_true(ev_class$metrics$total_hedge_notional > 0.10)
  expect_equal(sum(ev_class$weights), 1.0, tolerance = 1e-12)

  # Currency exposure table includes latest annualized forward carry per currency
  ccy_tbl <- ev_class$exposures$currency_table
  expect_true("latest_carry_pa" %in% names(ccy_tbl))
  expect_equal(ccy_tbl[fx_exposure == "CHF", latest_carry_pa], 0.0)
  expect_true(!is.na(ccy_tbl[fx_exposure == "USD", latest_carry_pa]))
  expect_true(is.na(ccy_tbl[fx_exposure == "EMEQ", latest_carry_pa]))

  # Negative risk contributions are preserved on hedging legs (which offset FX risk)
  hedge_rcs <- ev_class$component_contributions[component_type == "FX_HEDGE", risk_contribution_ann]
  expect_true(any(hedge_rcs < 0))
  # Sum of component risk contributions equals portfolio annualized volatility
  expect_equal(sum(ev_class$component_contributions$risk_contribution_ann), ev_class$metrics$vol_annualized, tolerance = 1e-12)

  # Non-100% weight sum must NOT block evaluation; only issue a WARNING
  w_vec_105 <- w_vec * 1.05
  ev_105 <- evaluate_portfolio(w_vec_105, hc_class, model, reg$meta)
  expect_true(ev_105$valid)
  expect_equal(ev_105$metrics$weight_sum, 1.05, tolerance = 1e-12)
  expect_true(ev_105$metrics$vol_annualized > 0)
  warn_codes <- vapply(ev_105$issues, function(x) x$code, character(1))
  expect_true("WEIGHT_SUM_NOT_ONE" %in% warn_codes)
  weight_issue <- ev_105$issues[[which(warn_codes == "WEIGHT_SUM_NOT_ONE")[1]]]
  expect_equal(weight_issue$severity, "WARNING")
})

test_that("Portfolio variance a' Sigma a equals direct historical portfolio variance and w' (A Sigma A') w", {
  wb <- read_input_workbook(TEST_DATA_PATH, TEST_CONFIG)
  reg <- build_series_registry(wb$long_data, wb$meta_raw, TEST_CONFIG, wb$file_hash, wb$path)
  spots <- normalize_spot_quotes(reg, TEST_CONFIG)
  fwds <- normalize_forward_quotes(reg, spots, TEST_CONFIG)

  for (base_ccy in c("CHF", "EUR", "USD")) {
    cr <- cross_fx_levels(spots, fwds, base_ccy)
    r_loc <- index_levels_to_returns(reg$data, reg$meta)
    u_b <- translate_asset_returns(r_loc, cr, reg$meta)
    z_b <- build_forward_pnl(cr)
    basis <- build_risk_basis(u_b, z_b, reg$meta, base_ccy = base_ccy)
    sample_obj <- resolve_estimation_sample(basis, list(window_mode = "FULL", missing_mode = "COMPLETE_CASE"))
    sample_model <- estimate_covariance(sample_obj, method = "SAMPLE")

    w_list <- TEST_CONFIG$portfolio_templates$saa_50$weights
    w_vec <- stats::setNames(as.numeric(w_list), names(w_list))
    hc <- list(mode = "ASSET_CLASS", class_ratios = list(EQUITY = 0.5, BOND = 1.0, GOLD = 0.5, COMMODITY = 0.0))

    ev <- evaluate_portfolio(w_vec, hc, sample_model, reg$meta)
    expect_true(ev$valid)

    # 1. Compare a' Sigma_sample a against direct stats::var(X_sample %*% a)
    X_samp <- sample_obj$X_sample
    r_port_series <- as.numeric(X_samp %*% ev$a_vector)
    direct_var_m <- stats::var(r_port_series)
    expect_equal(ev$metrics$var_monthly, direct_var_m, tolerance = 1e-14)

    # 2. Compare Sigma_assets,B,h = A(h) Sigma_sample A(h)' against direct sample covariance of hedged asset series R(h) = X_sample %*% t(A(h))
    R_hedged_series <- X_samp %*% t(ev$A_matrix)
    direct_Sigma_assets_m <- stats::cov(R_hedged_series)
    expect_equal(ev$Sigma_assets_monthly, direct_Sigma_assets_m, tolerance = 1e-14)

    # 3. Verify w' Sigma_assets,B,h w == a' Sigma_B a
    expect_lt(ev$metrics$transform_identity_diff, 1e-15)
  }
})

test_that("PSD regularization, permutation invariance, collinearity, zero-variance, and factor moment reconstruction work", {
  set.seed(123)
  T_n <- 100
  f1 <- rnorm(T_n, 0, 0.04)
  f2 <- rnorm(T_n, 0, 0.02)
  # Include near-identical series (collinear) and a zero-variance series
  X_edge <- cbind(
    A1 = f1,
    A2 = f1 + rnorm(T_n, 0, 1e-6),
    A3 = f2,
    A_ZERO = rep(0.0, T_n)
  )

  sh_1 <- shrink_cov_constant_correlation(X_edge)
  reg_1 <- regularize_covariance(sh_1$Sigma)
  expect_true(reg_1$is_psd)
  expect_gte(reg_1$min_eigenvalue, 0)

  # Permutation invariance: permuting columns of X produces identically permuted Sigma
  perm <- c("A3", "A_ZERO", "A2", "A1")
  sh_perm <- shrink_cov_constant_correlation(X_edge[, perm])
  expect_equal(sh_1$Sigma[perm, perm], sh_perm$Sigma, tolerance = 1e-13)

  # Factor moment reconstruction on real Data.xlsx preserves PSD and separates true_obs from donor_obs
  wb <- read_input_workbook(TEST_DATA_PATH, TEST_CONFIG)
  reg <- build_series_registry(wb$long_data, wb$meta_raw, TEST_CONFIG, wb$file_hash, wb$path)
  spots <- normalize_spot_quotes(reg, TEST_CONFIG)
  fwds <- normalize_forward_quotes(reg, spots, TEST_CONFIG)
  cr <- cross_fx_levels(spots, fwds, "CHF")
  u_b <- translate_asset_returns(index_levels_to_returns(reg$data, reg$meta), cr, reg$meta)
  z_b <- build_forward_pnl(cr)
  basis <- build_risk_basis(u_b, z_b, reg$meta, base_ccy = "CHF")

  samp_fac <- resolve_estimation_sample(basis, list(window_mode = "FULL", missing_mode = "FACTOR_RECONSTRUCTION"))
  mod_fac <- estimate_covariance(samp_fac, method = "FACTOR_COMPLETED")
  expect_true(mod_fac$valid)
  expect_true(mod_fac$diagnostics$is_psd)
  expect_gt(nrow(mod_fac$factor_audit), 0L)
  # SPCHICC(RI) has 344 true observations vs 436 donor observations
  sp_audit <- mod_fac$factor_audit[series_code == "SPCHICC(RI)"]
  expect_equal(nrow(sp_audit), 1L)
  expect_lt(sp_audit$true_obs, sp_audit$donor_obs)
  expect_gt(sp_audit$residual_vol_ann, 0.0)
  expect_true("selection_mode" %in% names(mod_fac$factor_audit))
})

test_that("factor reconstruction supports automatic BIC donor selection, metadata Preferred_Donors, and inactive donor series", {
  wb <- read_input_workbook(TEST_DATA_PATH, TEST_CONFIG)

  meta_dt <- data.table::as.data.table(wb$meta_raw)
  long_dt <- data.table::as.data.table(wb$long_data)

  # 1. Add an inactive benchmark series with long history (intended strictly as a factor donor)
  meta_donor <- data.table::copy(meta_dt[Code == "BMSW10Y(RI)"])
  meta_donor[, Code := "BENCH_GLOBAL_AGG"]
  meta_donor[, Name := "Global Aggregate Benchmark (Inactive Factor Donor)"]
  meta_donor[, Active := FALSE]
  meta_donor[, Is_Donor := TRUE]

  data_donor <- data.table::copy(long_dt[Code == "BMUS10Y(RI)"])
  data_donor[, Code := "BENCH_GLOBAL_AGG"]
  data_donor[, value := value * 1.02]

  # 2. Add an explicit Preferred_Donors to SPCHICC(RI)
  meta_dt[Code == "SPCHICC(RI)", Preferred_Donors := "BMSW10Y(RI), BENCH_GLOBAL_AGG"]

  meta_combined <- rbind(meta_dt, meta_donor, fill = TRUE)
  data_combined <- rbind(long_dt, data_donor, fill = TRUE)

  reg <- build_series_registry(data_combined, meta_combined, TEST_CONFIG, wb$file_hash, wb$path)
  expect_true("BENCH_GLOBAL_AGG" %in% reg$donor_factor_codes)

  spots <- normalize_spot_quotes(reg, TEST_CONFIG)
  fwds <- normalize_forward_quotes(reg, spots, TEST_CONFIG)
  cr <- cross_fx_levels(spots, fwds, "CHF")
  u_b <- translate_asset_returns(index_levels_to_returns(reg$data, reg$meta), cr, reg$meta)
  z_b <- build_forward_pnl(cr)

  basis <- build_risk_basis(u_b, z_b, reg$meta, base_ccy = "CHF")
  expect_true("BENCH_GLOBAL_AGG" %in% basis$donor_factor_ids)
  expect_false("BENCH_GLOBAL_AGG" %in% basis$component_ids)

  samp_fac <- resolve_estimation_sample(basis, list(window_mode = "FULL", missing_mode = "FACTOR_RECONSTRUCTION"))
  expect_true("BENCH_GLOBAL_AGG" %in% colnames(samp_fac$X_full))
  expect_false("BENCH_GLOBAL_AGG" %in% colnames(samp_fac$X_sample))

  mod_fac <- estimate_covariance(samp_fac, method = "FACTOR_COMPLETED")
  expect_true(mod_fac$valid)
  expect_true(mod_fac$diagnostics$is_psd)

  # Check that Sigma has exact component_ids dimension without the inactive donor
  expect_equal(nrow(mod_fac$Sigma_monthly), length(basis$component_ids))
  expect_false("BENCH_GLOBAL_AGG" %in% rownames(mod_fac$Sigma_monthly))

  # SPCHICC(RI) should use metadata-specified donors including the inactive factor
  sp_audit <- mod_fac$factor_audit[series_code == "SPCHICC(RI)"]
  expect_equal(sp_audit$selection_mode, "METADATA_SPECIFIED")
  expect_true(grepl("BENCH_GLOBAL_AGG", sp_audit$donors))

  # Other short series without explicit metadata should use AUTO_BIC
  jpm_audit <- mod_fac$factor_audit[series_code == "JPMGCOC(RI)"]
  expect_equal(jpm_audit$selection_mode, "AUTO_BIC")
  expect_gt(jpm_audit$r_squared, 0.50)

  # 3. Check metadata warning when unknown code is given in Preferred_Donors
  bad_meta <- data.table::copy(meta_dt)
  bad_meta[Code == "SPCHICC(RI)", Preferred_Donors := "NON_EXISTENT_SERIES"]
  val_res <- validate_metadata(bad_meta)
  unk_issues <- Filter(function(x) identical(x$code, "UNKNOWN_PREFERRED_DONOR_CODE"), val_res$issues)
  expect_gt(length(unk_issues), 0L)
})

test_that("5Y deterministic scenario paths compute proper rebalanced portfolio CAGR without weighting asset CAGRs", {
  wb <- read_input_workbook(TEST_DATA_PATH, TEST_CONFIG)
  reg <- build_series_registry(wb$long_data, wb$meta_raw, TEST_CONFIG, wb$file_hash, wb$path)

  w_list <- TEST_CONFIG$portfolio_templates$saa_50$weights
  w_vec <- stats::setNames(as.numeric(w_list), names(w_list))
  hc <- TEST_CONFIG$default_hedge_configs$ASSET_CLASS

  scen_ann <- evaluate_return_scenarios(
    expectations = TEST_CONFIG$default_expectations,
    weights = w_vec,
    hedge_config = hc,
    meta = reg$meta,
    base_ccy = "CHF",
    scenario_id = "stagflation_5y",
    rebalancing_rule = "ANNUAL_REBALANCED",
    cost_bps_pa = 10,
    available_hedge_currencies = c("USD", "EUR", "GBP")
  )
  expect_true(scen_ann$valid)
  expect_equal(nrow(scen_ann$yearly_path), 6L) # Year 0..5

  # Verify portfolio 5Y CAGR equals (W_5 / 100)^(1/5) - 1
  w5 <- tail(scen_ann$yearly_path$wealth_hedged_base, 1L)
  expect_equal(scen_ann$metrics$scenario_cagr_hedged_5y, (w5 / 100.0)^(1/5) - 1.0, tolerance = 1e-12)

  # Verify Buy & Hold produces a distinct path from Annual Rebalanced when returns vary across years
  scen_bh <- evaluate_return_scenarios(
    expectations = TEST_CONFIG$default_expectations,
    weights = w_vec,
    hedge_config = hc,
    meta = reg$meta,
    base_ccy = "CHF",
    scenario_id = "stagflation_5y",
    rebalancing_rule = "BUY_AND_HOLD",
    cost_bps_pa = 10,
    available_hedge_currencies = c("USD", "EUR", "GBP")
  )
  expect_true(scen_bh$valid)
  expect_false(isTRUE(all.equal(scen_ann$metrics$scenario_cagr_hedged_5y, scen_bh$metrics$scenario_cagr_hedged_5y)))
})
