# ==============================================================================
# tests/testthat/test-phase1-fx-returns.R
# Phase 1 Domain Tests:
# - TR-Level -> monthly return, first month NA, staggered starts, interior gaps
#   never bridged
# - Quote scaling/inversion, Outright & Points, GBP spot migration/correction,
#   Spot and Forward triangular consistency
# - CHF/EUR/USD base currency switch with USD identity and domestic asset
# - Forward t-1 lag, zero-FX and known carry PnLs, full/half hedge, preserved
#   cross-term r_L * fx
# - EM (EMEQ) translated from USD, never included in USD hedgeable denominator
# - Futures-Gold (GSGCTOT) never automatically treated as physical gold
# - Missing JPY forwards block only affected JPY hedges; no silent substitutes
# - EUR-Cash-Proxy before 1999, Rate/100/Day Count, Excess-Cash subtracted once
# ==============================================================================

test_that("TR-Level to monthly return handles first month, staggered starts, and never bridges interior gaps", {
  # Construct synthetic series with staggered start and an interior NA gap at month 4
  synth_levels <- data.table::data.table(
    date = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30", "2020-05-31",
                     "2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30", "2020-05-31")),
    month_key = rep(c("2020-01", "2020-02", "2020-03", "2020-04", "2020-05"), 2),
    month_index = rep(2020L * 12L + 1:5, 2),
    Code = rep(c("ASSET_FULL", "ASSET_GAP"), each = 5),
    value = c(
      100, 105, 110.25, 110.25, 121.275, # +5%, +5%, 0%, +10%
      NA,  100, 110,    NA,     121       # starts Feb, has interior gap in Apr -> May must NOT bridge Mar->May!
    )
  )

  ret_dt <- index_levels_to_returns(synth_levels)

  r_full <- ret_dt[Code == "ASSET_FULL", r_local]
  expect_true(is.na(r_full[1]))
  expect_equal(r_full[2:5], c(0.05, 0.05, 0.0, 0.10), tolerance = 1e-12)

  r_gap <- ret_dt[Code == "ASSET_GAP", r_local]
  expect_true(is.na(r_gap[1])) # leading NA
  expect_true(is.na(r_gap[2])) # first observed level has no t-1
  expect_equal(r_gap[3], 0.10, tolerance = 1e-12)
  expect_true(is.na(r_gap[4])) # missing April level
  expect_true(is.na(r_gap[5])) # May has no April level -> must NOT bridge March to May!

  # Also verify calendar month skip (e.g., Jan directly to Mar without Feb row) is not bridged
  skip_dt <- synth_levels[Code == "ASSET_FULL" & month_key != "2020-02"]
  r_skip <- index_levels_to_returns(skip_dt)$r_local
  expect_true(is.na(r_skip[2])) # 2020-03 after 2020-01 (month_index diff == 2) -> NA
})

test_that("Quote scaling, inversion, Outright vs Points, GBP migration, and FX triangles hold", {
  wb <- read_input_workbook(TEST_DATA_PATH, TEST_CONFIG)
  expect_true(wb$valid)

  reg <- build_series_registry(wb$long_data, wb$meta_raw, TEST_CONFIG, wb$file_hash, wb$path)
  expect_true(reg$valid)

  spots <- normalize_spot_quotes(reg, TEST_CONFIG)
  fwds <- normalize_forward_quotes(reg, spots, TEST_CONFIG)
  expect_true(spots$valid)
  expect_true(fwds$valid)

  # Test triangular identity across CHF, EUR, USD for both Spot S and Forward F:
  # S^(CHF/EUR) * S^(EUR/USD) * S^(USD/CHF) == 1
  cr_chf <- cross_fx_levels(spots, fwds, "CHF")$crosses
  cr_eur <- cross_fx_levels(spots, fwds, "EUR")$crosses
  cr_usd <- cross_fx_levels(spots, fwds, "USD")$crosses

  s_chf_eur <- cr_chf[currency == "EUR", S_b_per_c]
  s_eur_usd <- cr_eur[currency == "USD", S_b_per_c]
  s_usd_chf <- cr_usd[currency == "CHF", S_b_per_c]
  tri_spot <- s_chf_eur * s_eur_usd * s_usd_chf
  expect_equal(tri_spot[!is.na(tri_spot)], rep(1.0, sum(!is.na(tri_spot))), tolerance = 1e-12)

  f_chf_eur <- cr_chf[currency == "EUR", F_b_per_c]
  f_eur_usd <- cr_eur[currency == "USD", F_b_per_c]
  f_usd_chf <- cr_usd[currency == "CHF", F_b_per_c]
  tri_fwd <- f_chf_eur * f_eur_usd * f_usd_chf
  valid_f <- !is.na(tri_fwd)
  expect_true(sum(valid_f) > 400)
  expect_equal(tri_fwd[valid_f], rep(1.0, sum(valid_f)), tolerance = 1e-12)

  # Test GBP migration when GBP spot is provided in USD per GBP with a new code (e.g. TDGBPSP)
  meta_mod <- data.table::copy(wb$meta_raw)
  long_mod <- data.table::copy(wb$long_data)
  long_mod[Code == "USSTBOE", `:=`(Code = "TDGBPSP", value = 1.0 / value)]
  reg_gbp_new <- build_series_registry(long_mod, meta_mod, TEST_CONFIG)
  expect_true(reg_gbp_new$valid)
  expect_true("TDGBPSP" %in% reg_gbp_new$meta$Code)
  expect_equal(reg_gbp_new$meta[Code == "TDGBPSP", Quote_Direction], "QUOTE_PER_FOREIGN")

  spots_new <- normalize_spot_quotes(reg_gbp_new, TEST_CONFIG)
  fwds_new <- normalize_forward_quotes(reg_gbp_new, spots_new, TEST_CONFIG)
  expect_true(spots_new$valid)
  expect_true(fwds_new$valid)
  expect_equal(
    spots_new$V[currency == "GBP", V_usd_per_ccy],
    spots$V[currency == "GBP", V_usd_per_ccy],
    tolerance = 1e-12
  )

  # Test Forward_Type == POINTS conversion before inversion
  meta_pts <- data.table::copy(reg$meta)
  long_pts <- data.table::copy(reg$data)
  # Replace TDCHF1M outright with points (TDCHF1M - TDCHFSP) * 10000 in FOREIGN_PER_QUOTE (CHF per USD)
  chf_sp <- long_pts[Code == "TDCHFSP", .(month_index, sp = value)]
  long_pts <- merge(long_pts, chf_sp, by = "month_index", all.x = TRUE, sort = FALSE)
  long_pts[Code == "TDCHF1M", value := (value - sp) * 10000.0]
  long_pts[, sp := NULL]
  data.table::setorder(long_pts, Code, month_index)

  meta_pts[Code == "TDCHF1M", `:=`(Forward_Type = "POINTS", Value_Unit = "POINTS", Points_Multiplier = 1e-4, Spot_Reference_Code = "TDCHFSP")]
  reg_pts <- list(meta = meta_pts, data = long_pts)
  fwds_pts <- normalize_forward_quotes(reg_pts, spots, TEST_CONFIG)
  expect_true(fwds_pts$valid)
  expect_equal(
    fwds_pts$G[currency == "CHF", G_usd_per_ccy],
    fwds$G[currency == "CHF", G_usd_per_ccy],
    tolerance = 1e-12
  )
})

test_that("Forward t-1 lag, carry + spot_offset decomposition, full/half hedge, and cross-term r_L * fx hold", {
  # Analytical 2-period test case:
  # Base = CHF, Asset = USD Equity
  # t=1 (2025-01): Spot S_0^(CHF/USD) = 1.00, 1M Forward F_0^(CHF/USD) = 0.997 (carry = -0.3%)
  # t=2 (2025-02): Spot S_1^(CHF/USD) = 1.02 (+2.0% USD appreciation), Forward F_1 = 1.015
  # Asset local return r_L = +5.0% (100 -> 105)
  dates <- as.Date(c("2025-01-31", "2025-02-28"))
  m_keys <- c("2025-01", "2025-02")
  m_idx <- c(2025L * 12L + 1L, 2025L * 12L + 2L)

  V_dt <- data.table::data.table(
    date = rep(dates, 2),
    month_key = rep(m_keys, 2),
    month_index = rep(m_idx, 2),
    currency = rep(c("USD", "CHF"), each = 2),
    source_code = rep(c("USD_ID", "TDCHFSP"), each = 2),
    raw_spot = c(1, 1, 1.00, 1.0 / 1.02),
    V_usd_per_ccy = c(1.0, 1.0, 1.00, 1.0 / 1.02)
  )
  G_dt <- data.table::data.table(
    date = rep(dates, 2),
    month_key = rep(m_keys, 2),
    month_index = rep(m_idx, 2),
    currency = rep(c("USD", "CHF"), each = 2),
    source_code = rep(c("USD_ID", "TDCHF1M"), each = 2),
    tenor = "1M",
    raw_forward = c(1, 1, 1.0 / 0.997, 1.0 / 1.015),
    G_usd_per_ccy = c(1.0, 1.0, 1.0 / 0.997, 1.0 / 1.015)
  )

  cr <- cross_fx_levels(V_dt, G_dt, base_ccy = "CHF")
  z_res <- build_forward_pnl(cr, cost_bps_pa = 0)
  z_usd <- z_res$foreign_pnl[currency == "USD" & month_key == "2025-02"]

  # Verify exact carry, spot_offset, and z_base using F_{t-1} (0.997), NOT F_t (1.015)!
  expect_equal(z_usd$carry, 0.997 / 1.00 - 1.0, tolerance = 1e-12)       # -0.003 (-0.3%)
  expect_equal(z_usd$spot_offset, -(1.02 / 1.00 - 1.0), tolerance = 1e-12) # -0.020 (-2.0%)
  expect_equal(z_usd$z_base, (0.997 - 1.02) / 1.00, tolerance = 1e-12)   # -0.023 (-2.3%)
  expect_equal(z_usd$carry + z_usd$spot_offset, z_usd$z_base, tolerance = 1e-12)

  # Asset translation and hedge return r(h) = u + h * z:
  levels_dt <- data.table::data.table(
    date = dates, month_key = m_keys, month_index = m_idx,
    Code = "MSUSAM$(RI)", value = c(100.0, 105.0)
  )
  meta_min <- data.table::data.table(
    Code = "MSUSAM$(RI)", Name = "MSCI USA", Series_Type = "ASSET_TR",
    Asset_Class = "EQUITY", Sub_Asset_Class = "BROAD", Return_Currency = "USD",
    FX_Exposure = "USD", Hedge_Allowed = TRUE, Default_Hedge_Ratio = 0,
    Return_Structure = "EQUITY_TR", Active = TRUE
  )
  r_loc <- index_levels_to_returns(levels_dt)
  u_dt <- translate_asset_returns(r_loc, cr, meta_min)[month_key == "2025-02"]

  # u = (1 + 0.05) * (1 + 0.02) - 1 = 0.071 (5% local + 2% FX + 0.1% cross-term)
  expect_equal(u_dt$r_local, 0.05, tolerance = 1e-12)
  expect_equal(u_dt$fx_spot_return, 0.02, tolerance = 1e-12)
  expect_equal(u_dt$cross_term, 0.05 * 0.02, tolerance = 1e-12)
  expect_equal(u_dt$u_base, 0.071, tolerance = 1e-12)

  # Full hedge (h = 1.0): r(1) = u + 1 * z = (0.05 + 0.02 + 0.001) + (-0.003 - 0.02) = 0.05 - 0.003 + 0.001 = 0.048
  # Notice: FX spot return (+2%) cancels out, carry (-0.3%) is applied once, cross-term (+0.1%) remains!
  r_full_hedge <- u_dt$u_base + 1.0 * z_usd$z_base
  expect_equal(r_full_hedge, u_dt$r_local + z_usd$carry + u_dt$cross_term, tolerance = 1e-12)

  # Half hedge (h = 0.5):
  r_half_hedge <- u_dt$u_base + 0.5 * z_usd$z_base
  expect_equal(r_half_hedge, 0.071 + 0.5 * (-0.023), tolerance = 1e-12)
})

test_that("EMEQ is translated from USD, never included in USD hedge denominator; JPY forward lock and EUR cash proxy work", {
  wb <- read_input_workbook(TEST_DATA_PATH, TEST_CONFIG)
  reg <- build_series_registry(wb$long_data, wb$meta_raw, TEST_CONFIG, wb$file_hash, wb$path)
  spots <- normalize_spot_quotes(reg, TEST_CONFIG)
  fwds <- normalize_forward_quotes(reg, spots, TEST_CONFIG)

  # 1. EMEQ translated from USD into CHF, but never included in USD hedgeable exposure
  cr_chf <- cross_fx_levels(spots, fwds, "CHF")
  r_loc <- index_levels_to_returns(reg$data, reg$meta)
  u_chf <- translate_asset_returns(r_loc, cr_chf, reg$meta)

  em_row <- u_chf[Code == "MSEMKF$(RI)" & month_key == "2020-06"]
  us_row <- u_chf[Code == "MSUSAM$(RI)" & month_key == "2020-06"]
  expect_equal(em_row$fx_spot_return, us_row$fx_spot_return, tolerance = 1e-12)
  expect_equal(em_row$FX_Exposure, "EMEQ")

  w_test <- TEST_CONFIG$portfolio_templates$saa_50$weights
  w_vec <- stats::setNames(as.numeric(w_test), names(w_test))
  exp_pre <- derive_prehedge_exposures(w_vec, reg$meta, base_ccy = "CHF", available_hedge_currencies = c("USD", "EUR", "GBP"))

  usd_ccy_row <- exp_pre$currency_exposures[fx_exposure == "USD"]
  emeq_ccy_row <- exp_pre$currency_exposures[fx_exposure == "EMEQ"]
  # USD exposure = MSUSAM$(RI) (0.16) + BMUS10Y(RI) (0.08) + GSGCTOT (0.04) + DJUBSTR (0.02) = 0.30 (excludes MSEMKF$(RI) = 0.06!)
  expect_equal(usd_ccy_row$exposure_before_hedge, 0.30, tolerance = 1e-12)
  expect_equal(usd_ccy_row$hedgeable_exposure, 0.30, tolerance = 1e-12)
  expect_equal(emeq_ccy_row$exposure_before_hedge, 0.06, tolerance = 1e-12)
  expect_equal(emeq_ccy_row$hedgeable_exposure, 0.0, tolerance = 1e-12)

  # Resolve hedge notionals and verify EMEQ effective hedge ratio is NA (n/a), never NaN
  resolved <- resolve_hedge_notionals(exp_pre, TEST_CONFIG$default_hedge_configs$CURRENCY_OVERLAY)
  expect_true(resolved$valid)
  expect_true(is.na(resolved$currency_table[fx_exposure == "EMEQ", effective_hedge_ratio]))

  # 2. Missing JPY forward blocks explicit JPY hedge request, but allows unhedged JPY
  bad_jpy_hc <- list(mode = "CURRENCY_OVERLAY", currency_ratios = list(USD = 0.5, EUR = 0.5, JPY = 0.5))
  res_bad_jpy <- resolve_hedge_notionals(exp_pre, bad_jpy_hc)
  expect_false(res_bad_jpy$valid)
  err_codes <- vapply(res_bad_jpy$issues, function(x) x$code, character(1))
  expect_true("MISSING_FORWARD_FOR_CURRENCY_OVERLAY" %in% err_codes)

  # 3. GSGCTOT physical gold assumption is rejected in expectations validator
  bad_exp <- TEST_CONFIG$default_expectations
  bad_exp$scenarios$base_5y$asset_assumptions$GSGCTOT$return_structure <- "PHYSICAL_GOLD"
  val_bad_gold <- validate_expectations(bad_exp, reg$meta)
  expect_false(val_bad_gold$valid)
  expect_true("GSGCTOT_PHYSICAL_GOLD_MISMATCH" %in% vapply(val_bad_gold$issues, function(x) x$code, character(1)))

  # 4. EUR Cash Proxy before 1999 and Excess Return single subtraction
  cash_res <- build_cash_returns(reg, TEST_CONFIG)
  expect_true(cash_res$valid)
  eur_cash_1995 <- cash_res$cash[cash_currency == "EUR" & month_key == "1995-06"]
  expect_equal(nrow(eur_cash_1995), 1L)
  expect_true(eur_cash_1995$is_proxy)
  expect_equal(eur_cash_1995$proxy_type, "DEM_DEPOSIT_PROXY")
  expect_equal(eur_cash_1995$r_cash, (eur_cash_1995$rate_pa_t_1 / 100.0) / 12.0, tolerance = 1e-12)

  # Excess return subtracts cash ONCE from u and NEVER from z
  z_chf <- build_forward_pnl(cr_chf)
  basis_tot <- build_risk_basis(u_chf, z_chf, reg$meta, base_ccy = "CHF", return_mode = "TOTAL")
  basis_exc <- build_risk_basis(u_chf, z_chf, reg$meta, base_ccy = "CHF", return_mode = "EXCESS", cash_returns = cash_res)

  # Hedge columns HEDGE_USD, HEDGE_EUR in X_matrix must be identical between TOTAL and EXCESS!
  expect_equal(
    basis_tot$X_matrix[, "HEDGE_USD"],
    basis_exc$X_matrix[, "HEDGE_USD"],
    tolerance = 1e-12
  )
  # Portfolio return difference between TOTAL and EXCESS for any (w, g) with Sum(w)=1 is exactly r_cash_CHF!
  a_test <- assemble_exposure_vector(w_vec, resolved$g_vector, basis_tot$component_ids)
  m_idx_2020 <- which(basis_tot$month_keys == "2020-06")
  r_p_tot <- sum(basis_tot$X_matrix[m_idx_2020, ] * a_test)
  r_p_exc <- sum(basis_exc$X_matrix[m_idx_2020, ] * a_test)
  chf_cash_2020 <- cash_res$cash[cash_currency == "CHF" & month_key == "2020-06", r_cash]
  expect_equal(r_p_tot - r_p_exc, chf_cash_2020, tolerance = 1e-12)
})
