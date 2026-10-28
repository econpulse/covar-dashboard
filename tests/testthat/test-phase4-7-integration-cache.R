# ==============================================================================
# tests/testthat/test-phase4-7-integration-cache.R
# Phase 4, 6, and 7 Integration, Cache, Isolation, Export & Performance Tests:
# - Browser-Shiny protocol handshake, repeated identical commands, JSON roundtrip
# - Weight/hedge change triggers ZERO new covariance estimations (call counter test)
# - Data/metadata revision invalidates relevant caches even with identical end date
# - Two sessions do not affect each other's private configurations or uploads
# - Export bundle is stamped with exact applied configuration hash
# - Performance budget test: 30 assets, 10 hedge components, 600 months
#   warm portfolio evaluation < 300 ms
# ==============================================================================

test_that("Protocol handshake, JSON roundtrip, and weight/hedge changes trigger zero new covariance estimations", {
  cache <- create_cache_manager(max_size_mb = 128)
  st <- create_session_state(TEST_CONFIG, shared_cache = cache)
  ld <- load_dataset_into_state(st, TEST_DATA_PATH, is_private = FALSE)
  expect_true(ld$valid)

  # 1. Bootstrap request estimates the active model once (plus comparison models)
  req_boot <- list(
    protocol_version = 1L,
    request_id = "req_boot_1",
    type = "app.bootstrap",
    context = list(base_currency = "CHF"),
    payload = list(base_currency = "CHF", model_id = "shrinkage_long", portfolio_id = "saa_50")
  )
  resp_boot <- handle_app_command(req_boot, st)
  expect_equal(resp_boot$status, "ok")
  expect_equal(resp_boot$request_id, "req_boot_1")

  # Verify clean JSON roundtrip (no NaN, Inf, or unserializable objects)
  json_str <- jsonlite::toJSON(resp_boot, auto_unbox = TRUE, na = "null")
  parsed_back <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
  expect_equal(parsed_back$status, "ok")
  expect_equal(parsed_back$request_id, "req_boot_1")

  cov_calls_after_boot <- cache$get_stats()$covariance_estimations
  expect_gt(cov_calls_after_boot, 0L)

  # 2. Modify portfolio weights and hedge ratios 10 times via portfolio.evaluate:
  # Covariance estimation counter MUST NOT increase!
  for (k in 1:5) {
    w_mod <- TEST_CONFIG$portfolio_templates$saa_50$weights
    w_mod[["MSSWITL(RI)"]] <- 0.18 + 0.01 * k
    w_mod[["BMSW10Y(RI)"]] <- 0.16 - 0.01 * k
    hc_mod <- list(
      mode = "ASSET_CLASS",
      class_ratios = list(EQUITY = 0.1 * k, BOND = 1.0, REAL_ESTATE = 0.0, GOLD = 0.2, COMMODITY = 0.0)
    )
    req_eval <- list(
      protocol_version = 1L,
      request_id = paste0("req_eval_", k),
      type = "portfolio.evaluate",
      context = list(base_currency = "CHF", model_id = "shrinkage_long"),
      payload = list(portfolio_id = "saa_50", weights = w_mod, hedge_config = hc_mod)
    )
    resp_eval <- handle_app_command(req_eval, st)
    expect_equal(resp_eval$status, "ok")
  }

  cov_calls_after_sliders <- cache$get_stats()$covariance_estimations
  expect_equal(cov_calls_after_sliders, cov_calls_after_boot)
})

test_that("Historical data/metadata revision invalidates cache even when end date is unchanged", {
  cache <- create_cache_manager(max_size_mb = 128)
  wb <- read_input_workbook(TEST_DATA_PATH, TEST_CONFIG)
  reg1 <- build_series_registry(wb$long_data, wb$meta_raw, TEST_CONFIG, wb$file_hash, wb$path)

  # Revise a single historical observation in 1995-06 while keeping the end date (2026-09) identical
  long_rev <- data.table::copy(wb$long_data)
  long_rev[Code == "MSSWITL(RI)" & month_key == "1995-06", value := value * 1.05]
  reg2 <- build_series_registry(long_rev, wb$meta_raw, TEST_CONFIG, wb$file_hash, wb$path)

  expect_equal(max(reg1$data$month_key), max(reg2$data$month_key))
  expect_false(identical(reg1$data_hash, reg2$data_hash))

  st1 <- create_session_state(TEST_CONFIG, shared_cache = cache)
  st1$dataset <- reg1
  st1$spots <- normalize_spot_quotes(reg1, TEST_CONFIG)
  st1$forwards <- normalize_forward_quotes(reg1, st1$spots, TEST_CONFIG)
  st1$cash <- build_cash_returns(reg1, TEST_CONFIG)
  st1$universe <- reg1$active_assets
  m1 <- get_or_estimate_covariance(st1, model_id = "shrinkage_long", base_ccy = "CHF")

  st2 <- create_session_state(TEST_CONFIG, shared_cache = cache)
  st2$dataset <- reg2
  st2$spots <- normalize_spot_quotes(reg2, TEST_CONFIG)
  st2$forwards <- normalize_forward_quotes(reg2, st2$spots, TEST_CONFIG)
  st2$cash <- build_cash_returns(reg2, TEST_CONFIG)
  st2$universe <- reg2$active_assets
  m2 <- get_or_estimate_covariance(st2, model_id = "shrinkage_long", base_ccy = "CHF")

  expect_false(m2$model_cache_hit)
  expect_false(identical(m1$model$model_hash, m2$model$model_hash))
})

test_that("Two sessions remain strictly isolated and exports match the applied configuration hash", {
  shared_cache <- create_cache_manager(max_size_mb = 128)
  sess_a <- create_session_state(TEST_CONFIG, shared_cache = shared_cache)
  sess_b <- create_session_state(TEST_CONFIG, shared_cache = shared_cache)

  load_dataset_into_state(sess_a, TEST_DATA_PATH, is_private = FALSE)
  load_dataset_into_state(sess_b, TEST_DATA_PATH, is_private = FALSE)

  # Session A switches to USD and SAA 75; Session B stays in CHF and SAA 25
  resp_a <- handle_app_command(list(
    protocol_version = 1L, request_id = "a1", type = "app.bootstrap",
    payload = list(base_currency = "USD", portfolio_id = "saa_75", model_id = "ewma_24m")
  ), sess_a)

  resp_b <- handle_app_command(list(
    protocol_version = 1L, request_id = "b1", type = "app.bootstrap",
    payload = list(base_currency = "CHF", portfolio_id = "saa_25", model_id = "shrinkage_long")
  ), sess_b)

  expect_equal(sess_a$base_currency, "USD")
  expect_equal(sess_b$base_currency, "CHF")
  expect_equal(sess_a$active_portfolio_id, "saa_75")
  expect_equal(sess_b$active_portfolio_id, "saa_25")
  expect_false(identical(
    sess_a$current_portfolio_eval$config_hash,
    sess_b$current_portfolio_eval$config_hash
  ))

  # Verify Export Bundle matches exact applied config_hash
  exp_xlsx <- export_analysis_bundle(
    dataset = sess_b$dataset,
    model = sess_b$current_model,
    portfolio_eval = sess_b$current_portfolio_eval,
    scenario_eval = sess_b$current_scenario_eval,
    comparison_eval = sess_b$current_comparison_eval,
    format = "XLSX",
    state_version = sess_b$state_version
  )
  expect_true(exp_xlsx$valid)
  expect_true(file.exists(exp_xlsx$output_path))
  expect_equal(exp_xlsx$manifest$portfolio_config_hash, sess_b$current_portfolio_eval$config_hash)
  expect_equal(exp_xlsx$manifest$risk_model_hash, sess_b$current_model$model_hash)
  unlink(exp_xlsx$output_path)
})

test_that("Performance budget: 30 assets, 10 hedge components, 600 months warm portfolio evaluation < 300 ms", {
  set.seed(42)
  n_months <- 600L
  n_assets <- 30L
  n_hedges <- 10L
  n_comp <- n_assets + n_hedges

  asset_ids <- sprintf("SYNTH_ASSET_%02d", seq_len(n_assets))
  ccy_pool <- sprintf("CCY%02d", seq_len(n_hedges))
  hedge_ids <- paste0("HEDGE_", ccy_pool)
  comp_ids <- c(asset_ids, hedge_ids)

  X_sim <- matrix(rnorm(n_months * n_comp, mean = 0.004, sd = 0.035), nrow = n_months, ncol = n_comp)
  colnames(X_sim) <- comp_ids
  m_keys <- sprintf("%04d-%02d", rep(1975:2024, each = 12), rep(1:12, times = 50))
  rownames(X_sim) <- m_keys

  synth_meta <- data.table::data.table(
    Code = asset_ids,
    Name = paste("Synthetic Asset", seq_len(n_assets)),
    Series_Type = "ASSET_TR",
    Asset_Class = rep(c("EQUITY", "BOND", "REAL_ESTATE", "GOLD", "COMMODITY"), length.out = n_assets),
    Sub_Asset_Class = "SYNTH",
    Return_Currency = rep(ccy_pool, length.out = n_assets),
    FX_Exposure = rep(ccy_pool, length.out = n_assets),
    Return_Structure = "EQUITY_TR",
    Hedge_Allowed = TRUE,
    Default_Hedge_Ratio = 0.5,
    Active = TRUE
  )

  sample_sim <- list(
    valid = TRUE,
    basis_id = "synth_600",
    base_currency = "CHF",
    return_mode = "TOTAL",
    n_obs = n_months,
    start_month = m_keys[1L],
    end_month = m_keys[n_months],
    unhedged_common_start = m_keys[1L],
    hedged_common_start = m_keys[1L],
    asset_ids = asset_ids,
    hedge_ids = hedge_ids,
    component_ids = comp_ids,
    required_hedge_currencies = ccy_pool,
    available_hedge_currencies = ccy_pool,
    missing_hedge_currencies = character(0),
    component_availability = data.table::data.table(),
    X_sample = X_sim,
    X_full = X_sim,
    month_keys = m_keys,
    all_month_keys = m_keys,
    issues = list()
  )

  model_sim <- estimate_covariance(sample_sim, method = "SHRINKAGE")
  w_sim <- stats::setNames(rep(1.0 / n_assets, n_assets), asset_ids)
  hc_sim <- list(
    mode = "ASSET_CLASS",
    class_ratios = list(EQUITY = 0.5, BOND = 1.0, REAL_ESTATE = 0.3, GOLD = 0.0, COMMODITY = 0.5)
  )

  # Measure warm portfolio evaluation latency
  t0 <- proc.time()[["elapsed"]]
  ev_sim <- evaluate_portfolio(w_sim, hc_sim, model_sim, synth_meta)
  elapsed_ms <- (proc.time()[["elapsed"]] - t0) * 1000.0

  expect_true(ev_sim$valid)
  expect_lt(elapsed_ms, 300.0) # Target budget: < 300 ms
})

test_that("Portfolio save/delete, universe toggle, and 5Y carry overrides work reliably", {
  cache <- create_cache_manager(max_size_mb = 128)
  st <- create_session_state(TEST_CONFIG, shared_cache = cache)
  ld <- load_dataset_into_state(st, TEST_DATA_PATH, is_private = FALSE)
  expect_true(ld$valid)

  # 1. Bootstrap
  resp_boot <- handle_app_command(list(
    protocol_version = 1L, request_id = "r1", type = "app.bootstrap",
    context = list(base_currency = "CHF"),
    payload = list(base_currency = "CHF", model_id = "shrinkage_long", portfolio_id = "saa_50")
  ), st)
  expect_equal(resp_boot$status, "ok")

  # 2. Portfolio save: create a custom portfolio
  w_custom <- list("MSSWITL(RI)" = 0.5, "BMSW10Y(RI)" = 0.5)
  resp_save <- handle_app_command(list(
    protocol_version = 1L, request_id = "r2", type = "portfolio.save",
    payload = list(portfolio_id = "port_custom_1", name = "Mein Test Portfolio", weights = w_custom)
  ), st)
  expect_equal(resp_save$status, "ok")
  expect_true("port_custom_1" %in% names(st$portfolios))
  expect_equal(st$active_portfolio_id, "port_custom_1")

  # 3. Portfolio delete: cannot delete builtin template
  resp_del_builtin <- handle_app_command(list(
    protocol_version = 1L, request_id = "r3", type = "portfolio.delete",
    payload = list(portfolio_id = "saa_50")
  ), st)
  expect_equal(resp_del_builtin$status, "error")

  # Delete custom portfolio
  resp_del_custom <- handle_app_command(list(
    protocol_version = 1L, request_id = "r4", type = "portfolio.delete",
    payload = list(portfolio_id = "port_custom_1")
  ), st)
  expect_equal(resp_del_custom$status, "ok")
  expect_false("port_custom_1" %in% names(st$portfolios))
  expect_equal(st$active_portfolio_id, "saa_50")

  # 4. 5Y Carry overrides
  resp_scen_ovr <- handle_app_command(list(
    protocol_version = 1L, request_id = "r5", type = "scenarios.evaluate",
    payload = list(scenario_id = "base_5y", carry_overrides = list(USD = -0.015))
  ), st)
  expect_equal(resp_scen_ovr$status, "ok")
  usd_asset_row <- st$current_scenario_eval$asset_table[fx_exposure == "USD"][1L]
  expect_equal(usd_asset_row$avg_forward_carry_pa, -0.015)

  # 5. Universe toggle asset
  orig_len <- length(st$universe)
  resp_tog <- handle_app_command(list(
    protocol_version = 1L, request_id = "r6", type = "universe.toggle_asset",
    payload = list(asset_id = "GSGCTOT")
  ), st)
  expect_equal(resp_tog$status, "ok")
  expect_false("GSGCTOT" %in% st$universe)
  expect_equal(length(st$universe), orig_len - 1L)

  # Toggle back
  resp_tog_back <- handle_app_command(list(
    protocol_version = 1L, request_id = "r7", type = "universe.toggle_asset",
    payload = list(asset_id = "GSGCTOT")
  ), st)
  expect_equal(resp_tog_back$status, "ok")
  expect_true("GSGCTOT" %in% st$universe)
  expect_equal(length(st$universe), orig_len)
})

test_that("Dynamic hedge currency detection in CMAs works across base currencies (CHF, EUR, USD, GBP)", {
  st <- create_session_state(TEST_CONFIG)
  load_dataset_into_state(st, TEST_DATA_PATH)

  # 1. Base CHF
  res_chf <- run_full_evaluation_pipeline(st, base_ccy = "CHF")
  pl_chf <- build_full_client_payload(st, res_chf)
  sc_chf <- pl_chf$scenario_evaluation

  expect_true(all(c("EUR", "USD") %in% sc_chf$relevant_hedge_currencies))
  expect_false("CHF" %in% sc_chf$relevant_hedge_currencies)
  expect_true(all(c("EUR", "USD", "GBP") %in% sc_chf$available_hedge_currencies))
  expect_false("CHF" %in% sc_chf$available_hedge_currencies)

  # 2. Base EUR: CHF is foreign, EUR is domestic
  res_eur <- run_full_evaluation_pipeline(st, base_ccy = "EUR")
  pl_eur <- build_full_client_payload(st, res_eur)
  sc_eur <- pl_eur$scenario_evaluation

  expect_true(all(c("CHF", "USD") %in% sc_eur$relevant_hedge_currencies))
  expect_false("EUR" %in% sc_eur$relevant_hedge_currencies)
  expect_true(all(c("CHF", "USD", "GBP") %in% sc_eur$available_hedge_currencies))
  expect_false("EUR" %in% sc_eur$available_hedge_currencies)

  # CHF in EUR should have positive forward carry (CHF rates lower than EUR) and be marked as needed
  chf_idx <- which(vapply(sc_eur$hedge_currency_summary, function(x) identical(x$currency, "CHF"), logical(1)))
  expect_true(length(chf_idx) == 1L)
  chf_summary <- sc_eur$hedge_currency_summary[[chf_idx]]
  expect_true(chf_summary$is_needed)
  expect_true(chf_summary$forward_available)
  expect_true(chf_summary$avg_forward_carry_pa > 0.01)

  # 3. Base USD: CHF is foreign, USD is domestic
  res_usd <- run_full_evaluation_pipeline(st, base_ccy = "USD")
  pl_usd <- build_full_client_payload(st, res_usd)
  sc_usd <- pl_usd$scenario_evaluation

  expect_true(all(c("CHF", "EUR") %in% sc_usd$relevant_hedge_currencies))
  expect_false("USD" %in% sc_usd$relevant_hedge_currencies)
  expect_true(all(c("CHF", "EUR", "GBP") %in% sc_usd$available_hedge_currencies))
  expect_false("USD" %in% sc_usd$available_hedge_currencies)
})

test_that("Portfolio save stores and restores hedge quotes across both ASSET_CLASS and CURRENCY_OVERLAY modes", {
  st <- create_session_state(TEST_CONFIG)
  load_dataset_into_state(st, TEST_DATA_PATH)

  # 1. Save custom portfolio in ASSET_CLASS mode with class ratios and asset override
  w_custom1 <- list("MSSWITL(RI)" = 0.5, "BMUS10Y(RI)" = 0.5)
  hc_custom1 <- list(
    mode = "ASSET_CLASS",
    class_ratios = list(EQUITY = 0.5, BOND = 0.8, CASH = 0.0),
    asset_overrides = list("BMUS10Y(RI)" = 0.9),
    currency_ratios = list(EUR = 0.3, USD = 0.7),
    configs = list(
      ASSET_CLASS = list(mode = "ASSET_CLASS", class_ratios = list(EQUITY = 0.5, BOND = 0.8, CASH = 0.0), asset_overrides = list("BMUS10Y(RI)" = 0.9)),
      CURRENCY_OVERLAY = list(mode = "CURRENCY_OVERLAY", currency_ratios = list(EUR = 0.3, USD = 0.7))
    )
  )

  resp_save1 <- handle_app_command(list(
    protocol_version = 1L, request_id = "save_ac", type = "portfolio.save",
    payload = list(portfolio_id = "port_class_hedged", name = "Klassen Hedge Portfolio", weights = w_custom1, hedge_config = hc_custom1)
  ), st)
  expect_equal(resp_save1$status, "ok")

  p1 <- st$portfolios[["port_class_hedged"]]
  expect_equal(p1$hedge_config$mode, "ASSET_CLASS")
  expect_equal(p1$hedge_config$class_ratios$BOND, 0.8)
  expect_equal(p1$hedge_config$asset_overrides[["BMUS10Y(RI)"]], 0.9)
  expect_equal(p1$hedge_config$configs$CURRENCY_OVERLAY$currency_ratios$USD, 0.7)

  # Check that evaluation applied the 0.9 hedge on BMUS10Y(RI)
  ev1 <- resp_save1$result$portfolio_evaluation
  row_bmus <- Filter(function(x) x$asset_id == "BMUS10Y(RI)", ev1$exposures$asset_table)[[1]]
  expect_equal(row_bmus$applied_h, 0.9)

  # 2. Save custom portfolio in CURRENCY_OVERLAY mode
  w_custom2 <- list("MSUSAM$(RI)" = 0.5, "MSEMUIL(RI)" = 0.3, "CASH_CHF_TR" = 0.2)
  hc_custom2 <- list(
    mode = "CURRENCY_OVERLAY",
    currency_ratios = list(USD = 1.0, EUR = 0.0, GBP = 0.5),
    configs = list(
      ASSET_CLASS = list(mode = "ASSET_CLASS", class_ratios = list(EQUITY = 0.0, BOND = 1.0)),
      CURRENCY_OVERLAY = list(mode = "CURRENCY_OVERLAY", currency_ratios = list(USD = 1.0, EUR = 0.0, GBP = 0.5))
    )
  )

  resp_save2 <- handle_app_command(list(
    protocol_version = 1L, request_id = "save_ov", type = "portfolio.save",
    payload = list(portfolio_id = "port_overlay_hedged", name = "Overlay Hedge Portfolio", weights = w_custom2, hedge_config = hc_custom2)
  ), st)
  expect_equal(resp_save2$status, "ok")

  p2 <- st$portfolios[["port_overlay_hedged"]]
  expect_equal(p2$hedge_config$mode, "CURRENCY_OVERLAY")
  expect_equal(p2$hedge_config$currency_ratios$USD, 1.0)
  expect_equal(p2$hedge_config$currency_ratios$EUR, 0.0)

  # 3. Switch back to port_class_hedged without specifying hedge_config in payload
  resp_eval1 <- handle_app_command(list(
    protocol_version = 1L, request_id = "eval_ac", type = "portfolio.evaluate",
    payload = list(portfolio_id = "port_class_hedged")
  ), st)
  expect_equal(resp_eval1$status, "ok")
  expect_equal(st$hedge_mode, "ASSET_CLASS")
  expect_equal(st$hedge_configs$ASSET_CLASS$class_ratios$BOND, 0.8)
  expect_equal(st$hedge_configs$ASSET_CLASS$asset_overrides[["BMUS10Y(RI)"]], 0.9)

  # 4. Multi-SAA comparison includes each portfolio with its OWN hedge settings and unhedged variants
  comp <- resp_eval1$result$comparison
  expect_true(!is.null(comp$summary_table))
  p_ids <- vapply(comp$summary_table, function(x) x$portfolio_id, character(1))
  expect_true(all(c("port_class_hedged", "port_overlay_hedged") %in% p_ids))
  expect_true(all(c("port_class_hedged_unhedged", "port_overlay_hedged_unhedged") %in% p_ids))
  expect_true(!is.null(comp$pairwise_tracking_error$portfolio_ids))
  expect_true(all(c("port_class_hedged", "port_class_hedged_unhedged") %in% comp$pairwise_tracking_error$portfolio_ids))
  expect_equal(length(comp$pairwise_tracking_error$matrix), length(comp$pairwise_tracking_error$portfolio_ids))

  # 5. Verify CMA return metrics in comparison summary table
  expect_true("cma_return_hedged_ann" %in% names(comp$summary_table[[1L]]))
  expect_true("cma_sharpe_ratio" %in% names(comp$summary_table[[1L]]))
  expect_true(is.numeric(comp$summary_table[[1L]]$cma_return_hedged_ann))

  # 6. Verify overriding asset return in scenarios.evaluate
  resp_scen <- handle_app_command(list(
    protocol_version = 1L, request_id = "scen_ovr", type = "scenarios.evaluate",
    payload = list(
      scenario_id = "base_5y",
      asset_return_overrides = list(
        "BMUS10Y(RI)" = 0.045
      )
    )
  ), st)
  expect_equal(resp_scen$status, "ok")
  asset_tbl <- resp_scen$result$scenario_evaluation$asset_table
  m_row <- asset_tbl[[which(vapply(asset_tbl, function(x) identical(x$asset_id, "BMUS10Y(RI)"), logical(1)))[1L]]]
  expect_equal(round(as.numeric(m_row$cagr_local_5y), 3), 0.045)

  # 7. Verify CMA set save and delete
  resp_cma_save <- handle_app_command(list(
    protocol_version = 1L, request_id = "cma_s", type = "cma.save",
    payload = list(
      set_id = "cma_custom_test",
      name = "Custom Bullish CMA",
      scenario_id = "base_5y",
      asset_returns = list("MSSWITL(RI)" = 0.08),
      carry_overrides = list("USD" = -0.01)
    )
  ), st)
  expect_equal(resp_cma_save$status, "ok")
  expect_true("cma_custom_test" %in% names(st$custom_cma_sets))
  expect_equal(st$custom_cma_sets$cma_custom_test$name, "Custom Bullish CMA")

  resp_cma_del <- handle_app_command(list(
    protocol_version = 1L, request_id = "cma_d", type = "cma.delete",
    payload = list(set_id = "cma_custom_test")
  ), st)
  expect_equal(resp_cma_del$status, "ok")
  expect_false("cma_custom_test" %in% names(st$custom_cma_sets))
})
