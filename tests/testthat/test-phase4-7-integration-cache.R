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
