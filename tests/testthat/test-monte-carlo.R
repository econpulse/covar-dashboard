# ==============================================================================
# tests/testthat/test-monte-carlo.R
# Unit and integration tests for forward-looking 5-year Monte Carlo simulation
# ==============================================================================

test_that("Monte Carlo simulation runs correctly with Student-t and produces valid quantiles and downside metrics", {
  cache <- create_cache_manager(max_size_mb = 128)
  st <- create_session_state(TEST_CONFIG, shared_cache = cache)
  ld <- load_dataset_into_state(st, TEST_DATA_PATH, is_private = FALSE)
  expect_true(ld$valid)

  # Estimate model to obtain Sigma_monthly and component_ids
  model_obj <- get_or_estimate_covariance(
    state = st,
    model_id = "shrinkage_long",
    base_ccy = "CHF",
    return_mode = "TOTAL",
    universe = st$universe
  )$model
  expect_true(model_obj$valid)

  weights <- TEST_CONFIG$portfolio_templates$saa_50$weights
  hc <- list(
    mode = "ASSET_CLASS",
    class_ratios = list(EQUITY = 0.0, BOND = 1.0, REAL_ESTATE = 0.0, GOLD = 0.0, COMMODITY = 0.0, CASH = 0.0),
    asset_overrides = list()
  )

  # Run simulation with 500 sims for fast testing
  sim_res <- simulate_cma_paths(
    expectations = st$expectations,
    model = model_obj,
    weights = weights,
    hedge_config = hc,
    meta = st$dataset$meta,
    base_ccy = "CHF",
    scenario_id = "base_5y",
    rebalancing_rule = "ANNUAL_REBALANCED",
    n_sims = 500L,
    horizon_years = 5L,
    method = "PARAMETRIC_STUDENT_T",
    df_t = 6.0,
    seed = 42L,
    target_cagr = 0.03
  )

  expect_true(sim_res$valid)
  expect_equal(sim_res$n_sims, 500L)
  expect_equal(sim_res$horizon_years, 5L)
  expect_equal(sim_res$horizon_months, 60L)
  expect_equal(sim_res$method, "PARAMETRIC_STUDENT_T")

  # Fan chart path has exactly 61 rows (Month 0 to Month 60)
  expect_equal(length(sim_res$fan_chart_path), 61L)
  p0 <- sim_res$fan_chart_path[[1L]]
  p60 <- sim_res$fan_chart_path[[61L]]
  expect_equal(p0$month, 0L)
  expect_equal(p0$hedged_p50, 100.0)
  expect_equal(p60$month, 60L)

  # Check quantile monotonicity on month 60
  expect_lte(p60$hedged_p05, p60$hedged_p25)
  expect_lte(p60$hedged_p25, p60$hedged_p50)
  expect_lte(p60$hedged_p50, p60$hedged_p75)
  expect_lte(p60$hedged_p75, p60$hedged_p95)

  # Downside metrics
  m_h <- sim_res$metrics_hedged
  expect_true(is.numeric(m_h$cagr_p50))
  expect_lte(m_h$cagr_p05, m_h$cagr_p50)
  expect_lte(m_h$cagr_p50, m_h$cagr_p95)
  expect_true(m_h$var_95_cagr >= 0.0 || m_h$cagr_p05 > 0.0)
  expect_gte(m_h$cvar_95_cagr, m_h$var_95_cagr)
  expect_lte(m_h$max_drawdown_median, 0.0)
  expect_lte(m_h$max_drawdown_p05, m_h$max_drawdown_median)
  expect_gte(m_h$shortfall_prob, 0.0)
  expect_lte(m_h$shortfall_prob, 1.0)
  expect_gte(m_h$prob_achieve_target, 0.0)
  expect_lte(m_h$prob_achieve_target, 1.0)

  # Histograms exist
  expect_true(is.data.frame(sim_res$cagr_histogram_hedged))
  expect_gt(nrow(sim_res$cagr_histogram_hedged), 10L)
})

test_that("Monte Carlo seed provides exact reproducibility and different seeds produce different draws", {
  cache <- create_cache_manager(max_size_mb = 128)
  st <- create_session_state(TEST_CONFIG, shared_cache = cache)
  ld <- load_dataset_into_state(st, TEST_DATA_PATH, is_private = FALSE)
  model_obj <- get_or_estimate_covariance(st, model_id = "shrinkage_long", base_ccy = "CHF")$model

  weights <- TEST_CONFIG$portfolio_templates$saa_50$weights
  hc <- list(mode = "ASSET_CLASS", class_ratios = list(BOND = 1.0))

  res1 <- simulate_cma_paths(st$expectations, model_obj, weights, hc, meta = st$dataset$meta, seed = 123L, n_sims = 200L)
  res2 <- simulate_cma_paths(st$expectations, model_obj, weights, hc, meta = st$dataset$meta, seed = 123L, n_sims = 200L)
  res3 <- simulate_cma_paths(st$expectations, model_obj, weights, hc, meta = st$dataset$meta, seed = 456L, n_sims = 200L)

  expect_equal(res1$metrics_hedged$cagr_p50, res2$metrics_hedged$cagr_p50)
  expect_equal(res1$metrics_hedged$var_95_cagr, res2$metrics_hedged$var_95_cagr)
  expect_false(identical(res1$metrics_hedged$cagr_p50, res3$metrics_hedged$cagr_p50))
})

test_that("All simulation methods (Student-t, Gaussian, Bootstrap) and rebalancing rules work without errors", {
  cache <- create_cache_manager(max_size_mb = 128)
  st <- create_session_state(TEST_CONFIG, shared_cache = cache)
  ld <- load_dataset_into_state(st, TEST_DATA_PATH, is_private = FALSE)
  model_res <- get_or_estimate_covariance(st, model_id = "shrinkage_long", base_ccy = "CHF")
  model_obj <- model_res$model
  basis_obj <- model_res$sample

  weights <- TEST_CONFIG$portfolio_templates$saa_50$weights
  hc <- list(mode = "ASSET_CLASS", class_ratios = list(BOND = 1.0))

  # 1. Gaussian
  res_gauss <- simulate_cma_paths(
    st$expectations, model_obj, weights, hc, meta = st$dataset$meta,
    method = "PARAMETRIC_GAUSSIAN", n_sims = 200L, seed = 42L
  )
  expect_true(res_gauss$valid)
  expect_equal(res_gauss$method, "PARAMETRIC_GAUSSIAN")

  # 2. Block-Bootstrap with basis
  res_boot <- simulate_cma_paths(
    st$expectations, model_obj, weights, hc, meta = st$dataset$meta,
    method = "BOOTSTRAP_MEAN_SHIFT", basis = basis_obj, n_sims = 200L, seed = 42L
  )
  expect_true(res_boot$valid)
  expect_equal(res_boot$method, "BOOTSTRAP_MEAN_SHIFT")

  # 3. Monthly rebalanced vs Buy & Hold
  res_monthly <- simulate_cma_paths(
    st$expectations, model_obj, weights, hc, meta = st$dataset$meta,
    rebalancing_rule = "MONTHLY_REBALANCED", n_sims = 200L, seed = 42L
  )
  expect_true(res_monthly$valid)

  res_bh <- simulate_cma_paths(
    st$expectations, model_obj, weights, hc, meta = st$dataset$meta,
    rebalancing_rule = "BUY_AND_HOLD", n_sims = 200L, seed = 42L
  )
  expect_true(res_bh$valid)
})
