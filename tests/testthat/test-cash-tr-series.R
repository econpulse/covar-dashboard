# tests/testthat/test-cash-tr-series.R
# Tests for dynamic Cash Total Return (TR) series derivation and integration

test_that("build_cash_tr_series correctly derives compounded index series from CASH_RATE", {
  # Mock monthly cash rate data for 4 consecutive months:
  # month 1: rate = 2.0% (at t-1, so month 2 earns 2.0% / 12)
  # month 2: rate = 2.4% (month 3 earns 2.4% / 12)
  # month 3: rate = 1.2% (month 4 earns 1.2% / 12)
  # month 4: rate = 1.5%
  dates <- as.Date(c("2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30"))
  m_idx <- c(241L, 242L, 243L, 244L)
  m_key <- c("2020-01", "2020-02", "2020-03", "2020-04")
  rates <- c(2.0, 2.4, 1.2, 1.5)

  mock_data <- data.table::data.table(
    Code = "CHF1M",
    date = dates,
    month_key = m_key,
    month_index = m_idx,
    value = rates
  )

  mock_meta <- data.table::data.table(
    Code = "CHF1M",
    Name = "CHF 1M Libor/SARON",
    Series_Type = "CASH_RATE",
    Cash_Currency = "CHF",
    Tenor = "1M",
    Active = TRUE,
    Rate_Convention = "SIMPLE_1_12",
    Day_Count = "30_360",
    Proxy_Type = NA_character_,
    Proxy_Until = NA_character_
  )

  mock_registry <- list(
    data = mock_data,
    meta = mock_meta,
    coverage = data.table::data.table(Code = "CHF1M"),
    active_assets = character(0),
    all_assets = character(0)
  )

  cash_bundle <- build_cash_returns(mock_registry)
  expect_true(cash_bundle$valid)
  expect_equal(nrow(cash_bundle$cash), 4L)

  tr_bundle <- build_cash_tr_series(cash_bundle, mock_registry, start_level = 100.0)
  expect_true(tr_bundle$valid)
  expect_equal(tr_bundle$active_codes, "CASH_CHF_TR")
  expect_equal(tr_bundle$all_codes, "CASH_CHF_TR")

  # Verify meta
  meta_tr <- tr_bundle$meta[Code == "CASH_CHF_TR"]
  expect_equal(nrow(meta_tr), 1L)
  expect_equal(meta_tr$Asset_Class, "CASH")
  expect_equal(meta_tr$Return_Structure, "CASH_TR")
  expect_equal(meta_tr$Return_Currency, "CHF")
  expect_equal(meta_tr$FX_Exposure, "CHF")
  expect_true(meta_tr$Hedge_Allowed)
  expect_equal(meta_tr$Default_Hedge_Ratio, 0.0)

  # Verify values compounding
  # t=1: 100.0
  # t=2: 100.0 * (1 + 0.02 / 12) = 100 * (1 + 0.001666667) = 100.166667
  # t=3: 100.166667 * (1 + 0.024 / 12) = 100.166667 * 1.002 = 100.367000
  # t=4: 100.367000 * (1 + 0.012 / 12) = 100.367000 * 1.001 = 100.467367
  vals <- tr_bundle$data$value
  expect_equal(vals[1], 100.0)
  expect_equal(vals[2], 100.0 * (1 + 0.02 / 12), tolerance = 1e-6)
  expect_equal(vals[3], 100.0 * (1 + 0.02 / 12) * (1 + 0.024 / 12), tolerance = 1e-6)
  expect_equal(vals[4], 100.0 * (1 + 0.02 / 12) * (1 + 0.024 / 12) * (1 + 0.012 / 12), tolerance = 1e-6)

  # Check coverage dates are Date class (critical for excel export!)
  expect_s3_class(tr_bundle$coverage$Start_Date, "Date")
  expect_s3_class(tr_bundle$coverage$End_Date, "Date")
})

test_that("attach_cash_tr_to_registry integrates cash TR into registry safely", {
  base_reg <- list(
    data = data.table::data.table(Code = "EQ_CH", date = as.Date("2020-01-31"), month_key = "2020-01", month_index = 241L, value = 100),
    meta = data.table::data.table(Code = "EQ_CH", Asset_Class = "EQUITY", Active = TRUE),
    coverage = data.table::data.table(Code = "EQ_CH", Obs_Count = 1L),
    active_assets = "EQ_CH",
    all_assets = "EQ_CH"
  )

  cash_tr <- list(
    valid = TRUE,
    data = data.table::data.table(Code = "CASH_CHF_TR", date = as.Date("2020-01-31"), month_key = "2020-01", month_index = 241L, value = 100),
    meta = data.table::data.table(Code = "CASH_CHF_TR", Asset_Class = "CASH", Active = TRUE),
    coverage = data.table::data.table(Code = "CASH_CHF_TR", Obs_Count = 1L, Start_Date = as.Date("2020-01-31"), End_Date = as.Date("2020-01-31")),
    active_codes = "CASH_CHF_TR",
    all_codes = "CASH_CHF_TR"
  )

  updated_reg <- attach_cash_tr_to_registry(base_reg, cash_tr)
  expect_true("CASH_CHF_TR" %in% updated_reg$active_assets)
  expect_true("CASH_CHF_TR" %in% updated_reg$all_assets)
  expect_equal(nrow(updated_reg$data), 2L)
  expect_equal(nrow(updated_reg$meta), 2L)
})

test_that("EUR cash rate with DEM proxy pre-1999 handles proxy toggle appropriately", {
  # Mock ECWGM1M spanning 1998-11 to 1999-02
  dates <- as.Date(c("1998-11-30", "1998-12-31", "1999-01-31", "1999-02-28"))
  m_idx <- c(1L, 2L, 3L, 4L)
  m_key <- c("1998-11", "1998-12", "1999-01", "1999-02")
  rates <- c(3.0, 3.2, 3.1, 2.9)

  mock_reg <- list(
    data = data.table::data.table(Code = "ECWGM1M", date = dates, month_key = m_key, month_index = m_idx, value = rates),
    meta = data.table::data.table(
      Code = "ECWGM1M", Name = "EUR 1M Deposit", Series_Type = "CASH_RATE",
      Cash_Currency = "EUR", Tenor = "1M", Active = TRUE, Rate_Convention = "SIMPLE_1_12",
      Day_Count = "30_360", Proxy_Type = "DEM_DEPOSIT_PROXY", Proxy_Until = "1998-12-31"
    ),
    coverage = data.table::data.table(Code = "ECWGM1M"),
    active_assets = character(0),
    all_assets = character(0)
  )

  # With proxy enabled (default)
  cb_enabled <- build_cash_returns(mock_reg, policy = list(cash_proxy_config = list(EUR = list(allow_pre_1999_proxy = TRUE))))
  tr_enabled <- build_cash_tr_series(cb_enabled, mock_reg)
  expect_true(tr_enabled$valid)
  expect_equal(nrow(tr_enabled$data), 4L)
  # Level starts at 100 on 1998-11-30 and compounds continuously
  expect_equal(tr_enabled$data$value[1], 100.0)
  expect_true(all(!is.na(tr_enabled$data$value)))

  # With proxy disabled
  cb_disabled <- build_cash_returns(mock_reg, policy = list(cash_proxy_config = list(EUR = list(allow_pre_1999_proxy = FALSE))))
  tr_disabled <- build_cash_tr_series(cb_disabled, mock_reg)
  expect_true(tr_disabled$valid)
  # Observations before 1999-01-01 should be skipped or start only from 1999-01-31
  sub_post <- tr_disabled$data[date >= as.Date("1999-01-01")]
  expect_true(nrow(sub_post) >= 1L)
})

test_that("Session state integration loads cash TR series into active assets from real Data.xlsx", {
  skip_if_not(file.exists(TEST_DATA_PATH), "Data.xlsx not found")

  st <- create_session_state(TEST_CONFIG)
  ld <- load_dataset_into_state(st, TEST_DATA_PATH, is_private = FALSE)
  expect_true(ld$valid)
  expect_true("CASH_CHF_TR" %in% st$dataset$active_assets)
  expect_true("CASH_EUR_TR" %in% st$dataset$active_assets)
  expect_true("CASH_USD_TR" %in% st$dataset$active_assets)
  expect_true("CASH_GBP_TR" %in% st$dataset$active_assets)

  # Verify CASH metadata in registry
  meta_cash <- st$dataset$meta[Code %in% c("CASH_CHF_TR", "CASH_EUR_TR", "CASH_USD_TR", "CASH_GBP_TR")]
  expect_equal(nrow(meta_cash), 4L)
  expect_true(all(meta_cash$Asset_Class == "CASH"))
  expect_true(all(meta_cash$Return_Structure == "CASH_TR"))
  expect_true(all(meta_cash$Hedge_Allowed == TRUE))

  # Verify CMA scenario returns for CASH assets fallback to short rates or scenario assumptions
  weights <- c(CASH_CHF_TR = 0.5, CASH_USD_TR = 0.5)
  scen_eval <- evaluate_return_scenarios(
    expectations = st$expectations,
    weights = weights,
    hedge_config = st$hedge_configs$ASSET_CLASS,
    meta = st$dataset$meta,
    base_ccy = "CHF",
    scenario_id = "base_5y"
  )
  expect_true(scen_eval$valid)
  expect_true("CASH_CHF_TR" %in% scen_eval$asset_table$asset_id)
  row_chf <- scen_eval$asset_table[asset_id == "CASH_CHF_TR"]
  expect_equal(row_chf$arith_mean_local_pa, 0.0094) # mean of c(0.008, 0.009, 0.010, 0.010, 0.010)
  expect_equal(row_chf$cagr_local_5y, ((1.008 * 1.009 * 1.010^3)^(1/5)) - 1, tolerance = 1e-6)
})
