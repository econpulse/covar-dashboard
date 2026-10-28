# ==============================================================================
# R/cash_returns.R
# Monthly cash return derivation from annualized % p.a. deposit rates,
# structured EUR pre-1999 cash proxy handling, and portfolio excess returns
# Pure R module - no Shiny dependencies
# ==============================================================================

build_cash_returns <- function(registry, policy = NULL, convention_override = NULL) {
  issues <- list()
  meta <- registry$meta
  data <- registry$data

  cash_meta <- meta[Series_Type == "CASH_RATE" & Active == TRUE]
  if (nrow(cash_meta) == 0L) {
    return(list(valid = TRUE, cash = data.table::data.table(), issues = issues))
  }

  allow_eur_proxy <- isTRUE(policy$cash_proxy_config$EUR$allow_pre_1999_proxy %||% TRUE)
  out_list <- vector("list", nrow(cash_meta))

  for (i in seq_len(nrow(cash_meta))) {
    row <- cash_meta[i]
    sc <- row$Code
    ccy <- row$Cash_Currency
    proxy_type <- row$Proxy_Type
    proxy_until <- if (!is.na(row$Proxy_Until)) as.Date(row$Proxy_Until) else as.Date("1998-12-31")
    rate_conv <- convention_override %||% (if (!is.na(row$Rate_Convention) && row$Rate_Convention != "UNKNOWN") row$Rate_Convention else "SIMPLE_1_12")
    day_cnt <- if (!is.na(row$Day_Count) && row$Day_Count != "UNKNOWN") row$Day_Count else "30_360"

    if (is.na(ccy) && identical(sc, "ECWGM1M") && allow_eur_proxy) {
      ccy <- "EUR"
      proxy_type <- proxy_type %||% "DEM_DEPOSIT_PROXY"
    }

    if (is.na(ccy)) {
      next
    }

    sub <- data.table::copy(data[Code == sc])
    data.table::setorder(sub, month_index)

    # Deposit rate quoted at end of t-1 earns over month t
    sub[, `:=`(
      rate_pa_t_1 = data.table::shift(value, n = 1L, type = "lag"),
      date_t_1 = data.table::shift(date, n = 1L, type = "lag"),
      prev_m = data.table::shift(month_index, n = 1L, type = "lag")
    )]
    sub[, is_consec := (!is.na(prev_m)) & ((month_index - prev_m) == 1L)]
    sub[, days_in_period := as.numeric(date - date_t_1)]

    # If EUR proxy before 1999 is disabled, mask out observations before 1999-01-01
    if (identical(ccy, "EUR") && !is.na(proxy_type) && !allow_eur_proxy) {
      sub[date <= proxy_until, rate_pa_t_1 := NA_real_]
    }

    sub[, r_cash := {
      r_dec <- rate_pa_t_1 / 100.0
      val <- if (identical(rate_conv, "ACT_360") || identical(day_cnt, "ACT_360")) {
        r_dec * (days_in_period / 360.0)
      } else if (identical(rate_conv, "ACT_365") || identical(day_cnt, "ACT_365")) {
        r_dec * (days_in_period / 365.0)
      } else if (identical(rate_conv, "COMPOUND_1_12")) {
        (1.0 + r_dec)^(1.0 / 12.0) - 1.0
      } else {
        # Standard 1/12 monthly convention (30/360)
        r_dec / 12.0
      }
      ifelse(is_consec & !is.na(rate_pa_t_1), val, NA_real_)
    }]

    sub[, `:=`(
      cash_currency = ccy,
      source_code = sc,
      rate_convention = rate_conv,
      is_proxy = (!is.na(proxy_type)) & (date <= proxy_until),
      proxy_type = ifelse((!is.na(proxy_type)) & (date <= proxy_until), proxy_type, NA_character_)
    )]

    out_list[[i]] <- sub[, .(
      date, month_key, month_index, cash_currency, source_code,
      rate_pa_t = value, rate_pa_t_1, r_cash, rate_convention, is_proxy, proxy_type
    )]
  }

  cash_dt <- data.table::rbindlist(out_list, fill = TRUE)
  data.table::setorder(cash_dt, cash_currency, month_index)

  list(
    valid = TRUE,
    cash = cash_dt,
    issues = issues
  )
}

apply_excess_returns <- function(u_dt, cash_dt, base_ccy = "CHF") {
  # Section 8 rule:
  # In Excess Return representation, subtract base-currency cash return ONCE
  # from every asset return u_{i,t}^B, and NEVER from hedge components z_{C,t}^B.
  # Since Sum(w) = 1, portfolio excess return w'(u - r_cash) + g'z = w'u + g'z - r_cash.
  u_copy <- data.table::copy(data.table::as.data.table(u_dt))
  c_dt <- if (is.data.frame(cash_dt)) data.table::as.data.table(cash_dt) else cash_dt$cash

  sub_cash <- c_dt[cash_currency == base_ccy, .(month_index, r_cash_base = r_cash, is_cash_proxy = is_proxy)]
  m <- merge(u_copy, sub_cash, by = "month_index", all.x = TRUE, sort = FALSE)
  data.table::setorder(m, Code, month_index)

  m[, `:=`(
    u_total_base = u_base,
    u_excess_base = ifelse(!is.na(u_base) & !is.na(r_cash_base), u_base - r_cash_base, NA_real_),
    u_base = ifelse(!is.na(u_base) & !is.na(r_cash_base), u_base - r_cash_base, NA_real_)
  )]
  m
}

build_cash_tr_series <- function(cash_bundle, registry, policy = NULL, start_level = 100.0) {
  issues <- list()
  if (is.null(cash_bundle) || is.null(cash_bundle$cash) || nrow(cash_bundle$cash) == 0L) {
    return(list(valid = TRUE, data = data.table::data.table(), meta = data.table::data.table(), active_codes = character(0), all_codes = character(0), coverage = data.table::data.table(), issues = issues))
  }

  cash_dt <- data.table::as.data.table(cash_bundle$cash)
  orig_meta <- data.table::as.data.table(registry$meta)

  cash_meta_rows <- orig_meta[Series_Type == "CASH_RATE" & Active == TRUE]
  if (nrow(cash_meta_rows) == 0L) {
    return(list(valid = TRUE, data = data.table::data.table(), meta = data.table::data.table(), active_codes = character(0), all_codes = character(0), coverage = data.table::data.table(), issues = issues))
  }

  data_list <- list()
  meta_list <- list()
  cov_list <- list()
  active_codes <- character(0)
  all_codes <- character(0)

  region_for_ccy <- function(c) {
    switch(c,
      "CHF" = "SWITZERLAND",
      "EUR" = "EUROZONE",
      "USD" = "USA",
      "GBP" = "UNITED_KINGDOM",
      "JPY" = "JAPAN",
      "CAD" = "CANADA",
      "AUD" = "AUSTRALIA",
      "GLOBAL"
    )
  }

  for (i in seq_len(nrow(cash_meta_rows))) {
    cm <- cash_meta_rows[i]
    sc <- cm$Code
    ccy <- cm$Cash_Currency
    if (is.na(ccy) && identical(sc, "ECWGM1M")) {
      ccy <- "EUR"
    }
    if (is.na(ccy) || !nzchar(trimws(ccy))) next

    sub_cash <- cash_dt[source_code == sc]
    if (nrow(sub_cash) == 0L) {
      sub_cash <- cash_dt[cash_currency == ccy]
    }
    if (nrow(sub_cash) == 0L) next

    data.table::setorder(sub_cash, month_index)

    tenor_str <- if (!is.na(cm$Tenor) && nzchar(trimws(cm$Tenor))) toupper(trimws(cm$Tenor)) else "1M"
    tr_code <- if (identical(tenor_str, "1M")) {
      paste0("CASH_", ccy, "_TR")
    } else {
      paste0("CASH_", ccy, "_", tenor_str, "_TR")
    }

    if (tr_code %in% orig_meta[Series_Type == "ASSET_TR", Code]) {
      next
    }

    n_obs <- nrow(sub_cash)
    levels <- numeric(n_obs)
    levels[1L] <- as.numeric(start_level %||% 100.0)

    for (k in 2L:n_obs) {
      r_k <- sub_cash$r_cash[k]
      levels[k] <- if (!is.na(r_k) && !is.na(levels[k - 1L])) {
        levels[k - 1L] * (1.0 + r_k)
      } else {
        levels[k - 1L]
      }
    }

    tr_data <- data.table::data.table(
      date = sub_cash$date,
      month_key = sub_cash$month_key,
      month_index = sub_cash$month_index,
      Code = tr_code,
      value = round(levels, 4)
    )

    tr_name <- sprintf("Cash %s %s TR%s", ccy, tenor_str, if (identical(sc, "ECWGM1M")) " (inkl. DEM pre-1999)" else "")
    reg_val <- region_for_ccy(ccy)

    tr_meta <- data.table::data.table(
      Code = tr_code,
      Name = tr_name,
      Series_Type = "ASSET_TR",
      Value_Unit = "INDEX_POINTS",
      Active = TRUE,
      Review_Status = "BERECHNET",
      Asset_Class = "CASH",
      Sub_Asset_Class = paste0("MONEY_MARKET_", tenor_str),
      Region = reg_val,
      Return_Currency = ccy,
      FX_Exposure = ccy,
      Hedge_Allowed = TRUE,
      Default_Hedge_Ratio = 0.0,
      TR_Convention = "MONTHLY_COMPOUNDED",
      Already_Hedged = FALSE,
      Existing_Hedge_Currency = NA_character_,
      Return_Structure = "CASH_TR",
      Foreign_Currency = NA_character_,
      Quote_Currency = NA_character_,
      Quote_Direction = NA_character_,
      Quote_Scale = NA_real_,
      Forward_Type = NA_character_,
      Points_Multiplier = NA_real_,
      Tenor = tenor_str,
      Cash_Currency = ccy,
      Rate_Convention = cm$Rate_Convention %||% "SIMPLE_1_12",
      Day_Count = cm$Day_Count %||% "30_360",
      Spot_Reference_Code = NA_character_,
      Source = sc,
      Notes = sprintf("Synthetische %s-Cash TR Reihe aus %s (Start 100)", tenor_str, sc)
    )

    tr_cov <- data.table::data.table(
      Code = tr_code,
      Start_Date = min(sub_cash$date),
      End_Date = max(sub_cash$date),
      Start_Month = min(sub_cash$month_key),
      End_Month = max(sub_cash$month_key),
      Obs_Count = as.integer(n_obs),
      Leading_NA = 0L,
      Trailing_NA = 0L,
      Interior_Gaps = 0L,
      Min_Value = as.numeric(min(levels, na.rm = TRUE)),
      Max_Value = as.numeric(max(levels, na.rm = TRUE))
    )

    data_list[[length(data_list) + 1L]] <- tr_data
    meta_list[[length(meta_list) + 1L]] <- tr_meta
    cov_list[[length(cov_list) + 1L]] <- tr_cov
    active_codes <- c(active_codes, tr_code)
    all_codes <- c(all_codes, tr_code)
  }

  out_data <- if (length(data_list) > 0L) data.table::rbindlist(data_list, fill = TRUE) else data.table::data.table()
  out_meta <- if (length(meta_list) > 0L) data.table::rbindlist(meta_list, fill = TRUE) else data.table::data.table()
  out_cov <- if (length(cov_list) > 0L) data.table::rbindlist(cov_list, fill = TRUE) else data.table::data.table()

  list(
    valid = TRUE,
    data = out_data,
    meta = out_meta,
    coverage = out_cov,
    active_codes = active_codes,
    all_codes = all_codes,
    issues = issues
  )
}

attach_cash_tr_to_registry <- function(reg, cash_tr) {
  if (is.null(cash_tr) || !isTRUE(cash_tr$valid) || nrow(cash_tr$data) == 0L) {
    return(reg)
  }

  new_codes <- setdiff(cash_tr$all_codes, reg$all_assets)
  if (length(new_codes) == 0L) {
    return(reg)
  }

  new_data <- cash_tr$data[Code %in% new_codes]
  new_meta <- cash_tr$meta[Code %in% new_codes]
  new_cov <- cash_tr$coverage[Code %in% new_codes]

  reg$data <- data.table::rbindlist(list(reg$data, new_data), fill = TRUE)
  data.table::setkey(reg$data, Code)

  meta_combined <- data.table::copy(new_meta)
  if (nrow(new_cov) > 0L) {
    target_cols <- c("Code", "Start_Date", "End_Date", "Start_Month", "End_Month", "Obs_Count", "Leading_NA", "Trailing_NA", "Interior_Gaps", "Min_Value", "Max_Value")
    avail_cols <- intersect(names(new_cov), target_cols)
    if ("Code" %in% avail_cols && length(avail_cols) > 1L) {
      meta_combined <- merge(
        meta_combined,
        new_cov[, ..avail_cols],
        by = "Code",
        all.x = TRUE,
        sort = FALSE
      )
    }
  }
  reg$meta <- data.table::rbindlist(list(reg$meta, meta_combined), fill = TRUE)
  reg$active_assets <- unique(c(reg$active_assets, cash_tr$active_codes))
  reg$all_assets <- unique(c(reg$all_assets, cash_tr$all_codes))
  reg$coverage <- data.table::rbindlist(list(reg$coverage, new_cov), fill = TRUE)

  reg
}
