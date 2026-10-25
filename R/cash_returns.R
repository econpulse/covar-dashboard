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
