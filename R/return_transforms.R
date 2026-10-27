# ==============================================================================
# R/return_transforms.R
# Monthly local TR returns, unhedged base-currency returns u_B, and
# 1M forward hedge PnL z_B (with carry and spot_offset audit decomposition)
# Pure R module - no Shiny dependencies
# ==============================================================================

index_levels_to_returns <- function(levels, meta = NULL) {
  dt <- data.table::as.data.table(levels)
  if (!is.null(meta)) {
    asset_codes <- meta[Series_Type %in% c("ASSET_TR", "FACTOR", "FACTOR_DONOR"), Code]
    dt <- dt[Code %in% asset_codes]
  }
  data.table::setorder(dt, Code, month_index)

  # Strict 1-month return: only when month_index[t] - month_index[t-1] == 1L
  # and both TR_t and TR_t-1 are non-NA and > 0. Never bridge across gaps!
  dt[, `:=`(
    prev_value = data.table::shift(value, n = 1L, type = "lag"),
    prev_month_index = data.table::shift(month_index, n = 1L, type = "lag")
  ), by = Code]

  dt[, is_consecutive := (!is.na(prev_month_index)) & ((month_index - prev_month_index) == 1L)]
  dt[, r_local := ifelse(
    is_consecutive & !is.na(value) & !is.na(prev_value) & prev_value > 0 & value > 0,
    (value / prev_value) - 1.0,
    NA_real_
  )]

  out <- dt[, .(
    date = date,
    month_key = month_key,
    month_index = month_index,
    Code = Code,
    level_t = value,
    level_t_1 = ifelse(is_consecutive, prev_value, NA_real_),
    r_local = r_local
  )]
  data.table::setorder(out, Code, month_index)
  out
}

translate_asset_returns <- function(returns, crosses, meta) {
  ret_dt <- data.table::as.data.table(returns)
  cross_dt <- if (is.data.frame(crosses)) data.table::as.data.table(crosses) else crosses$crosses
  meta_dt <- data.table::as.data.table(meta)
  base_ccy <- unique(cross_dt$base_currency)[1L]

  # Compute consecutive spot returns S_t / S_{t-1} per currency
  spot_dt <- data.table::copy(cross_dt[, .(month_index, currency, S_t = S_b_per_c, is_domestic)])
  data.table::setorder(spot_dt, currency, month_index)
  spot_dt[, `:=`(
    S_t_1 = data.table::shift(S_t, n = 1L, type = "lag"),
    prev_m = data.table::shift(month_index, n = 1L, type = "lag")
  ), by = currency]
  spot_dt[, is_consec := (!is.na(prev_m)) & ((month_index - prev_m) == 1L)]
  spot_dt[, fx_spot_return := ifelse(
    is_consec & !is.na(S_t) & !is.na(S_t_1) & S_t_1 > 0,
    (S_t / S_t_1) - 1.0,
    NA_real_
  )]

  asset_meta <- meta_dt[Series_Type %in% c("ASSET_TR", "FACTOR", "FACTOR_DONOR"), .(
    Code, Name, Asset_Class, Sub_Asset_Class, Return_Currency,
    FX_Exposure, Hedge_Allowed, Default_Hedge_Ratio, Return_Structure, Active
  )]

  m <- merge(ret_dt, asset_meta, by = "Code", all.x = TRUE, sort = FALSE)
  m <- merge(
    m,
    spot_dt[, .(month_index, Return_Currency = currency, S_t, S_t_1, fx_spot_return)],
    by = c("month_index", "Return_Currency"),
    all.x = TRUE,
    sort = FALSE
  )
  data.table::setorder(m, Code, month_index)

  # Unhedged base-currency return:
  # u_{i,t}^B = (1 + r_{L,i,t}) * (S_t^(B/C) / S_{t-1}^(B/C)) - 1
  # Decomposition: u = r_local + fx_spot_return + cross_term
  m[, `:=`(
    base_currency = base_ccy,
    cross_term = ifelse(!is.na(r_local) & !is.na(fx_spot_return), r_local * fx_spot_return, NA_real_),
    u_base = ifelse(
      !is.na(r_local) & !is.na(fx_spot_return),
      (1.0 + r_local) * (1.0 + fx_spot_return) - 1.0,
      NA_real_
    )
  )]

  m[, .(
    date, month_key, month_index, Code, Name, Asset_Class,
    Return_Currency, FX_Exposure, Hedge_Allowed, Return_Structure, Active,
    base_currency, r_local, fx_spot_return, cross_term, u_base
  )]
}

build_forward_pnl <- function(crosses, cost_bps_pa = 0.0) {
  cross_dt <- if (is.data.frame(crosses)) data.table::as.data.table(crosses) else crosses$crosses
  base_ccy <- unique(cross_dt$base_currency)[1L]
  dt <- data.table::copy(cross_dt)
  data.table::setorder(dt, currency, month_index)

  # Strict t-1 lag for S_{t-1}^(B/C) and F_{t-1}^(B/C)
  dt[, `:=`(
    S_t = S_b_per_c,
    S_t_1 = data.table::shift(S_b_per_c, n = 1L, type = "lag"),
    F_t_1 = data.table::shift(F_b_per_c, n = 1L, type = "lag"),
    prev_m = data.table::shift(month_index, n = 1L, type = "lag")
  ), by = currency]

  dt[, is_consec := (!is.na(prev_m)) & ((month_index - prev_m) == 1L)]
  dt[, valid_pair := is_consec & !is.na(S_t) & !is.na(S_t_1) & !is.na(F_t_1) & S_t_1 > 0]

  # Decomposition:
  # carry = F_{t-1} / S_{t-1} - 1
  # spot_offset = -(S_t / S_{t-1} - 1)
  # z_gross = (F_{t-1} - S_t) / S_{t-1} = carry + spot_offset
  monthly_cost <- (as.numeric(cost_bps_pa %||% 0.0) / 10000.0) / 12.0

  dt[, `:=`(
    carry = ifelse(valid_pair, (F_t_1 / S_t_1) - 1.0, NA_real_),
    spot_offset = ifelse(valid_pair, -((S_t / S_t_1) - 1.0), NA_real_),
    z_gross = ifelse(valid_pair, (F_t_1 - S_t) / S_t_1, NA_real_),
    tc_drag = ifelse(valid_pair & !is_domestic, monthly_cost, ifelse(valid_pair & is_domestic, 0.0, NA_real_))
  )]
  dt[, z_net := ifelse(!is.na(z_gross), z_gross - tc_drag, NA_real_)]
  dt[, component_id := paste0("HEDGE_", currency)]

  # Exclude domestic currency from hedge components (since B = C has 0 hedge PnL and no hedge leg)
  foreign_dt <- dt[is_domestic == FALSE, .(
    date, month_key, month_index, base_currency, currency, component_id,
    S_t, S_t_1, F_t_1, carry, spot_offset, tc_drag, z_gross, z_base = z_net
  )]
  all_dt <- dt[, .(
    date, month_key, month_index, base_currency, currency, is_domestic, component_id,
    S_t, S_t_1, F_t_1, carry, spot_offset, tc_drag, z_gross, z_base = z_net
  )]

  list(
    base_currency = base_ccy,
    cost_bps_pa = as.numeric(cost_bps_pa %||% 0.0),
    foreign_pnl = foreign_dt,
    all_pnl = all_dt
  )
}
