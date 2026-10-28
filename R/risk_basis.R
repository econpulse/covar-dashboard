# ==============================================================================
# R/risk_basis.R
# Joint risk basis X_B = [u_asset1^B, ..., u_assetN^B, z_ccy1^B, ..., z_ccyK^B]
# and estimation sample resolution per base currency B
# Pure R module - no Shiny dependencies
# ==============================================================================

build_risk_basis <- function(u,
                             z,
                             meta,
                             universe = NULL,
                             base_ccy = "CHF",
                             return_mode = c("TOTAL", "EXCESS"),
                             cash_returns = NULL) {
  return_mode <- match.arg(return_mode)
  issues <- list()

  u_dt <- data.table::copy(data.table::as.data.table(u))
  z_dt <- if (is.data.frame(z)) data.table::copy(data.table::as.data.table(z)) else data.table::copy(z$foreign_pnl)
  meta_dt <- data.table::as.data.table(meta)

  # Determine stable working asset universe from metadata Active flag (or explicit universe vector)
  # Crucial rule (Section 6): Required hedge currencies are derived from the analysis universe
  # and Hedge_Allowed, NEVER from the currently selected slider hedge ratio h!
  if (is.null(universe) || length(universe) == 0L) {
    asset_ids <- meta_dt[Series_Type == "ASSET_TR" & Active == TRUE, Code]
  } else {
    asset_ids <- intersect(as.character(universe), meta_dt[Series_Type == "ASSET_TR", Code])
  }
  asset_ids <- order_asset_universe(asset_ids, meta_dt, base_ccy = base_ccy)

  if (length(asset_ids) == 0L) {
    issues[[1L]] <- make_issue(
      code = "EMPTY_ANALYSIS_UNIVERSE",
      severity = "ERROR",
      message = "Das gewählte Analyseuniversum enthält keine gültigen Assets.",
      scope = "RISK_BASIS"
    )
    return(list(valid = FALSE, issues = issues))
  }

  # Identify available factor donors (inactive assets or explicitly designated donors)
  donor_factor_rows <- meta_dt[
    (Series_Type %in% c("ASSET_TR", "FACTOR", "FACTOR_DONOR") & Active == FALSE) |
    (isTRUE(Is_Donor) | Is_Donor == TRUE)
  ]
  donor_factor_ids <- setdiff(unique(donor_factor_rows$Code), asset_ids)
  donor_factor_ids <- intersect(donor_factor_ids, unique(u_dt$Code))

  # Extract preferred donors specified in metadata per asset
  preferred_donors_meta <- list()
  if ("Preferred_Donors" %in% names(meta_dt)) {
    for (aid in asset_ids) {
      pd_raw <- meta_dt[Code == aid, Preferred_Donors][1L]
      if (!is.na(pd_raw) && nzchar(trimws(pd_raw))) {
        split_donors <- trimws(unlist(strsplit(pd_raw, "[,;]+")))
        split_donors <- split_donors[nzchar(split_donors)]
        if (length(split_donors) > 0L) {
          preferred_donors_meta[[aid]] <- split_donors
        }
      }
    }
  }

  # Optional Excess Return transformation: subtract base-currency cash ONCE from u, never from z
  if (identical(return_mode, "EXCESS")) {
    if (is.null(cash_returns)) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "MISSING_CASH_FOR_EXCESS_MODE",
        severity = "ERROR",
        message = sprintf("Excess-Return-Modus gewählt, aber keine Cash-Renditen für %s übergeben.", base_ccy),
        scope = "RISK_BASIS"
      )
      return(list(valid = FALSE, issues = issues))
    }
    u_dt <- apply_excess_returns(u_dt, cash_returns, base_ccy = base_ccy)
  }

  u_univ <- u_dt[Code %in% asset_ids]
  univ_meta <- meta_dt[match(asset_ids, Code)]

  # Derive required foreign hedge currencies from universe & Hedge_Allowed:
  # Exclude domestic currency (base_ccy) and EMEQ (never hedgeable).
  eligible_hedge_rows <- univ_meta[
    isTRUE(Hedge_Allowed) | Hedge_Allowed == TRUE
  ][
    !is.na(FX_Exposure) & FX_Exposure != base_ccy & FX_Exposure != "EMEQ"
  ]
  required_hedge_ccys <- unique(eligible_hedge_rows$FX_Exposure)

  # Check which required hedge currencies actually have non-NA forward PnL observations
  avail_z_ccys <- if (nrow(z_dt) > 0L) {
    z_dt[!is.na(z_base), unique(currency)]
  } else {
    character(0)
  }

  supported_hedge_ccys <- avail_z_ccys
  available_hedge_ccys <- intersect(required_hedge_ccys, avail_z_ccys)
  missing_hedge_ccys <- setdiff(required_hedge_ccys, avail_z_ccys)

  if (length(missing_hedge_ccys) > 0L) {
    for (mccy in missing_hedge_ccys) {
      affected_assets <- univ_meta[FX_Exposure == mccy, Code]
      issues[[length(issues) + 1L]] <- make_issue(
        code = "MISSING_HEDGE_COMPONENT_IN_BASIS",
        severity = "WARNING",
        message = sprintf(
          "Hedgekomponente für %s (Basis %s) nicht verfügbar (betroffene Assets: %s). Ungesicherte Analyse aktiv; %s-Hedge wird bei Anforderung abgelehnt.",
          mccy, base_ccy, paste(affected_assets, collapse = ", "), mccy
        ),
        scope = "RISK_BASIS",
        details = list(currency = mccy, affected_assets = affected_assets)
      )
    }
  }

  hedge_ids <- if (length(available_hedge_ccys) > 0L) paste0("HEDGE_", available_hedge_ccys) else character(0)
  component_ids <- c(asset_ids, hedge_ids)

  # Build wide matrix X_B indexed by month_index
  grid <- unique(u_dt[, .(date, month_key, month_index)])
  data.table::setorder(grid, month_index)

  all_u_codes <- unique(c(asset_ids, donor_factor_ids))
  u_all <- u_dt[Code %in% all_u_codes]
  u_wide <- data.table::dcast(
    u_all[, .(month_index, Code, u_base)],
    month_index ~ Code,
    value.var = "u_base"
  )
  X_dt <- merge(grid, u_wide, by = "month_index", all.x = TRUE, sort = TRUE)

  if (length(available_hedge_ccys) > 0L) {
    z_sub <- z_dt[currency %in% available_hedge_ccys, .(month_index, component_id = paste0("HEDGE_", currency), z_base)]
    z_wide <- data.table::dcast(z_sub, month_index ~ component_id, value.var = "z_base")
    X_dt <- merge(X_dt, z_wide, by = "month_index", all.x = TRUE, sort = TRUE)
  }

  all_basis_ids <- unique(c(component_ids, donor_factor_ids))
  missing_cols <- setdiff(all_basis_ids, names(X_dt))
  for (mc in missing_cols) {
    data.table::set(X_dt, j = mc, value = NA_real_)
  }
  X_mat <- as.matrix(X_dt[, ..component_ids])
  rownames(X_mat) <- X_dt$month_key

  X_full_mat <- as.matrix(X_dt[, ..all_basis_ids])
  rownames(X_full_mat) <- X_dt$month_key

  # Component metadata and availability summary
  comp_avail_list <- lapply(component_ids, function(cid) {
    vec <- X_mat[, cid]
    valid_pos <- which(!is.na(vec))
    is_hedge <- cid %in% hedge_ids
    ccy <- if (is_hedge) sub("^HEDGE_", "", cid) else univ_meta[Code == cid, FX_Exposure][1L]
    data.table::data.table(
      component_id = cid,
      component_type = if (is_hedge) "FX_HEDGE" else "ASSET_TR",
      label = if (is_hedge) sprintf("FX Hedge %s -> %s (1M)", ccy, base_ccy) else univ_meta[Code == cid, Name][1L],
      currency = ccy,
      obs_count = length(valid_pos),
      start_month = if (length(valid_pos) > 0L) X_dt$month_key[min(valid_pos)] else NA_character_,
      end_month = if (length(valid_pos) > 0L) X_dt$month_key[max(valid_pos)] else NA_character_,
      available = length(valid_pos) > 0L
    )
  })
  comp_avail_dt <- data.table::rbindlist(comp_avail_list, fill = TRUE)

  # Also append missing hedge currencies as unavailable entries for transparent UI/validation
  if (length(missing_hedge_ccys) > 0L) {
    miss_dt <- data.table::data.table(
      component_id = paste0("HEDGE_", missing_hedge_ccys),
      component_type = "FX_HEDGE",
      label = sprintf("FX Hedge %s -> %s (1M fehlt)", missing_hedge_ccys, base_ccy),
      currency = missing_hedge_ccys,
      obs_count = 0L,
      start_month = NA_character_,
      end_month = NA_character_,
      available = FALSE
    )
    comp_avail_dt <- rbind(comp_avail_dt, miss_dt, fill = TRUE)
  }

  basis_hash <- compute_object_hash(list(
    base_ccy = base_ccy,
    return_mode = return_mode,
    component_ids = component_ids,
    donor_factor_ids = donor_factor_ids,
    preferred_donors_meta = preferred_donors_meta,
    X_mat = X_mat,
    X_full = X_full_mat
  ))

  fx_audit_summary <- if (nrow(z_dt) > 0L) {
    z_dt[!is.na(z_base), .(
      obs = .N,
      start_month = min(month_key),
      end_month = max(month_key),
      mean_carry_pa = mean(carry, na.rm = TRUE) * 12.0,
      mean_spot_offset_pa = mean(spot_offset, na.rm = TRUE) * 12.0,
      mean_z_net_pa = mean(z_base, na.rm = TRUE) * 12.0,
      vol_z_ann = stats::sd(z_base, na.rm = TRUE) * sqrt(12.0)
    ), by = .(base_currency, currency, component_id)]
  } else {
    data.table::data.table()
  }

  basis <- list(
    valid = TRUE,
    basis_id = substr(basis_hash, 1L, 12L),
    basis_hash = basis_hash,
    base_currency = base_ccy,
    return_mode = return_mode,
    asset_ids = asset_ids,
    hedge_ids = hedge_ids,
    component_ids = component_ids,
    donor_factor_ids = donor_factor_ids,
    preferred_donors_meta = preferred_donors_meta,
    required_hedge_currencies = required_hedge_ccys,
    available_hedge_currencies = available_hedge_ccys,
    supported_hedge_currencies = supported_hedge_ccys,
    missing_hedge_currencies = missing_hedge_ccys,
    dates = X_dt$date,
    month_keys = X_dt$month_key,
    month_index = X_dt$month_index,
    X_matrix = X_mat,
    X_full = X_full_mat,
    X_table = X_dt,
    component_availability = comp_avail_dt,
    fx_audit_summary = fx_audit_summary,
    latest_carry = if (!is.null(z$latest_carry)) z$latest_carry else list(),
    issues = issues
  )
  class(basis) <- c("maps_risk_basis", "list")
  basis
}

resolve_estimation_sample <- function(basis, settings = list()) {
  issues <- basis$issues
  X_mat <- basis$X_matrix
  X_full_basis <- basis$X_full %||% X_mat
  month_keys <- basis$month_keys
  dates <- basis$dates
  comp_ids <- basis$component_ids
  asset_ids <- basis$asset_ids

  window_mode <- toupper(settings$window_mode %||% "FULL")
  window_months <- settings$window_months
  missing_mode <- toupper(settings$missing_mode %||% "COMPLETE_CASE")
  start_month <- settings$start_month
  end_month <- settings$end_month

  # Determine unhedged complete-case coverage vs full (hedged + short-asset) complete-case coverage
  unhedged_complete_rows <- which(rowSums(is.na(X_mat[, asset_ids, drop = FALSE])) == 0L)
  full_complete_rows <- which(rowSums(is.na(X_mat[, comp_ids, drop = FALSE])) == 0L)

  # Also check core long-history assets (those starting by 1990-06)
  comp_starts <- apply(X_mat, 2L, function(col) {
    idx <- which(!is.na(col))
    if (length(idx) > 0L) min(idx) else NA_integer_
  })
  latest_start_idx <- max(comp_starts, na.rm = TRUE)
  earliest_return_idx <- min(comp_starts, na.rm = TRUE)
  short_history_components <- names(comp_starts)[comp_starts > earliest_return_idx]

  if (length(short_history_components) > 0L && identical(missing_mode, "COMPLETE_CASE") && identical(window_mode, "FULL")) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "COMMON_SAMPLE_TRUNCATED_BY_SHORT_SERIES",
      severity = "INFO",
      message = sprintf(
        "Vollständiger gemeinsamer Zeitraum beginnt %s (statt %s), bedingt durch kürzere Historien von: %s.",
        month_keys[latest_start_idx],
        month_keys[earliest_return_idx],
        paste(short_history_components, collapse = ", ")
      ),
      scope = "SAMPLE",
      details = list(
        common_start = month_keys[latest_start_idx],
        earliest_start = month_keys[earliest_return_idx],
        limiting_components = short_history_components
      )
    )
  }

  selected_rows <- if (identical(missing_mode, "FACTOR_RECONSTRUCTION")) {
    # Keep all rows where at least one component or donor factor is observed; factor model handles staggered starts
    which(rowSums(!is.na(X_full_basis)) > 0L)
  } else {
    full_complete_rows
  }

  # Apply custom date range filter if requested
  if (!is.null(start_month) && nzchar(start_month)) {
    selected_rows <- selected_rows[month_keys[selected_rows] >= start_month]
  }
  if (!is.null(end_month) && nzchar(end_month)) {
    selected_rows <- selected_rows[month_keys[selected_rows] <= end_month]
  }

  # Apply window_mode (ROLLING last M months, or CRISIS top M high-stress months)
  if (identical(window_mode, "ROLLING") && !is.null(window_months) && is.numeric(window_months) && window_months > 0) {
    w_len <- as.integer(window_months)
    if (length(selected_rows) > w_len) {
      selected_rows <- utils::tail(selected_rows, w_len)
    }
  } else if (identical(window_mode, "CRISIS")) {
    w_len <- as.integer(window_months %||% 60L)
    if (length(selected_rows) > w_len) {
      sub_X <- X_mat[selected_rows, asset_ids, drop = FALSE]
      # Equal-weighted cross-sectional squared return / dispersion stress score
      stress_score <- rowMeans(sub_X^2, na.rm = TRUE)
      top_idx <- order(stress_score, decreasing = TRUE)[seq_len(min(w_len, length(selected_rows)))]
      selected_rows <- sort(selected_rows[top_idx])
    }
  }

  if (length(selected_rows) < 12L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "INSUFFICIENT_SAMPLE_OBSERVATIONS",
      severity = "ERROR",
      message = sprintf("Zu wenige gemeinsame Beobachtungen im Schätzfenster (%d Monate; mindestens 12 erforderlich).", length(selected_rows)),
      scope = "SAMPLE"
    )
    return(list(valid = FALSE, issues = issues))
  }

  X_sample <- X_mat[selected_rows, comp_ids, drop = FALSE]
  sample_dates <- dates[selected_rows]
  sample_months <- month_keys[selected_rows]

  list(
    valid = TRUE,
    basis_id = basis$basis_id,
    base_currency = basis$base_currency,
    return_mode = basis$return_mode,
    window_mode = window_mode,
    missing_mode = missing_mode,
    requested_window_months = window_months,
    n_obs = nrow(X_sample),
    start_month = sample_months[1L],
    end_month = sample_months[length(sample_months)],
    start_date = sample_dates[1L],
    end_date = sample_dates[length(sample_dates)],
    unhedged_common_start = if (length(unhedged_complete_rows) > 0L) month_keys[min(unhedged_complete_rows)] else NA_character_,
    hedged_common_start = if (length(full_complete_rows) > 0L) month_keys[min(full_complete_rows)] else NA_character_,
    short_history_components = short_history_components,
    asset_ids = basis$asset_ids,
    hedge_ids = basis$hedge_ids,
    component_ids = comp_ids,
    required_hedge_currencies = basis$required_hedge_currencies,
    available_hedge_currencies = basis$available_hedge_currencies,
    supported_hedge_currencies = basis$supported_hedge_currencies %||% basis$available_hedge_currencies,
    missing_hedge_currencies = basis$missing_hedge_currencies,
    component_availability = basis$component_availability,
    X_sample = X_sample,
    X_full = X_full_basis[selected_rows, , drop = FALSE],
    donor_factor_ids = basis$donor_factor_ids %||% character(0),
    preferred_donors_meta = basis$preferred_donors_meta %||% list(),
    latest_carry = basis$latest_carry,
    month_keys = sample_months,
    all_month_keys = month_keys,
    issues = issues
  )
}
