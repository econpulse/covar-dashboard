# ==============================================================================
# R/fx_quotes.R
# FX spot/forward quote normalization to USD numeraire (V_C, G_C) and
# level triangulation into base currency B (S_t^(B/C), F_t^(B/C))
# Pure R module - no Shiny dependencies
# ==============================================================================

normalize_spot_quotes <- function(registry, policy = NULL) {
  issues <- list()
  meta <- registry$meta
  data <- registry$data

  # Unique calendar grid from dataset
  grid <- unique(data[, .(date, month_key, month_index)])
  data.table::setorder(grid, month_index)

  spot_meta <- meta[Series_Type == "FX_SPOT" & Active == TRUE]
  out_list <- list()

  # USD numeraire identity: V_USD,t = 1
  out_list[["USD"]] <- data.table::data.table(
    date = grid$date,
    month_key = grid$month_key,
    month_index = grid$month_index,
    currency = "USD",
    source_code = "USD_IDENTITY",
    raw_spot = 1.0,
    V_usd_per_ccy = 1.0
  )

  currencies <- unique(spot_meta$Foreign_Currency)
  for (ccy in currencies) {
    rows <- spot_meta[Foreign_Currency == ccy]
    if (nrow(rows) > 1L) {
      sel_code <- policy$fx_selection$FX_SPOT[[ccy]]
      if (!is.null(sel_code) && sel_code %in% rows$Code) {
        rows <- rows[Code == sel_code]
      } else {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "AMBIGUOUS_SPOT_SERIES",
          severity = "ERROR",
          message = sprintf("Mehrere aktive FX_SPOT-Reihen für %s ohne explizite Auswahl: %s", ccy, paste(rows$Code, collapse = ", ")),
          scope = "FX"
        )
        next
      }
    }

    row <- rows[1L]
    sc <- row$Code
    q_ccy <- row$Quote_Currency
    q_dir <- row$Quote_Direction
    q_scale <- if (is.na(row$Quote_Scale) || row$Quote_Scale <= 0) 1.0 else as.numeric(row$Quote_Scale)

    if (!identical(q_ccy, "USD")) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "UNSUPPORTED_SPOT_QUOTE_CURRENCY",
        severity = "ERROR",
        message = sprintf("FX_SPOT '%s' hat Quote_Currency='%s' (erwartet: 'USD').", sc, q_ccy),
        scope = "FX",
        series_code = sc
      )
      next
    }

    sub <- data[Code == sc, .(month_index, raw_spot = value)]
    merged <- merge(grid, sub, by = "month_index", all.x = TRUE, sort = TRUE)

    # Step 1: Scale in raw quote direction (price per 1 unit of foreign/quote currency)
    scaled_spot <- merged$raw_spot / q_scale

    # Step 2: Convert to V_C = USD per 1 unit of C
    V_val <- if (identical(q_dir, "QUOTE_PER_FOREIGN")) {
      scaled_spot
    } else if (identical(q_dir, "FOREIGN_PER_QUOTE")) {
      ifelse(!is.na(scaled_spot) & scaled_spot > 0, 1.0 / scaled_spot, NA_real_)
    } else {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "INVALID_SPOT_QUOTE_DIRECTION",
        severity = "ERROR",
        message = sprintf("Ungültige Quote_Direction '%s' für FX_SPOT '%s'.", q_dir, sc),
        scope = "FX",
        series_code = sc
      )
      rep(NA_real_, nrow(merged))
    }

    out_list[[ccy]] <- data.table::data.table(
      date = merged$date,
      month_key = merged$month_key,
      month_index = merged$month_index,
      currency = ccy,
      source_code = sc,
      raw_spot = merged$raw_spot,
      V_usd_per_ccy = V_val
    )
  }

  V_dt <- data.table::rbindlist(out_list, fill = TRUE)
  data.table::setorder(V_dt, currency, month_index)
  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))

  list(
    valid = !has_errors,
    V = V_dt,
    currencies = unique(V_dt$currency),
    issues = issues
  )
}

normalize_forward_quotes <- function(registry, spots, policy = NULL) {
  issues <- list()
  meta <- registry$meta
  data <- registry$data

  grid <- unique(data[, .(date, month_key, month_index)])
  data.table::setorder(grid, month_index)

  fwd_meta <- meta[Series_Type == "FX_FORWARD" & Active == TRUE]
  out_list <- list()

  # USD numeraire identity: G_USD,t = 1
  out_list[["USD"]] <- data.table::data.table(
    date = grid$date,
    month_key = grid$month_key,
    month_index = grid$month_index,
    currency = "USD",
    source_code = "USD_IDENTITY",
    tenor = "1M",
    raw_forward = 1.0,
    G_usd_per_ccy = 1.0
  )

  max_div <- policy$fx_policy$max_spot_forward_divergence %||% 0.15
  currencies <- unique(fwd_meta$Foreign_Currency)

  for (ccy in currencies) {
    rows <- fwd_meta[Foreign_Currency == ccy]
    if (nrow(rows) > 1L) {
      sel_code <- policy$fx_selection$FX_FORWARD[[ccy]]
      if (!is.null(sel_code) && sel_code %in% rows$Code) {
        rows <- rows[Code == sel_code]
      } else {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "AMBIGUOUS_FORWARD_SERIES",
          severity = "ERROR",
          message = sprintf("Mehrere aktive FX_FORWARD-Reihen für %s ohne explizite Auswahl: %s", ccy, paste(rows$Code, collapse = ", ")),
          scope = "FX"
        )
        next
      }
    }

    row <- rows[1L]
    sc <- row$Code
    q_ccy <- row$Quote_Currency
    q_dir <- row$Quote_Direction
    q_scale <- if (is.na(row$Quote_Scale) || row$Quote_Scale <= 0) 1.0 else as.numeric(row$Quote_Scale)
    f_type <- row$Forward_Type %||% "OUTRIGHT"
    p_mult <- if (is.na(row$Points_Multiplier)) 1.0 else as.numeric(row$Points_Multiplier)
    tenor <- row$Tenor %||% "1M"
    spot_ref <- row$Spot_Reference_Code

    if (!identical(q_ccy, "USD")) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "UNSUPPORTED_FORWARD_QUOTE_CURRENCY",
        severity = "ERROR",
        message = sprintf("FX_FORWARD '%s' hat Quote_Currency='%s' (erwartet: 'USD').", sc, q_ccy),
        scope = "FX",
        series_code = sc
      )
      next
    }

    if (!identical(tenor, "1M")) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "NON_1M_FORWARD_TENOR",
        severity = "ERROR",
        message = sprintf("FX_FORWARD '%s' hat Tenor='%s' (erwartet: '1M').", sc, tenor),
        scope = "FX",
        series_code = sc
      )
      next
    }

    sub_fwd <- data[Code == sc, .(month_index, raw_forward = value)]
    merged <- merge(grid, sub_fwd, by = "month_index", all.x = TRUE, sort = TRUE)

    # Step 1: Convert points to outright in raw direction BEFORE any inversion
    outright_raw_scaled <- rep(NA_real_, nrow(merged))
    if (identical(f_type, "OUTRIGHT")) {
      outright_raw_scaled <- merged$raw_forward / q_scale
    } else if (identical(f_type, "POINTS")) {
      if (is.na(spot_ref) || !(spot_ref %in% meta$Code)) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "MISSING_SPOT_REFERENCE_FOR_POINTS",
          severity = "ERROR",
          message = sprintf("FX_FORWARD '%s' (POINTS) verweist auf unbekannten Spot_Reference_Code '%s'.", sc, spot_ref),
          scope = "FX",
          series_code = sc
        )
        next
      }
      sref_meta <- meta[Code == spot_ref][1L]
      if (!identical(sref_meta$Quote_Direction, q_dir) || !identical(sref_meta$Foreign_Currency, ccy)) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "MISMATCHED_SPOT_REFERENCE_DIRECTION",
          severity = "ERROR",
          message = sprintf(
            "FX_FORWARD '%s' (POINTS, %s) und Spot_Reference_Code '%s' (%s) haben unterschiedliche Rohkursrichtungen.",
            sc, q_dir, spot_ref, sref_meta$Quote_Direction
          ),
          scope = "FX",
          series_code = sc
        )
        next
      }
      sref_scale <- if (is.na(sref_meta$Quote_Scale) || sref_meta$Quote_Scale <= 0) 1.0 else as.numeric(sref_meta$Quote_Scale)
      sub_spot_raw <- data[Code == spot_ref, .(month_index, ref_spot = value)]
      merged <- merge(merged, sub_spot_raw, by = "month_index", all.x = TRUE, sort = TRUE)
      outright_raw_scaled <- (merged$ref_spot / sref_scale) + (merged$raw_forward * p_mult) / q_scale
    } else {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "UNKNOWN_FORWARD_TYPE",
        severity = "ERROR",
        message = sprintf("Unbekannter Forward_Type '%s' für '%s'.", f_type, sc),
        scope = "FX",
        series_code = sc
      )
      next
    }

    if (any(!is.na(outright_raw_scaled) & outright_raw_scaled <= 0)) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "NON_POSITIVE_FORWARD_OUTRIGHT",
        severity = "ERROR",
        message = sprintf("FX_FORWARD '%s' erzeugt nicht-positive Outright-Werte.", sc),
        scope = "FX",
        series_code = sc
      )
      next
    }

    # Step 2: Convert to G_C = 1M Forward USD per 1 unit of C
    G_val <- if (identical(q_dir, "QUOTE_PER_FOREIGN")) {
      outright_raw_scaled
    } else if (identical(q_dir, "FOREIGN_PER_QUOTE")) {
      ifelse(!is.na(outright_raw_scaled) & outright_raw_scaled > 0, 1.0 / outright_raw_scaled, NA_real_)
    } else {
      rep(NA_real_, nrow(merged))
    }

    # Step 3: Plausibility check against normalized spot V_C
    spot_ccy_dt <- spots$V[currency == ccy, .(month_index, V_usd_per_ccy)]
    chk <- merge(
      data.table::data.table(month_index = merged$month_index, G_val = G_val),
      spot_ccy_dt,
      by = "month_index"
    )
    chk_valid <- chk[!is.na(G_val) & !is.na(V_usd_per_ccy) & G_val > 0 & V_usd_per_ccy > 0]
    if (nrow(chk_valid) > 0L) {
      log_diff <- abs(log(chk_valid$G_val / chk_valid$V_usd_per_ccy))
      max_ld <- max(log_diff)
      if (max_ld > max_div) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "FX_SPOT_FORWARD_DIVERGENCE",
          severity = "ERROR",
          message = sprintf(
            "Unplausible Abweichung zwischen normalisiertem Spot V_%s und 1M-Forward G_%s (max |log(G/V)| = %.4f > %.2f). Bitte Quote_Direction/Quote_Scale prüfen.",
            ccy, ccy, max_ld, max_div
          ),
          scope = "FX",
          series_code = sc,
          details = list(currency = ccy, max_abs_log_ratio = max_ld)
        )
      }
    }

    out_list[[ccy]] <- data.table::data.table(
      date = merged$date,
      month_key = merged$month_key,
      month_index = merged$month_index,
      currency = ccy,
      source_code = sc,
      tenor = tenor,
      raw_forward = merged$raw_forward,
      G_usd_per_ccy = G_val
    )
  }

  G_dt <- data.table::rbindlist(out_list, fill = TRUE)
  data.table::setorder(G_dt, currency, month_index)
  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))

  list(
    valid = !has_errors,
    G = G_dt,
    currencies = unique(G_dt$currency),
    issues = issues
  )
}

cross_fx_levels <- function(V, G, base_ccy = "CHF") {
  issues <- list()
  V_dt <- if (is.data.frame(V)) data.table::as.data.table(V) else V$V
  G_dt <- if (is.data.frame(G)) data.table::as.data.table(G) else G$G

  if (!(base_ccy %in% unique(V_dt$currency))) {
    issues[[1L]] <- make_issue(
      code = "MISSING_BASE_CURRENCY_SPOT",
      severity = "ERROR",
      message = sprintf("Basiswährung '%s' ist in den normalisierten Spot-Kursen nicht vorhanden.", base_ccy),
      scope = "FX"
    )
    return(list(valid = FALSE, crosses = data.table::data.table(), issues = issues))
  }

  base_spot <- V_dt[currency == base_ccy, .(month_index, V_base = V_usd_per_ccy)]
  base_fwd <- G_dt[currency == base_ccy, .(month_index, G_base = G_usd_per_ccy)]

  all_ccys <- unique(V_dt$currency)
  cross_list <- vector("list", length(all_ccys))

  for (i in seq_along(all_ccys)) {
    ccy <- all_ccys[i]
    sub_v <- V_dt[currency == ccy, .(date, month_key, month_index, currency, spot_code = source_code, V_c = V_usd_per_ccy)]
    sub_g <- G_dt[currency == ccy, .(month_index, fwd_code = source_code, G_c = G_usd_per_ccy)]

    m <- merge(sub_v, base_spot, by = "month_index", all.x = TRUE, sort = TRUE)
    m <- merge(m, sub_g, by = "month_index", all.x = TRUE, sort = TRUE)
    m <- merge(m, base_fwd, by = "month_index", all.x = TRUE, sort = TRUE)

    if (identical(ccy, base_ccy)) {
      # Domestic identity: B = C -> Spot = 1, Forward = 1
      m[, `:=`(
        base_currency = base_ccy,
        S_b_per_c = ifelse(!is.na(V_c) & !is.na(V_base), 1.0, NA_real_),
        F_b_per_c = ifelse(!is.na(V_c) & !is.na(V_base), 1.0, NA_real_),
        spot_available = !is.na(V_c) & !is.na(V_base),
        forward_available = TRUE,
        is_domestic = TRUE
      )]
    } else {
      m[, `:=`(
        base_currency = base_ccy,
        S_b_per_c = ifelse(!is.na(V_c) & !is.na(V_base) & V_base > 0, V_c / V_base, NA_real_),
        F_b_per_c = ifelse(!is.na(G_c) & !is.na(G_base) & G_base > 0, G_c / G_base, NA_real_),
        spot_available = !is.na(V_c) & !is.na(V_base) & V_base > 0,
        forward_available = !is.na(G_c) & !is.na(G_base) & G_base > 0,
        is_domestic = FALSE
      )]
    }
    cross_list[[i]] <- m
  }

  crosses_dt <- data.table::rbindlist(cross_list, fill = TRUE)
  data.table::setorder(crosses_dt, currency, month_index)

  # Availability summary per currency
  avail_dt <- crosses_dt[, .(
    spot_obs = sum(spot_available, na.rm = TRUE),
    spot_start = if (any(spot_available)) min(month_key[spot_available]) else NA_character_,
    spot_end = if (any(spot_available)) max(month_key[spot_available]) else NA_character_,
    forward_obs = sum(forward_available & !is_domestic, na.rm = TRUE),
    forward_start = if (any(forward_available & !is_domestic)) min(month_key[forward_available & !is_domestic]) else if (any(is_domestic)) min(month_key[spot_available]) else NA_character_,
    forward_end = if (any(forward_available & !is_domestic)) max(month_key[forward_available & !is_domestic]) else if (any(is_domestic)) max(month_key[spot_available]) else NA_character_,
    has_forward = any(forward_available)
  ), by = .(base_currency, currency, is_domestic)]

  list(
    valid = length(issues) == 0L,
    base_currency = base_ccy,
    crosses = crosses_dt,
    availability = avail_dt,
    issues = issues
  )
}
