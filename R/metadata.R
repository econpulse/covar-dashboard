# ==============================================================================
# R/metadata.R
# Consolidated metadata validation, non-destructive migration, observation
# checks (coverage, interior gaps, value domains), and series registry builder
# Pure R module - no Shiny dependencies
# ==============================================================================

validate_metadata <- function(meta,
                              policy = NULL,
                              raw_codes = NULL,
                              long_data = NULL) {
  issues <- list()
  if (!is.data.frame(meta)) {
    issues[[1L]] <- make_issue(
      code = "INVALID_METADATA_TABLE",
      severity = "ERROR",
      message = "Metadatenblatt ist keine gültige Tabelle.",
      scope = "METADATA"
    )
    return(list(valid = FALSE, meta = data.table::data.table(), issues = issues))
  }

  req_cols <- c(
    "Code", "Name", "Series_Type", "Value_Unit", "Active", "Review_Status",
    "Asset_Class", "Sub_Asset_Class", "Region", "Return_Currency", "FX_Exposure",
    "Hedge_Allowed", "Default_Hedge_Ratio", "TR_Convention", "Already_Hedged",
    "Existing_Hedge_Currency", "Return_Structure", "Foreign_Currency",
    "Quote_Currency", "Quote_Direction", "Quote_Scale", "Forward_Type",
    "Points_Multiplier", "Tenor", "Cash_Currency", "Rate_Convention",
    "Day_Count", "Spot_Reference_Code", "Source", "Notes"
  )

  missing_cols <- setdiff(req_cols, names(meta))
  if (length(missing_cols) > 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "MISSING_METADATA_COLUMNS",
      severity = "ERROR",
      message = sprintf("Fehlende Pflichtspalten in Metadaten: %s", paste(missing_cols, collapse = ", ")),
      scope = "METADATA",
      details = list(missing_columns = missing_cols)
    )
    return(list(valid = FALSE, meta = data.table::as.data.table(meta), issues = issues))
  }

  dt <- data.table::as.data.table(meta)

  # Normalize column types cleanly without altering exact Code strings
  char_cols <- c(
    "Code", "Name", "Series_Type", "Value_Unit", "Review_Status",
    "Asset_Class", "Sub_Asset_Class", "Region", "Return_Currency", "FX_Exposure",
    "TR_Convention", "Existing_Hedge_Currency", "Return_Structure",
    "Foreign_Currency", "Quote_Currency", "Quote_Direction", "Forward_Type",
    "Tenor", "Cash_Currency", "Rate_Convention", "Day_Count",
    "Spot_Reference_Code", "Source", "Notes"
  )
  for (col in char_cols) {
    val <- as.character(dt[[col]])
    val <- trimws(val)
    val[!nzchar(val) | val == "NA"] <- NA_character_
    data.table::set(dt, j = col, value = val)
  }

  to_logical <- function(x) {
    if (is.logical(x)) return(x)
    xc <- toupper(trimws(as.character(x)))
    out <- rep(NA, length(xc))
    out[xc %in% c("TRUE", "T", "1", "JA", "YES", "WAHR")] <- TRUE
    out[xc %in% c("FALSE", "F", "0", "NEIN", "NO", "FALSCH")] <- FALSE
    out
  }
  dt[, Active := to_logical(Active)]
  dt[, Hedge_Allowed := to_logical(Hedge_Allowed)]
  dt[, Already_Hedged := to_logical(Already_Hedged)]
  dt[, Default_Hedge_Ratio := suppressWarnings(as.numeric(Default_Hedge_Ratio))]
  dt[, Quote_Scale := suppressWarnings(as.numeric(Quote_Scale))]
  dt[, Points_Multiplier := suppressWarnings(as.numeric(Points_Multiplier))]

  # Ensure versioned proxy columns exist without overwriting user-supplied values
  opt_proxy_cols <- c("Proxy_Type", "Proxy_Until", "Proxy_Description")
  for (pcol in opt_proxy_cols) {
    if (!(pcol %in% names(dt))) {
      dt[, (pcol) := NA_character_]
    } else {
      dt[, (pcol) := as.character(get(pcol))]
    }
  }

  # Ensure optional donor factor columns exist
  if (!("Preferred_Donors" %in% names(dt))) {
    dt[, Preferred_Donors := NA_character_]
  } else {
    dt[, Preferred_Donors := as.character(Preferred_Donors)]
    dt[trimws(Preferred_Donors) == "" | Preferred_Donors == "NA", Preferred_Donors := NA_character_]
  }
  if (!("Is_Donor" %in% names(dt))) {
    dt[, Is_Donor := FALSE]
  } else {
    dt[, Is_Donor := to_logical(Is_Donor)]
    dt[is.na(Is_Donor), Is_Donor := FALSE]
  }

  # Validate Preferred_Donors references
  pref_rows <- which(!is.na(dt$Preferred_Donors))
  for (pr in pref_rows) {
    p_cd <- dt$Code[pr]
    p_str <- dt$Preferred_Donors[pr]
    tokens <- trimws(unlist(strsplit(p_str, "[,;]+")))
    tokens <- tokens[nzchar(tokens)]
    unknown <- setdiff(tokens, dt$Code)
    if (length(unknown) > 0L) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "UNKNOWN_PREFERRED_DONOR_CODE",
        severity = "WARNING",
        message = sprintf(
          "Reihe '%s' verweist in Preferred_Donors auf nicht im Metadatenblatt definierte(n) Code(s): %s",
          p_cd, paste(unknown, collapse = ", ")
        ),
        scope = "METADATA",
        series_code = p_cd,
        details = list(unknown_codes = unknown)
      )
    }
  }

  # Check duplicate Codes in Metadata
  dup_codes <- unique(dt$Code[!is.na(dt$Code) & duplicated(dt$Code)])
  if (length(dup_codes) > 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "DUPLICATE_METADATA_CODE",
      severity = "ERROR",
      message = sprintf("Doppelte Codes im Metadatenblatt: %s", paste(dup_codes, collapse = ", ")),
      scope = "METADATA",
      details = list(duplicate_codes = dup_codes)
    )
  }

  # Cross-check against raw_codes if provided:
  # Support non-destructive GBP spot code replacement if raw sheet has a new GBP spot code
  if (!is.null(raw_codes)) {
    raw_codes <- unique(trimws(as.character(raw_codes)))
    unmatched_raw <- setdiff(raw_codes, dt$Code)
    unmatched_meta <- setdiff(dt$Code, raw_codes)

    # Check if user replaced legacy GBP spot code (e.g. USSTBOE) in raw data sheet with a new GBP spot code
    if (length(unmatched_raw) == 1L && ("USSTBOE" %in% unmatched_meta)) {
      new_gbp_code <- unmatched_raw[1L]
      dt[Code == "USSTBOE", `:=`(
        Code = new_gbp_code,
        Quote_Direction = "QUOTE_PER_FOREIGN",
        Quote_Scale = 1,
        Notes = paste0(Notes %||% "", " [Auto-Migration: GBP-Spot auf ", new_gbp_code, " (USD je GBP) aktualisiert]")
      )]
      dt[Spot_Reference_Code == "USSTBOE", Spot_Reference_Code := new_gbp_code]
      issues[[length(issues) + 1L]] <- make_issue(
        code = "GBP_SPOT_CODE_MIGRATED",
        severity = "INFO",
        message = sprintf("GBP-Spot-Code wurde auf '%s' (USD je GBP) aktualisiert.", new_gbp_code),
        scope = "METADATA",
        series_code = new_gbp_code
      )
      unmatched_raw <- setdiff(raw_codes, dt$Code)
      unmatched_meta <- setdiff(dt$Code, raw_codes)
    }

    if (length(unmatched_raw) > 0L) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "RAW_CODE_MISSING_IN_METADATA",
        severity = "ERROR",
        message = sprintf("Zeitreihen im Datenblatt ohne Metadateneintrag: %s", paste(unmatched_raw, collapse = ", ")),
        scope = "METADATA",
        details = list(unmatched_raw = unmatched_raw)
      )
    }
    if (length(unmatched_meta) > 0L) {
      active_missing <- dt[Code %in% unmatched_meta & isTRUE(Active), Code]
      sev <- if (length(active_missing) > 0L) "ERROR" else "WARNING"
      issues[[length(issues) + 1L]] <- make_issue(
        code = "METADATA_CODE_MISSING_IN_RAW",
        severity = sev,
        message = sprintf("Metadateneinträge ohne Zeitreihe im Datenblatt: %s", paste(unmatched_meta, collapse = ", ")),
        scope = "METADATA",
        details = list(unmatched_meta = unmatched_meta)
      )
    }
  }

  # Inspect GBP Spot series: do NOT automatically trust legacy USSTBOE direction without checking actual values/metadata
  gbp_spot_idx <- which(dt$Series_Type == "FX_SPOT" & dt$Foreign_Currency == "GBP" & dt$Active == TRUE)
  if (length(gbp_spot_idx) == 1L) {
    gbp_code <- dt$Code[gbp_spot_idx]
    gbp_dir <- dt$Quote_Direction[gbp_spot_idx]
    if (!is.null(long_data) && nrow(long_data) > 0L) {
      gbp_vals <- long_data[Code == gbp_code & !is.na(value), value]
      if (length(gbp_vals) > 0L) {
        med_val <- stats::median(gbp_vals)
        # Historical GBP/USD in USD per GBP is > 1.05 (median ~ 1.50); in GBP per USD is < 0.95 (median ~ 0.65)
        if (med_val > 1.0 && identical(gbp_dir, "FOREIGN_PER_QUOTE")) {
          dt[gbp_spot_idx, Quote_Direction := "QUOTE_PER_FOREIGN"]
          issues[[length(issues) + 1L]] <- make_issue(
            code = "GBP_SPOT_DIRECTION_UPDATED",
            severity = "INFO",
            message = sprintf(
              "GBP-Spot '%s' liegt in den Daten als USD je GBP vor (Median %.4f); Quote_Direction auf QUOTE_PER_FOREIGN aktualisiert.",
              gbp_code, med_val
            ),
            scope = "METADATA",
            series_code = gbp_code
          )
        } else if (identical(gbp_code, "USSTBOE") && med_val < 1.0 && identical(gbp_dir, "FOREIGN_PER_QUOTE")) {
          issues[[length(issues) + 1L]] <- make_issue(
            code = "GBP_LEGACY_SNAPSHOT_ACTIVE",
            severity = "WARNING",
            message = paste0(
              "GBP-Spot verwendet in dieser Datei noch den Snapshot-Code 'USSTBOE' (GBP je USD, Median ",
              sprintf("%.4f", med_val),
              "). Wird über explizite Metadaten-Konvention in USD je GBP invertiert; bei Umstellung der Rohreihe auf USD je GBP wird der neue Code/Richtung übernommen."
            ),
            scope = "METADATA",
            series_code = gbp_code
          )
        }
      }
    }
  }

  # Structured EUR-Cash-Proxy configuration (never parse free-text Notes!)
  allow_eur_proxy <- isTRUE(policy$cash_proxy_config$EUR$allow_pre_1999_proxy %||% TRUE)
  eur_proxy_code <- policy$cash_proxy_config$EUR$proxy_code %||% "ECWGM1M"
  if (eur_proxy_code %in% dt$Code) {
    idx_eur <- which(dt$Code == eur_proxy_code)
    if (allow_eur_proxy) {
      if (is.na(dt$Cash_Currency[idx_eur])) {
        dt[idx_eur, Cash_Currency := "EUR"]
      }
      if (is.na(dt$Proxy_Type[idx_eur])) {
        dt[idx_eur, Proxy_Type := policy$cash_proxy_config$EUR$proxy_type %||% "DEM_DEPOSIT_PROXY"]
      }
      if (is.na(dt$Proxy_Until[idx_eur])) {
        dt[idx_eur, Proxy_Until := policy$cash_proxy_config$EUR$proxy_until %||% "1998-12-31"]
      }
      if (is.na(dt$Proxy_Description[idx_eur])) {
        dt[idx_eur, Proxy_Description := policy$cash_proxy_config$EUR$proxy_description %||%
             "BD EU-MARK 1M Einlage vor 1999 als bewusst gewählter EUR-Cash-Proxy (vermutete DEM-Herkunft als Annahme dokumentiert; ab 1999 EUR-Reihe)"]
      }
      issues[[length(issues) + 1L]] <- make_issue(
        code = "EUR_CASH_PROXY_ACTIVE",
        severity = "WARNING",
        message = sprintf(
          "%s wird bis %s als bewusst konfigurierter EUR-Cash-Proxy (vermutete DEM-Herkunft) verwendet; ab 1999 entspricht die Reihe EUR.",
          eur_proxy_code, dt$Proxy_Until[idx_eur]
        ),
        scope = "METADATA",
        series_code = eur_proxy_code
      )
    } else {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "EUR_CASH_PROXY_DISABLED",
        severity = "WARNING",
        message = sprintf("EUR-Cash-Proxy (%s) vor 1999 ist per Policy deaktiviert.", eur_proxy_code),
        scope = "METADATA",
        series_code = eur_proxy_code
      )
    }
  }

  # Validate ASSET_TR rows
  asset_rows <- which(dt$Series_Type == "ASSET_TR")
  for (r in asset_rows) {
    sc <- dt$Code[r]
    if (is.na(dt$Return_Currency[r]) || is.na(dt$FX_Exposure[r])) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "ASSET_MISSING_CURRENCY",
        severity = "ERROR",
        message = sprintf("Asset '%s' benötigt Return_Currency und FX_Exposure.", sc),
        scope = "METADATA",
        series_code = sc
      )
    }
    # EM equity rule: EMEQ is never hedgeable and must not enter USD hedge denominator
    if (identical(dt$FX_Exposure[r], "EMEQ")) {
      if (isTRUE(dt$Hedge_Allowed[r])) {
        dt[r, Hedge_Allowed := FALSE]
        dt[r, Default_Hedge_Ratio := 0]
        issues[[length(issues) + 1L]] <- make_issue(
          code = "EMEQ_HEDGE_FORCED_FALSE",
          severity = "WARNING",
          message = sprintf("Asset '%s' hat FX_Exposure=EMEQ; Hedge_Allowed wurde verbindlich auf FALSE gesetzt.", sc),
          scope = "METADATA",
          series_code = sc
        )
      }
    }
    # Gold / Commodity Futures TR rule
    if (identical(dt$Return_Structure[r], "FUTURES_TR") || sc %in% c("GSGCTOT", "DJUBSTR")) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "FUTURES_TR_CONVENTION_NOTE",
        severity = "INFO",
        message = sprintf(
          "Asset '%s' (%s) ist ein Futures-Total-Return-Index inkl. Collateral/Roll; ein USD-Hedge steuert nur das kontraktuelle Nominal.",
          sc, dt$Name[r]
        ),
        scope = "METADATA",
        series_code = sc
      )
    }
    # Already_Hedged check: block in MVP unless explicitly supported
    if (isTRUE(dt$Already_Hedged[r]) && isTRUE(dt$Active[r])) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "UNSUPPORTED_PREHEDGED_INDEX",
        severity = "ERROR",
        message = sprintf(
          "Asset '%s' ist als Already_Hedged=TRUE markiert. Vorab abgesicherte Indizes sind ohne explizite Entflechtungsregel gesperrt.",
          sc
        ),
        scope = "METADATA",
        series_code = sc
      )
    }
  }

  # Validate FX_SPOT and FX_FORWARD rows + check multiple matching series & missing forwards
  fx_rows <- which(dt$Series_Type %in% c("FX_SPOT", "FX_FORWARD") & dt$Active == TRUE)
  for (r in fx_rows) {
    sc <- dt$Code[r]
    if (is.na(dt$Foreign_Currency[r]) || is.na(dt$Quote_Currency[r]) || is.na(dt$Quote_Direction[r])) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "FX_MISSING_QUOTE_CONVENTION",
        severity = "ERROR",
        message = sprintf("FX-Reihe '%s' benötigt Foreign_Currency, Quote_Currency und Quote_Direction.", sc),
        scope = "METADATA",
        series_code = sc
      )
    }
    if (is.na(dt$Quote_Scale[r]) || dt$Quote_Scale[r] <= 0) {
      dt[r, Quote_Scale := 1]
    }
    if (identical(dt$Series_Type[r], "FX_FORWARD")) {
      if (is.na(dt$Forward_Type[r]) || !(dt$Forward_Type[r] %in% c("OUTRIGHT", "POINTS"))) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "FX_FORWARD_MISSING_TYPE",
          severity = "ERROR",
          message = sprintf("FX-Forward '%s' benötigt Forward_Type (OUTRIGHT oder POINTS).", sc),
          scope = "METADATA",
          series_code = sc
        )
      }
      if (identical(dt$Forward_Type[r], "POINTS")) {
        if (is.na(dt$Spot_Reference_Code[r]) || is.na(dt$Points_Multiplier[r])) {
          issues[[length(issues) + 1L]] <- make_issue(
            code = "FX_POINTS_MISSING_REFERENCE",
            severity = "ERROR",
            message = sprintf("FX-Forward '%s' mit Forward_Type=POINTS benötigt Spot_Reference_Code und Points_Multiplier.", sc),
            scope = "METADATA",
            series_code = sc
          )
        }
      }
    }
  }

  # Check for duplicate active FX series per (Series_Type, Foreign_Currency)
  fx_dups <- dt[Series_Type %in% c("FX_SPOT", "FX_FORWARD") & Active == TRUE,
                .(count = .N, codes = paste(Code, collapse = ", ")),
                by = .(Series_Type, Foreign_Currency)]
  fx_dups <- fx_dups[count > 1L]
  if (nrow(fx_dups) > 0L) {
    for (i in seq_len(nrow(fx_dups))) {
      sel_map <- policy$fx_selection[[fx_dups$Series_Type[i]]][[fx_dups$Foreign_Currency[i]]]
      if (is.null(sel_map)) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "AMBIGUOUS_FX_SERIES",
          severity = "ERROR",
          message = sprintf(
            "Mehrere aktive %s-Reihen für Währung %s gefunden (%s). Explizite Auswahl in der Konfiguration erforderlich.",
            fx_dups$Series_Type[i], fx_dups$Foreign_Currency[i], fx_dups$codes[i]
          ),
          scope = "METADATA"
        )
      }
    }
  }

  # Check availability of Spot and 1M Forward for all active asset currencies
  active_assets <- dt[Series_Type == "ASSET_TR" & Active == TRUE]
  req_spot_ccys <- setdiff(unique(active_assets$Return_Currency), c("USD", NA_character_))
  avail_spot_ccys <- dt[Series_Type == "FX_SPOT" & Active == TRUE, unique(Foreign_Currency)]
  missing_spots <- setdiff(req_spot_ccys, avail_spot_ccys)
  if (length(missing_spots) > 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "MISSING_REQUIRED_SPOT_SERIES",
      severity = "ERROR",
      message = sprintf("Fehlende aktive FX-Spot-Reihe für Währungen: %s", paste(missing_spots, collapse = ", ")),
      scope = "METADATA"
    )
  }

  avail_fwd_ccys <- dt[Series_Type == "FX_FORWARD" & Active == TRUE, unique(Foreign_Currency)]
  dt[, Forward_Available := TRUE]
  for (r in which(dt$Series_Type == "ASSET_TR")) {
    exp_ccy <- dt$FX_Exposure[r]
    if (!is.na(exp_ccy) && !(exp_ccy %in% c("USD", "EMEQ")) && !(exp_ccy %in% avail_fwd_ccys)) {
      dt[r, Forward_Available := FALSE]
      if (isTRUE(dt$Active[r]) && isTRUE(dt$Hedge_Allowed[r])) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "MISSING_FORWARD_FOR_ASSET_CURRENCY",
          severity = "WARNING",
          message = sprintf(
            "Für '%s' (Exposure %s) liegt keine 1M-Forward-Reihe vor. Ungesicherte Position bleibt nutzbar; %s-Hedging ist gesperrt.",
            dt$Code[r], exp_ccy, exp_ccy
          ),
          scope = "METADATA",
          series_code = dt$Code[r],
          details = list(currency = exp_ccy)
        )
      }
    }
  }

  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))
  list(
    valid = !has_errors,
    meta = dt,
    issues = issues
  )
}

validate_observations <- function(data, meta) {
  issues <- list()
  if (!is.data.frame(data) || nrow(data) == 0L) {
    issues[[1L]] <- make_issue(
      code = "EMPTY_OBSERVATION_DATA",
      severity = "ERROR",
      message = "Keine Beobachtungsdaten vorhanden.",
      scope = "OBSERVATIONS"
    )
    return(list(valid = FALSE, coverage = data.table::data.table(), issues = issues))
  }

  dt <- data.table::as.data.table(data)
  meta_dt <- data.table::as.data.table(meta)
  data.table::setorder(dt, Code, month_index)

  # Check duplicate (Code, month_key) pairs
  dup_obs <- dt[, .N, by = .(Code, month_key)][N > 1L]
  if (nrow(dup_obs) > 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "DUPLICATE_OBSERVATION_CELL",
      severity = "ERROR",
      message = sprintf("Doppelte Beobachtungen für (Code, Monat): %d Fälle.", nrow(dup_obs)),
      scope = "OBSERVATIONS"
    )
  }

  codes <- meta_dt$Code
  cov_list <- vector("list", length(codes))

  for (i in seq_along(codes)) {
    sc <- codes[i]
    stype <- meta_dt$Series_Type[i]
    vunit <- meta_dt$Value_Unit[i]
    is_act <- isTRUE(meta_dt$Active[i])
    sub <- dt[Code == sc]

    if (nrow(sub) == 0L) {
      cov_list[[i]] <- data.table::data.table(
        Code = sc,
        Series_Type = stype,
        Active = is_act,
        Start_Date = as.Date(NA),
        End_Date = as.Date(NA),
        Start_Month = NA_character_,
        End_Month = NA_character_,
        Obs_Count = 0L,
        Leading_NA = 0L,
        Trailing_NA = 0L,
        Interior_Gaps = 0L,
        Min_Value = NA_real_,
        Max_Value = NA_real_
      )
      next
    }

    non_na_idx <- which(!is.na(sub$value))
    if (length(non_na_idx) == 0L) {
      cov_list[[i]] <- data.table::data.table(
        Code = sc,
        Series_Type = stype,
        Active = is_act,
        Start_Date = as.Date(NA),
        End_Date = as.Date(NA),
        Start_Month = NA_character_,
        End_Month = NA_character_,
        Obs_Count = 0L,
        Leading_NA = nrow(sub),
        Trailing_NA = 0L,
        Interior_Gaps = 0L,
        Min_Value = NA_real_,
        Max_Value = NA_real_
      )
      if (is_act) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "ACTIVE_SERIES_ALL_NA",
          severity = "ERROR",
          message = sprintf("Aktive Zeitreihe '%s' enthält ausschliesslich fehlende Werte (NA).", sc),
          scope = "OBSERVATIONS",
          series_code = sc
        )
      }
      next
    }

    first_idx <- min(non_na_idx)
    last_idx <- max(non_na_idx)
    span_vals <- sub$value[first_idx:last_idx]
    span_months <- sub$month_key[first_idx:last_idx]
    interior_na_pos <- which(is.na(span_vals))
    n_interior_na <- length(interior_na_pos)

    # Check also if calendar month_index skipped inside [first_idx, last_idx]
    span_m_idx <- sub$month_index[first_idx:last_idx]
    calendar_jumps <- sum(pmax(0L, diff(span_m_idx) - 1L))
    total_interior_gaps <- n_interior_na + calendar_jumps

    if (total_interior_gaps > 0L) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "INTERIOR_SERIES_GAP",
        severity = if (is_act) "ERROR" else "WARNING",
        message = sprintf(
          "Zeitreihe '%s' weist %d Innenlücke(n) zwischen %s und %s auf (Monate: %s). Innenlücken werden nicht überbrückt.",
          sc, total_interior_gaps,
          sub$month_key[first_idx], sub$month_key[last_idx],
          paste(utils::head(span_months[interior_na_pos], 5L), collapse = ", ")
        ),
        scope = "OBSERVATIONS",
        series_code = sc,
        details = list(gap_months = span_months[interior_na_pos], count = total_interior_gaps)
      )
    }

    valid_vals <- sub$value[non_na_idx]
    min_v <- min(valid_vals)
    max_v <- max(valid_vals)

    # Value domain checks: TR levels and Outright FX must be strictly positive
    if (stype == "ASSET_TR" || (stype %in% c("FX_SPOT", "FX_FORWARD") && identical(vunit, "OUTRIGHT"))) {
      if (any(valid_vals <= 0)) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "NON_POSITIVE_LEVEL_VALUE",
          severity = "ERROR",
          message = sprintf(
            "Zeitreihe '%s' (%s) enthält Werte <= 0 (Min: %.6f). Indexniveaus und Outright-Kurse müssen strikt positiv sein.",
            sc, stype, min_v
          ),
          scope = "OBSERVATIONS",
          series_code = sc
        )
      }
    }

    cov_list[[i]] <- data.table::data.table(
      Code = sc,
      Series_Type = stype,
      Active = is_act,
      Start_Date = sub$date[first_idx],
      End_Date = sub$date[last_idx],
      Start_Month = sub$month_key[first_idx],
      End_Month = sub$month_key[last_idx],
      Obs_Count = length(non_na_idx),
      Leading_NA = first_idx - 1L,
      Trailing_NA = nrow(sub) - last_idx,
      Interior_Gaps = total_interior_gaps,
      Min_Value = min_v,
      Max_Value = max_v
    )
  }

  coverage_dt <- data.table::rbindlist(cov_list, fill = TRUE)
  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))

  list(
    valid = !has_errors,
    coverage = coverage_dt,
    issues = issues
  )
}

build_series_registry <- function(data, meta, policy = NULL, file_hash = NULL, source_path = NULL) {
  raw_codes <- unique(data$Code)
  meta_val <- validate_metadata(meta = meta, policy = policy, raw_codes = raw_codes, long_data = data)
  obs_val <- validate_observations(data = data, meta = meta_val$meta)

  meta_enriched <- merge(
    meta_val$meta,
    obs_val$coverage[, .(Code, Start_Date, End_Date, Start_Month, End_Month, Obs_Count, Leading_NA, Trailing_NA, Interior_Gaps, Min_Value, Max_Value)],
    by = "Code",
    all.x = TRUE,
    sort = FALSE
  )

  # Preserve exact row order from original metadata
  meta_enriched <- meta_enriched[match(meta_val$meta$Code, meta_enriched$Code)]

  issues <- c(meta_val$issues, obs_val$issues)
  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))

  meta_hash_cols <- intersect(
    c("Code", "Series_Type", "Active", "Return_Currency", "FX_Exposure", "Hedge_Allowed",
      "Foreign_Currency", "Quote_Currency", "Quote_Direction", "Quote_Scale",
      "Forward_Type", "Points_Multiplier", "Cash_Currency", "Proxy_Type", "Proxy_Until",
      "Preferred_Donors", "Is_Donor"),
    names(meta_enriched)
  )

  data_hash <- compute_object_hash(list(
    data = data[, .(month_key, Code, value)],
    meta = meta_enriched[, ..meta_hash_cols]
  ))

  registry <- list(
    valid = !has_errors,
    dataset_id = substr(data_hash, 1L, 12L),
    file_hash = file_hash %||% data_hash,
    data_hash = data_hash,
    source_path = source_path,
    data = data.table::copy(data),
    meta = meta_enriched,
    coverage = obs_val$coverage,
    active_assets = meta_enriched[Series_Type == "ASSET_TR" & Active == TRUE, Code],
    all_assets = meta_enriched[Series_Type == "ASSET_TR", Code],
    donor_factor_codes = meta_enriched[
      (Series_Type %in% c("ASSET_TR", "FACTOR", "FACTOR_DONOR") & Active == FALSE) | isTRUE(Is_Donor),
      Code
    ],
    fx_spot_codes = meta_enriched[Series_Type == "FX_SPOT" & Active == TRUE, Code],
    fx_forward_codes = meta_enriched[Series_Type == "FX_FORWARD" & Active == TRUE, Code],
    cash_codes = meta_enriched[Series_Type == "CASH_RATE" & Active == TRUE, Code],
    issues = issues
  )
  class(registry) <- c("maps_dataset", "list")
  registry
}

order_asset_universe <- function(codes, meta, base_ccy = "CHF") {
  if (is.null(codes) || length(codes) == 0L) return(character(0))
  meta_dt <- data.table::as.data.table(meta)

  valid_codes <- intersect(as.character(codes), meta_dt$Code)
  if (length(valid_codes) == 0L) return(as.character(codes))

  # 1. CASH assets: Domestic base_ccy cash first, then remaining cash rows in metadata order
  cash_rows <- meta_dt[Code %in% valid_codes & Asset_Class == "CASH"]
  cash_codes <- if (nrow(cash_rows) > 0L) {
    if ("Return_Currency" %in% names(cash_rows)) {
      dom_cash <- cash_rows[Return_Currency == base_ccy, Code]
      other_cash <- cash_rows[Return_Currency != base_ccy, Code]
      c(dom_cash, other_cash)
    } else {
      cash_rows$Code
    }
  } else character(0)

  # 2. Non-CASH assets: Preserve exact order of appearance in original Excel metadata
  non_cash_codes <- meta_dt[Code %in% valid_codes & (Asset_Class != "CASH" | is.na(Asset_Class)), Code]

  c(cash_codes, non_cash_codes)
}

