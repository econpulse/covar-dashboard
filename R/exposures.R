# ==============================================================================
# R/exposures.R
# Pre-hedge exposure derivation, mutually exclusive ASSET_CLASS and
# CURRENCY_OVERLAY hedge notional resolution, mode transitions, and A(h) matrix
# Pure R module - no Shiny dependencies
# ==============================================================================

derive_prehedge_exposures <- function(weights,
                                      meta,
                                      base_ccy = "CHF",
                                      available_hedge_currencies = NULL) {
  issues <- list()
  meta_dt <- data.table::as.data.table(meta)
  asset_meta <- meta_dt[Series_Type == "ASSET_TR"]

  if (is.null(names(weights))) {
    stop("weights must be a named numeric vector keyed by asset Code", call. = FALSE)
  }

  w_Ids <- names(weights)
  w_vals <- as.numeric(weights)
  names(w_vals) <- w_Ids

  w_sum <- sum(w_vals, na.rm = TRUE)
  if (any(is.na(w_vals)) || any(is.nan(w_vals)) || any(is.infinite(w_vals))) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "INVALID_WEIGHT_NUMBERS",
      severity = "ERROR",
      message = "Portfoliogewichte enthalten ungültige Zahlen (NA/NaN/Inf).",
      scope = "EXPOSURE"
    )
  }
  if (any(w_vals < -1e-9)) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "NEGATIVE_ASSET_WEIGHT",
      severity = "ERROR",
      message = "Negative Kapitalgewichte sind im MVP nicht zulässig.",
      scope = "EXPOSURE"
    )
  }
  if (abs(w_sum - 1.0) > 1e-4) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "WEIGHT_SUM_NOT_ONE",
      severity = "ERROR",
      message = sprintf("Summe der Kapitalgewichte muss 100.0%% ergeben (aktuell: %.2f%%).", 100.0 * w_sum),
      scope = "EXPOSURE",
      details = list(weight_sum = w_sum)
    )
  }

  sub_meta <- asset_meta[match(w_Ids, Code)]
  if (any(is.na(sub_meta$Code))) {
    unknown_codes <- w_Ids[is.na(sub_meta$Code)]
    issues[[length(issues) + 1L]] <- make_issue(
      code = "UNKNOWN_ASSET_CODE_IN_WEIGHTS",
      severity = "ERROR",
      message = sprintf("Unbekannte Asset-Codes in Gewichtsvektor: %s", paste(unknown_codes, collapse = ", ")),
      scope = "EXPOSURE"
    )
  }

  if (is.null(available_hedge_currencies)) {
    avail_fwd_ccys <- meta_dt[Series_Type == "FX_FORWARD" & Active == TRUE, unique(Foreign_Currency)]
    available_hedge_currencies <- setdiff(unique(c("USD", avail_fwd_ccys)), base_ccy)
  }

  asset_dt <- data.table::data.table(
    asset_id = w_Ids,
    name = sub_meta$Name,
    asset_class = sub_meta$Asset_Class,
    sub_asset_class = sub_meta$Sub_Asset_Class,
    return_currency = sub_meta$Return_Currency,
    fx_exposure = sub_meta$FX_Exposure,
    return_structure = sub_meta$Return_Structure,
    weight = w_vals,
    is_domestic = (sub_meta$FX_Exposure == base_ccy),
    is_emeq = (sub_meta$FX_Exposure == "EMEQ"),
    is_contractual_overlay = (sub_meta$Return_Structure == "FUTURES_TR"),
    meta_hedge_allowed = isTRUE(sub_meta$Hedge_Allowed) | (sub_meta$Hedge_Allowed == TRUE),
    default_hedge_ratio = ifelse(is.na(sub_meta$Default_Hedge_Ratio), 0.0, as.numeric(sub_meta$Default_Hedge_Ratio))
  )

  asset_dt[, forward_available := ifelse(
    is_domestic,
    TRUE,
    ifelse(is_emeq, FALSE, fx_exposure %in% available_hedge_currencies)
  )]

  asset_dt[, hedge_eligible := (!is_domestic) & (!is_emeq) & meta_hedge_allowed & forward_available]

  asset_dt[, eligibility_reason := ifelse(
    is_domestic, "INLAND_KEIN_HEDGE",
    ifelse(is_emeq, "EMEQ_NICHT_HEDGEBAR",
      ifelse(!meta_hedge_allowed, "METADATEN_GESPERRT",
        ifelse(!forward_available, "FORWARD_DATEN_FEHLEN", "HEDGEFAEHIG")
      )
    )
  )]

  # Aggregate currency exposure table before hedge
  # Keep all relevant exposure buckets (base_ccy, USD, EUR, CHF, JPY, GBP, EMEQ) visible
  all_exp_buckets <- unique(c(base_ccy, asset_dt$fx_exposure))
  ccy_rows <- lapply(all_exp_buckets, function(ccy) {
    sub <- asset_dt[fx_exposure == ccy]
    e_before <- sum(sub$weight, na.rm = TRUE)
    e_hedgeable <- sum(sub[hedge_eligible == TRUE, weight], na.rm = TRUE)
    e_locked <- sum(sub[hedge_eligible == FALSE, weight], na.rm = TRUE)
    is_dom <- identical(ccy, base_ccy)
    is_em <- identical(ccy, "EMEQ")
    fwd_ok <- if (is_dom) TRUE else if (is_em) FALSE else (ccy %in% available_hedge_currencies)

    status_code <- if (is_dom) {
      "DOMESTIC"
    } else if (is_em) {
      "EMEQ_UNHEDGED"
    } else if (!fwd_ok) {
      "MISSING_FORWARD"
    } else {
      "HEDGEABLE"
    }

    data.table::data.table(
      base_currency = base_ccy,
      fx_exposure = ccy,
      is_domestic = is_dom,
      is_emeq = is_em,
      forward_available = fwd_ok,
      status = status_code,
      exposure_before_hedge = e_before,
      hedgeable_exposure = e_hedgeable,
      non_hedgeable_exposure = e_locked,
      contractual_overlay_exposure = sum(sub[is_contractual_overlay == TRUE, weight], na.rm = TRUE)
    )
  })
  currency_dt <- data.table::rbindlist(ccy_rows, fill = TRUE)

  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))

  list(
    valid = !has_errors,
    base_currency = base_ccy,
    weight_sum = w_sum,
    equity_share = sum(asset_dt[asset_class == "EQUITY", weight], na.rm = TRUE),
    foreign_total_exposure = sum(asset_dt[is_domestic == FALSE, weight], na.rm = TRUE),
    foreign_hedgeable_exposure = sum(asset_dt[hedge_eligible == TRUE, weight], na.rm = TRUE),
    emeq_exposure = sum(asset_dt[is_emeq == TRUE, weight], na.rm = TRUE),
    domestic_exposure = sum(asset_dt[is_domestic == TRUE, weight], na.rm = TRUE),
    asset_exposures = asset_dt,
    currency_exposures = currency_dt,
    available_hedge_currencies = available_hedge_currencies,
    issues = issues
  )
}

resolve_hedge_notionals <- function(exposures,
                                    hedge_config = list(),
                                    strict_missing_forward = FALSE) {
  issues <- exposures$issues
  base_ccy <- exposures$base_currency
  asset_dt <- data.table::copy(exposures$asset_exposures)
  ccy_dt <- data.table::copy(exposures$currency_exposures)

  mode <- toupper(hedge_config$mode %||% "ASSET_CLASS")
  if (!(mode %in% c("ASSET_CLASS", "CURRENCY_OVERLAY"))) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "INVALID_HEDGE_MODE",
      severity = "ERROR",
      message = sprintf("Ungültiger Hedge-Modus '%s' (erlaubt: ASSET_CLASS oder CURRENCY_OVERLAY).", mode),
      scope = "HEDGE"
    )
    return(list(valid = FALSE, issues = issues))
  }

  n_rows <- nrow(asset_dt)
  req_h_vec <- numeric(n_rows)
  app_h_vec <- numeric(n_rows)
  notional_vec <- numeric(n_rows)

  if (identical(mode, "ASSET_CLASS")) {
    class_ratios <- as.list(hedge_config$class_ratios %||% list())
    asset_overrides <- as.list(hedge_config$asset_overrides %||% list())
    ccy_filter <- as.list(hedge_config$currency_ratios_by_class %||% list())

    for (i in seq_len(n_rows)) {
      aid <- asset_dt$asset_id[i]
      ac <- asset_dt$asset_class[i]
      ccy <- asset_dt$fx_exposure[i]
      w_i <- asset_dt$weight[i]

      if (asset_dt$is_domestic[i] || asset_dt$is_emeq[i]) {
        if (!is.null(asset_overrides[[aid]]) && !is.na(asset_overrides[[aid]]) && as.numeric(asset_overrides[[aid]]) > 0) {
          issues[[length(issues) + 1L]] <- make_issue(
            code = "INVALID_OVERRIDE_ON_UNHEDGEABLE_ASSET",
            severity = "ERROR",
            message = sprintf(
              "Asset '%s' (%s) kann nicht abgesichert werden (%s).",
              aid, ccy, asset_dt$eligibility_reason[i]
            ),
            scope = "HEDGE",
            series_code = aid
          )
        }
        next
      }

      has_explicit_override <- !is.null(asset_overrides[[aid]]) && length(asset_overrides[[aid]]) > 0L && !is.na(asset_overrides[[aid]])
      ac_ccy_map <- if (!is.null(ccy_filter[[ac]])) as.list(ccy_filter[[ac]]) else list()
      h_req <- if (has_explicit_override) {
        as.numeric(asset_overrides[[aid]])
      } else if (!is.null(ac_ccy_map[[ccy]])) {
        as.numeric(ac_ccy_map[[ccy]])
      } else if (!is.null(class_ratios[[ac]])) {
        as.numeric(class_ratios[[ac]])
      } else {
        0.0
      }

      if (is.na(h_req) || h_req < -1e-9 || h_req > 1.0 + 1e-9) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "HEDGE_RATIO_OUT_OF_BOUNDS",
          severity = "ERROR",
          message = sprintf(
            "Hedgequote für '%s' (%.1f%%) liegt ausserhalb des zulässigen Bereichs [0%%, 100%%]. Kein Overhedging oder spekulatives Overlay zulässig.",
            aid, 100.0 * h_req
          ),
          scope = "HEDGE",
          series_code = aid
        )
        h_req <- min(1.0, max(0.0, h_req %||% 0.0))
      }

      req_h_vec[i] <- h_req

      if (!asset_dt$forward_available[i]) {
        if (w_i > 0 && h_req > 0) {
          if (has_explicit_override || isTRUE(strict_missing_forward)) {
            issues[[length(issues) + 1L]] <- make_issue(
              code = "MISSING_FORWARD_FOR_REQUESTED_HEDGE",
              severity = "ERROR",
              message = sprintf(
                "Hedge für '%s' (%s, Quote %.0f%%) abgelehnt: 1M-Forwarddaten für %s fehlen im Datensatz.",
                aid, ccy, 100.0 * h_req, ccy
              ),
              scope = "HEDGE",
              series_code = aid,
              details = list(currency = ccy, requested_h = h_req)
            )
          } else {
            issues[[length(issues) + 1L]] <- make_issue(
              code = "LOCKED_CURRENCY_SKIPPED_IN_CLASS_HEDGE",
              severity = "WARNING",
              message = sprintf(
                "Asset '%s' (%s) ist im Klassen-Hedge '%s' (%.0f%%) ausgenommen, da keine %s-1M-Forwarddaten vorliegen.",
                aid, ccy, ac, 100.0 * h_req, ccy
              ),
              scope = "HEDGE",
              series_code = aid,
              details = list(currency = ccy, asset_class = ac)
            )
          }
        }
      } else if (asset_dt$hedge_eligible[i]) {
        app_h_vec[i] <- h_req
        notional_vec[i] <- w_i * h_req
      }
    }
  } else {
    # CURRENCY_OVERLAY mode: h_c applied to total hedgeable foreign exposure E_{c,hedgefaehig}
    ccy_ratios <- as.list(hedge_config$currency_ratios %||% list())

    if (!is.null(ccy_ratios[["EMEQ"]]) && as.numeric(ccy_ratios[["EMEQ"]]) > 0) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "EMEQ_HEDGE_NOT_ALLOWED",
        severity = "ERROR",
        message = "EMEQ (Emerging Markets Aktien) ist nicht hedgefähig.",
        scope = "HEDGE"
      )
    }

    for (ccy in names(ccy_ratios)) {
      if (identical(ccy, base_ccy) || identical(ccy, "EMEQ")) next
      h_c <- as.numeric(ccy_ratios[[ccy]])
      if (is.na(h_c)) next

      if (h_c < -1e-9 || h_c > 1.0 + 1e-9) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "CURRENCY_HEDGE_RATIO_OUT_OF_BOUNDS",
          severity = "ERROR",
          message = sprintf(
            "Währungs-Hedgequote für %s (%.1f%%) liegt ausserhalb von [0%%, 100%%].",
            ccy, 100.0 * h_c
          ),
          scope = "HEDGE"
        )
        h_c <- min(1.0, max(0.0, h_c))
      }

      sub_idx <- which(asset_dt$fx_exposure == ccy)
      if (length(sub_idx) == 0L) next

      e_before_c <- sum(asset_dt$weight[sub_idx], na.rm = TRUE)
      fwd_ok_c <- all(asset_dt$forward_available[sub_idx])

      if (!fwd_ok_c && h_c > 0 && e_before_c > 0) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "MISSING_FORWARD_FOR_CURRENCY_OVERLAY",
          severity = "ERROR",
          message = sprintf(
            "Währungs-Overlay für %s (Quote %.0f%% bei %.1f%% Exposure) abgelehnt: 1M-Forwarddaten für %s fehlen.",
            ccy, 100.0 * h_c, 100.0 * e_before_c, ccy
          ),
          scope = "HEDGE",
          details = list(currency = ccy, requested_h = h_c, exposure = e_before_c)
        )
        next
      }

      elig_idx <- sub_idx[asset_dt$hedge_eligible[sub_idx] == TRUE]
      if (length(elig_idx) > 0L) {
        req_h_vec[elig_idx] <- h_c
        app_h_vec[elig_idx] <- h_c
        notional_vec[elig_idx] <- asset_dt$weight[elig_idx] * h_c
      }
    }
  }

  asset_dt[, `:=`(
    requested_h = req_h_vec,
    applied_h = app_h_vec,
    hedge_notional = notional_vec,
    post_hedge_exposure = ifelse(is_domestic, weight, weight - notional_vec)
  )]

  # Update currency exposure summary table:
  # E_{c,vor} = Sum_i w_i * e_{i,c}
  # g_c = Sum_i w_i * e_{i,c} * h_{i,c}
  # E_{c,nach} = E_{c,vor} - g_c
  # When hedgeable_exposure == 0 (or EMEQ / domestic), effective_hedge_ratio is NA_real_ ("n/a"), never 0/0!
  g_by_ccy <- asset_dt[, .(
    g_notional = sum(hedge_notional, na.rm = TRUE)
  ), by = fx_exposure]

  ccy_dt <- merge(ccy_dt, g_by_ccy, by = "fx_exposure", all.x = TRUE, sort = FALSE)
  ccy_dt[is.na(g_notional), g_notional := 0.0]
  ccy_dt[, `:=`(
    hedge_notional = g_notional,
    exposure_after_hedge = exposure_before_hedge - g_notional,
    effective_hedge_ratio = ifelse(
      !is_domestic & !is_emeq & forward_available & hedgeable_exposure > 1e-12,
      g_notional / hedgeable_exposure,
      NA_real_
    ),
    total_fx_hedge_share = ifelse(
      !is_domestic & !is_emeq & exposure_before_hedge > 1e-12,
      g_notional / exposure_before_hedge,
      NA_real_
    )
  )]
  ccy_dt[, g_notional := NULL]

  # Preserve original currency order
  ccy_dt <- ccy_dt[match(exposures$currency_exposures$fx_exposure, ccy_dt$fx_exposure)]

  # Derived read-only representations for the inactive mode
  derived_currency_ratios <- stats::setNames(
    as.list(ifelse(is.na(ccy_dt$effective_hedge_ratio), 0.0, ccy_dt$effective_hedge_ratio)),
    ccy_dt$fx_exposure
  )
  derived_class_table <- asset_dt[, .(
    total_weight = sum(weight, na.rm = TRUE),
    hedgeable_weight = sum(ifelse(hedge_eligible, weight, 0.0), na.rm = TRUE),
    hedge_notional = sum(hedge_notional, na.rm = TRUE),
    effective_class_ratio = ifelse(
      sum(ifelse(hedge_eligible, weight, 0.0), na.rm = TRUE) > 1e-12,
      sum(hedge_notional, na.rm = TRUE) / sum(ifelse(hedge_eligible, weight, 0.0), na.rm = TRUE),
      NA_real_
    )
  ), by = asset_class]

  # Named vector of foreign hedge notionals g_c keyed by "HEDGE_<CCY>"
  foreign_hedges <- ccy_dt[is_domestic == FALSE & is_emeq == FALSE & forward_available == TRUE]
  g_vec <- stats::setNames(foreign_hedges$hedge_notional, paste0("HEDGE_", foreign_hedges$fx_exposure))

  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))

  out <- list(
    valid = !has_errors,
    base_currency = base_ccy,
    active_mode = mode,
    allocation_attribution_note = if (identical(mode, "CURRENCY_OVERLAY")) {
      "PROPORTIONAL_TO_HEDGEABLE_EXPOSURE (Währungs-Overlay proportional auf hedgefähige Assets derselben Währung umgelegt; nicht eindeutig auf Assetklassen rückführbar)"
    } else {
      "DIRECT_ASSET_CLASS_MAPPING"
    },
    weight_sum = exposures$weight_sum,
    total_hedge_notional = sum(ccy_dt$hedge_notional, na.rm = TRUE),
    foreign_exposure_before = sum(ccy_dt[is_domestic == FALSE, exposure_before_hedge], na.rm = TRUE),
    foreign_exposure_after = sum(ccy_dt[is_domestic == FALSE, exposure_after_hedge], na.rm = TRUE),
    hedgeable_foreign_before = sum(ccy_dt[is_domestic == FALSE & is_emeq == FALSE & forward_available == TRUE, hedgeable_exposure], na.rm = TRUE),
    hedgeable_foreign_after = sum(ccy_dt[is_domestic == FALSE & is_emeq == FALSE & forward_available == TRUE, hedgeable_exposure - hedge_notional], na.rm = TRUE),
    emeq_exposure = exposures$emeq_exposure,
    domestic_exposure = exposures$domestic_exposure,
    g_vector = g_vec,
    asset_table = asset_dt,
    currency_table = ccy_dt,
    derived_class_table = derived_class_table,
    derived_currency_ratios = derived_currency_ratios,
    issues = issues
  )
  class(out) <- c("maps_exposure_report", "list")
  out
}

convert_hedge_mode <- function(exposures,
                               current_config,
                               target_mode = c("CURRENCY_OVERLAY", "ASSET_CLASS"),
                               saved_asset_class_config = NULL,
                               fallback_policy = c("RESTORE_SAVED", "PROPORTIONAL")) {
  target_mode <- match.arg(target_mode)
  fallback_policy <- match.arg(fallback_policy)

  resolved <- resolve_hedge_notionals(exposures, current_config)

  if (identical(target_mode, "CURRENCY_OVERLAY")) {
    # Switching from ASSET_CLASS to CURRENCY_OVERLAY preserves aggregate g_c exactly
    return(list(
      mode = "CURRENCY_OVERLAY",
      currency_ratios = resolved$derived_currency_ratios,
      transition_type = "EXACT_AGGREGATE_PRESERVATION",
      saved_asset_class_config = if (identical(current_config$mode, "ASSET_CLASS")) current_config else saved_asset_class_config
    ))
  } else {
    # Switching from CURRENCY_OVERLAY to ASSET_CLASS has no unique inverse reconstruction:
    # Either restore saved ASSET_CLASS config or apply explicitly labeled proportional allocation
    if (identical(fallback_policy, "RESTORE_SAVED") && !is.null(saved_asset_class_config)) {
      return(list(
        mode = "ASSET_CLASS",
        class_ratios = saved_asset_class_config$class_ratios,
        asset_overrides = saved_asset_class_config$asset_overrides %||% list(),
        transition_type = "RESTORED_SAVED_ASSET_CLASS_CONFIG",
        saved_asset_class_config = saved_asset_class_config
      ))
    } else {
      prop_overrides <- stats::setNames(
        as.list(resolved$asset_table$applied_h),
        resolved$asset_table$asset_id
      )
      class_r <- stats::setNames(
        as.list(ifelse(is.na(resolved$derived_class_table$effective_class_ratio), 0.0, resolved$derived_class_table$effective_class_ratio)),
        resolved$derived_class_table$asset_class
      )
      return(list(
        mode = "ASSET_CLASS",
        class_ratios = class_r,
        asset_overrides = prop_overrides,
        transition_type = "PROPORTIONAL_ALLOCATION_FROM_OVERLAY",
        saved_asset_class_config = saved_asset_class_config
      ))
    }
  }
}

build_asset_transform <- function(resolved_hedges, component_ids) {
  # Builds transformation matrix A(h) of dimension N_assets x N_components
  # such that hedged asset return vector r(h) = A(h) %*% X_B
  # and Sigma_{assets,B,h} = A(h) %*% Sigma_B %*% t(A(h))
  asset_dt <- resolved_hedges$asset_table
  asset_ids <- asset_dt$asset_id
  n_assets <- length(asset_ids)
  n_comp <- length(component_ids)

  A_mat <- matrix(0.0, nrow = n_assets, ncol = n_comp, dimnames = list(asset_ids, component_ids))

  for (i in seq_len(n_assets)) {
    aid <- asset_dt$asset_id[i]
    if (aid %in% component_ids) {
      A_mat[aid, aid] <- 1.0
    }
    h_i <- asset_dt$applied_h[i]
    ccy <- asset_dt$fx_exposure[i]
    hedge_col <- paste0("HEDGE_", ccy)
    if (!is.na(h_i) && h_i != 0.0 && (hedge_col %in% component_ids)) {
      A_mat[aid, hedge_col] <- h_i
    }
  }

  list(
    A_matrix = A_mat,
    asset_ids = asset_ids,
    component_ids = component_ids,
    allocation_note = resolved_hedges$allocation_attribution_note
  )
}
