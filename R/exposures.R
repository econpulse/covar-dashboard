# ==============================================================================
# R/exposures.R
# Pre-hedge exposure derivation, mutually exclusive ASSET_CLASS and
# CURRENCY_OVERLAY hedge notional resolution, mode transitions, and A(h) matrix
# Pure R module - no Shiny dependencies
# ==============================================================================

derive_prehedge_exposures <- function(weights,
                                      meta,
                                      base_ccy = "CHF",
                                      available_hedge_currencies = NULL,
                                      fx_carry = NULL) {
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
      severity = "WARNING",
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
    available_hedge_currencies <- setdiff(unique(c("USD", "CHF", avail_fwd_ccys)), base_ccy)
  }

  is_dom_vec <- (sub_meta$FX_Exposure == base_ccy)
  is_em_vec <- (sub_meta$FX_Exposure == "EMEQ")
  is_ovl_vec <- (sub_meta$Return_Structure == "FUTURES_TR")
  meta_ha_vec <- !is.na(sub_meta$Hedge_Allowed) & (sub_meta$Hedge_Allowed == TRUE)
  def_hr_vec <- ifelse(is.na(sub_meta$Default_Hedge_Ratio), 0.0, as.numeric(sub_meta$Default_Hedge_Ratio))
  is_act_vec <- ifelse(is.na(sub_meta$Active), TRUE, as.logical(sub_meta$Active))
  fwd_avail_vec <- ifelse(is_dom_vec, TRUE, ifelse(is_em_vec, FALSE, sub_meta$FX_Exposure %in% available_hedge_currencies))
  hedge_elig_vec <- (!is_dom_vec) & (!is_em_vec) & meta_ha_vec & fwd_avail_vec
  elig_reason_vec <- ifelse(
    is_dom_vec, "INLAND_KEIN_HEDGE",
    ifelse(is_em_vec, "EMEQ_NICHT_HEDGEBAR",
      ifelse(!meta_ha_vec, "METADATEN_GESPERRT",
        ifelse(!fwd_avail_vec, "FORWARD_DATEN_FEHLEN", "HEDGEFAEHIG")
      )
    )
  )

  asset_dt <- data.table::data.table(
    asset_id = w_Ids,
    name = sub_meta$Name,
    asset_class = sub_meta$Asset_Class,
    sub_asset_class = sub_meta$Sub_Asset_Class,
    return_currency = sub_meta$Return_Currency,
    fx_exposure = sub_meta$FX_Exposure,
    return_structure = sub_meta$Return_Structure,
    weight = w_vals,
    is_domestic = is_dom_vec,
    is_emeq = is_em_vec,
    is_contractual_overlay = is_ovl_vec,
    meta_hedge_allowed = meta_ha_vec,
    default_hedge_ratio = def_hr_vec,
    is_active = is_act_vec,
    forward_available = fwd_avail_vec,
    hedge_eligible = hedge_elig_vec,
    eligibility_reason = elig_reason_vec
  )

  # Aggregate currency exposure table before hedge
  # Keep all relevant exposure buckets (base_ccy, available foreign hedge currencies, asset exposures) visible
  all_exp_buckets <- unique(c(base_ccy, available_hedge_currencies, sub_meta$FX_Exposure))
  all_exp_buckets <- c(base_ccy, setdiff(all_exp_buckets, base_ccy))
  n_ccy <- length(all_exp_buckets)

  ccy_is_dom <- all_exp_buckets == base_ccy
  ccy_is_em <- all_exp_buckets == "EMEQ"
  ccy_fwd_ok <- ifelse(ccy_is_dom, TRUE, ifelse(ccy_is_em, FALSE, all_exp_buckets %in% available_hedge_currencies))
  ccy_status <- ifelse(
    ccy_is_dom, "DOMESTIC",
    ifelse(ccy_is_em, "EMEQ_UNHEDGED",
      ifelse(!ccy_fwd_ok, "MISSING_FORWARD", "HEDGEABLE")
    )
  )
  ccy_e_before <- numeric(n_ccy)
  ccy_e_hedgeable <- numeric(n_ccy)
  ccy_e_locked <- numeric(n_ccy)
  ccy_carry <- rep(NA_real_, n_ccy)
  ccy_ovl <- numeric(n_ccy)

  fx_exp_col <- sub_meta$FX_Exposure
  for (k in seq_len(n_ccy)) {
    ccy <- all_exp_buckets[k]
    idx <- which(fx_exp_col == ccy)
    if (length(idx) > 0L) {
      w_sub <- w_vals[idx]
      el_sub <- hedge_elig_vec[idx]
      ccy_e_before[k] <- sum(w_sub, na.rm = TRUE)
      ccy_e_hedgeable[k] <- sum(w_sub[el_sub], na.rm = TRUE)
      ccy_e_locked[k] <- sum(w_sub[!el_sub], na.rm = TRUE)
      ccy_ovl[k] <- sum(w_sub[is_ovl_vec[idx]], na.rm = TRUE)
    }
    if (ccy_is_dom[k]) {
      ccy_carry[k] <- 0.0
    } else if (!is.null(fx_carry) && ccy %in% names(fx_carry)) {
      ccy_carry[k] <- as.numeric(fx_carry[[ccy]])
    }
  }

  currency_dt <- data.table::data.table(
    base_currency = rep(base_ccy, n_ccy),
    fx_exposure = all_exp_buckets,
    is_domestic = ccy_is_dom,
    is_emeq = ccy_is_em,
    forward_available = ccy_fwd_ok,
    status = ccy_status,
    exposure_before_hedge = ccy_e_before,
    hedgeable_exposure = ccy_e_hedgeable,
    non_hedgeable_exposure = ccy_e_locked,
    latest_carry_pa = ccy_carry,
    contractual_overlay_exposure = ccy_ovl
  )

  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))

  list(
    valid = !has_errors,
    base_currency = base_ccy,
    weight_sum = w_sum,
    equity_share = sum(w_vals[sub_meta$Asset_Class == "EQUITY"], na.rm = TRUE),
    foreign_total_exposure = sum(w_vals[!is_dom_vec], na.rm = TRUE),
    foreign_hedgeable_exposure = sum(w_vals[hedge_elig_vec], na.rm = TRUE),
    emeq_exposure = sum(w_vals[is_em_vec], na.rm = TRUE),
    domestic_exposure = sum(w_vals[is_dom_vec], na.rm = TRUE),
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

  data.table::set(asset_dt, j = "requested_h", value = req_h_vec)
  data.table::set(asset_dt, j = "applied_h", value = app_h_vec)
  data.table::set(asset_dt, j = "hedge_notional", value = notional_vec)
  data.table::set(asset_dt, j = "post_hedge_exposure", value = ifelse(asset_dt$is_domestic, asset_dt$weight, asset_dt$weight - notional_vec))

  # Update currency exposure summary table:
  # E_{c,vor} = Sum_i w_i * e_{i,c}
  # g_c = Sum_i w_i * e_{i,c} * h_{i,c}
  # E_{c,nach} = E_{c,vor} - g_c
  # When hedgeable_exposure == 0 (or EMEQ / domestic), effective_hedge_ratio is NA_real_ ("n/a"), never 0/0!
  ccy_fx_vec <- ccy_dt$fx_exposure
  asset_fx_vec <- asset_dt$fx_exposure
  g_notional_vec <- vapply(ccy_fx_vec, function(ccy) {
    sum(notional_vec[asset_fx_vec == ccy], na.rm = TRUE)
  }, numeric(1))

  e_before_vec <- ccy_dt$exposure_before_hedge
  e_after_vec <- e_before_vec - g_notional_vec
  e_hedgeable_vec <- ccy_dt$hedgeable_exposure
  not_dom_em <- (!ccy_dt$is_domestic) & (!ccy_dt$is_emeq)
  eff_hr_vec <- ifelse(
    not_dom_em & ccy_dt$forward_available & (e_hedgeable_vec > 1e-12),
    g_notional_vec / e_hedgeable_vec,
    NA_real_
  )
  tot_share_vec <- ifelse(
    not_dom_em & (e_before_vec > 1e-12),
    g_notional_vec / e_before_vec,
    NA_real_
  )

  data.table::set(ccy_dt, j = "hedge_notional", value = g_notional_vec)
  data.table::set(ccy_dt, j = "exposure_after_hedge", value = e_after_vec)
  data.table::set(ccy_dt, j = "effective_hedge_ratio", value = eff_hr_vec)
  data.table::set(ccy_dt, j = "total_fx_hedge_share", value = tot_share_vec)

  # Derived read-only representations for the inactive mode
  derived_currency_ratios <- stats::setNames(
    as.list(ifelse(is.na(eff_hr_vec), 0.0, eff_hr_vec)),
    ccy_fx_vec
  )

  ac_vec <- asset_dt$Asset_Class %||% asset_dt$asset_class
  uniq_ac <- unique(ac_vec)
  w_vec_dt <- asset_dt$weight
  elig_vec_dt <- asset_dt$hedge_eligible
  ac_tot_w <- numeric(length(uniq_ac))
  ac_hed_w <- numeric(length(uniq_ac))
  ac_notional <- numeric(length(uniq_ac))
  for (k in seq_along(uniq_ac)) {
    idx_ac <- which(ac_vec == uniq_ac[k])
    w_s <- w_vec_dt[idx_ac]
    el_s <- elig_vec_dt[idx_ac]
    ac_tot_w[k] <- sum(w_s, na.rm = TRUE)
    ac_hed_w[k] <- sum(w_s[el_s], na.rm = TRUE)
    ac_notional[k] <- sum(notional_vec[idx_ac], na.rm = TRUE)
  }
  derived_class_table <- data.table::data.table(
    asset_class = uniq_ac,
    total_weight = ac_tot_w,
    hedgeable_weight = ac_hed_w,
    hedge_notional = ac_notional,
    effective_class_ratio = ifelse(ac_hed_w > 1e-12, ac_notional / ac_hed_w, NA_real_)
  )

  # Named vector of foreign hedge notionals g_c keyed by "HEDGE_<CCY>"
  fh_mask <- not_dom_em & ccy_dt$forward_available
  g_vec <- stats::setNames(g_notional_vec[fh_mask], paste0("HEDGE_", ccy_fx_vec[fh_mask]))

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
    total_hedge_notional = sum(g_notional_vec, na.rm = TRUE),
    foreign_exposure_before = sum(e_before_vec[!ccy_dt$is_domestic], na.rm = TRUE),
    foreign_exposure_after = sum(e_after_vec[!ccy_dt$is_domestic], na.rm = TRUE),
    hedgeable_foreign_before = sum(e_hedgeable_vec[fh_mask], na.rm = TRUE),
    hedgeable_foreign_after = sum((e_hedgeable_vec - g_notional_vec)[fh_mask], na.rm = TRUE),
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
