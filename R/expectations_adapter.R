# ==============================================================================
# R/expectations_adapter.R
# Validation and normalization of versioned 5-year forward-looking return
# scenario assumptions (USD-numeraire triangular consistency, GSGCTOT futures check)
# Pure R module - no Shiny dependencies
# ==============================================================================

validate_expectations <- function(payload, meta = NULL) {
  issues <- list()
  if (is.null(payload) || !is.list(payload)) {
    issues[[1L]] <- make_issue(
      code = "EMPTY_EXPECTATIONS_PAYLOAD",
      severity = "ERROR",
      message = "Keine gültige Renditeannahmen-Struktur übergeben.",
      scope = "EXPECTATIONS"
    )
    return(list(valid = FALSE, expectations = NULL, issues = issues))
  }

  req_top <- c("expectations_id", "as_of", "horizon", "fee_basis", "return_definition", "uncertainty_description", "scenarios")
  missing_top <- setdiff(req_top, names(payload))
  if (length(missing_top) > 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "MISSING_EXPECTATIONS_FIELDS",
      severity = "ERROR",
      message = sprintf("Pflichtfelder in Renditeannahmen fehlen: %s", paste(missing_top, collapse = ", ")),
      scope = "EXPECTATIONS"
    )
    return(list(valid = FALSE, expectations = payload, issues = issues))
  }

  horizon <- as.integer(payload$horizon %||% 5L)
  if (is.na(horizon) || horizon < 1L || horizon > 30L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "INVALID_EXPECTATIONS_HORIZON",
      severity = "ERROR",
      message = sprintf("Ungültiger Szenariohorizont: %s Jahre.", as.character(payload$horizon)),
      scope = "EXPECTATIONS"
    )
  }

  scenarios <- payload$scenarios
  if (length(scenarios) == 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "NO_SCENARIOS_DEFINED",
      severity = "ERROR",
      message = "Mindestens ein Renditeszenario muss definiert sein.",
      scope = "EXPECTATIONS"
    )
  }

  for (scen_id in names(scenarios)) {
    sc <- scenarios[[scen_id]]
    num_paths <- sc$usd_numeraire_paths
    if (is.null(num_paths) || is.null(num_paths$USD)) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "MISSING_USD_NUMERAIRE_PATH",
        severity = "ERROR",
        message = sprintf("Szenario '%s' benötigt konsistente USD-Numeraire-Währungspfade (usd_numeraire_paths).", scen_id),
        scope = "EXPECTATIONS"
      )
    } else {
      usd_vec <- as.numeric(num_paths$USD)
      if (any(abs(usd_vec - 1.0) > 1e-9)) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "NON_UNIT_USD_NUMERAIRE_PATH",
          severity = "ERROR",
          message = sprintf("Szenario '%s': USD-Numeraire-Pfad muss durchgehend 1.0 betragen.", scen_id),
          scope = "EXPECTATIONS"
        )
      }
      for (ccy in names(num_paths)) {
        p_vec <- as.numeric(num_paths[[ccy]])
        if (length(p_vec) != horizon || any(is.na(p_vec)) || any(p_vec <= 0)) {
          issues[[length(issues) + 1L]] <- make_issue(
            code = "INVALID_CURRENCY_PATH_VECTOR",
            severity = "ERROR",
            message = sprintf("Szenario '%s': Währungspfad für %s muss %d positive Jahresfaktoren enthalten.", scen_id, ccy, horizon),
            scope = "EXPECTATIONS"
          )
        }
      }
    }

    assets_cfg <- sc$asset_assumptions
    if (is.null(assets_cfg) || length(assets_cfg) == 0L) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "EMPTY_SCENARIO_ASSET_ASSUMPTIONS",
        severity = "ERROR",
        message = sprintf("Szenario '%s' enthält keine Asset-Renditebausteine.", scen_id),
        scope = "EXPECTATIONS"
      )
      next
    }

    for (acode in names(assets_cfg)) {
      aa <- assets_cfg[[acode]]
      r_vec <- as.numeric(aa$annual_local_returns %||% aa$local_return_assumption)
      if (length(r_vec) == 1L && !is.na(r_vec)) {
        r_vec <- rep(r_vec, horizon)
        payload$scenarios[[scen_id]]$asset_assumptions[[acode]]$annual_local_returns <- r_vec
      }
      if (length(r_vec) != horizon || any(is.na(r_vec)) || any(r_vec <= -1.0)) {
        issues[[length(issues) + 1L]] <- make_issue(
          code = "INVALID_ASSET_SCENARIO_PATH",
          severity = "ERROR",
          message = sprintf("Szenario '%s', Asset '%s': Jährliche Renditebausteine ungültig.", scen_id, acode),
          scope = "EXPECTATIONS",
          series_code = acode
        )
      }

      # Enforce GSGCTOT futures total return rule (no automatic physical gold assumption)
      if (identical(acode, "GSGCTOT")) {
        r_struct <- aa$return_structure %||% "FUTURES_TR"
        if (identical(toupper(r_struct), "PHYSICAL_GOLD") || identical(toupper(r_struct), "PHYSICAL_TR")) {
          issues[[length(issues) + 1L]] <- make_issue(
            code = "GSGCTOT_PHYSICAL_GOLD_MISMATCH",
            severity = "ERROR",
            message = "GSGCTOT ist ein Gold-Futures-Total-Return-Index (inkl. Collateral/Roll). Ein physisches Goldmodell ohne Futures-Komponenten ist unzulässig.",
            scope = "EXPECTATIONS",
            series_code = "GSGCTOT"
          )
        }
      }
    }
  }

  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))
  out <- list(
    valid = !has_errors,
    expectations_id = payload$expectations_id,
    as_of = payload$as_of,
    horizon = horizon,
    rebalancing_rule = payload$rebalancing_rule %||% "ANNUAL_REBALANCED",
    fee_basis = payload$fee_basis,
    return_definition = payload$return_definition,
    uncertainty_description = payload$uncertainty_description,
    scenarios = payload$scenarios,
    issues = issues
  )
  class(out) <- c("maps_expectations", "list")
  out
}
