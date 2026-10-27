# ==============================================================================
# R/app_config.R
# Configuration loader, structured diagnostics, and deterministic hashing
# Pure R module - no Shiny dependencies
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

load_app_config <- function(config_path = "config/defaults.json",
                            schema_path = "config/metadata-schema.json") {
  if (!file.exists(config_path)) {
    stop(sprintf("Configuration file not found: %s", config_path), call. = FALSE)
  }
  cfg <- jsonlite::read_json(config_path, simplifyVector = TRUE)
  if (file.exists(schema_path)) {
    cfg$metadata_schema <- jsonlite::read_json(schema_path, simplifyVector = TRUE)
  } else {
    cfg$metadata_schema <- list(
      required_columns = c(
        "Code", "Name", "Series_Type", "Value_Unit", "Active", "Review_Status",
        "Asset_Class", "Sub_Asset_Class", "Region", "Return_Currency", "FX_Exposure",
        "Hedge_Allowed", "Default_Hedge_Ratio", "TR_Convention", "Already_Hedged",
        "Existing_Hedge_Currency", "Return_Structure", "Foreign_Currency",
        "Quote_Currency", "Quote_Direction", "Quote_Scale", "Forward_Type",
        "Points_Multiplier", "Tenor", "Cash_Currency", "Rate_Convention",
        "Day_Count", "Spot_Reference_Code", "Source", "Notes"
      ),
      optional_versioned_columns = c("Proxy_Type", "Proxy_Until", "Proxy_Description", "Preferred_Donors", "Is_Donor")
    )
  }
  class(cfg) <- c("maps_config", "list")
  cfg
}

make_issue <- function(code,
                       severity = c("ERROR", "WARNING", "INFO"),
                       message,
                       scope = "GENERAL",
                       series_code = NULL,
                       details = NULL) {
  severity <- match.arg(severity)
  list(
    code = as.character(code),
    severity = severity,
    message = as.character(message),
    scope = as.character(scope),
    series_code = if (is.null(series_code) || is.na(series_code)) NULL else as.character(series_code),
    details = details
  )
}

split_issues <- function(issues) {
  if (length(issues) == 0L) {
    return(list(errors = list(), warnings = list(), info = list()))
  }
  sev <- vapply(issues, function(x) x$severity %||% "INFO", character(1))
  list(
    errors = unname(issues[sev == "ERROR"]),
    warnings = unname(issues[sev == "WARNING"]),
    info = unname(issues[sev == "INFO"])
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) y else x
}

compute_file_hash <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Cannot hash missing file: %s", path), call. = FALSE)
  }
  unname(digest::digest(file = path, algo = "sha256"))
}

compute_object_hash <- function(obj) {
  unname(digest::digest(obj, algo = "sha256"))
}

sanitize_for_json <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d"))
  }
  if (is.numeric(x)) {
    bad <- is.nan(x) | is.infinite(x)
    if (any(bad)) x[bad] <- NA_real_
    return(x)
  }
  if (is.data.frame(x)) {
    out <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
    if (nrow(out) == 0L) return(list())
    for (nm in names(out)) {
      out[[nm]] <- sanitize_for_json(out[[nm]])
    }
    return(unname(do.call(Map, c(list(f = function(...) list(...)), as.list(out)))))
  }
  if (is.list(x)) {
    return(lapply(x, sanitize_for_json))
  }
  x
}
