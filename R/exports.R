# ==============================================================================
# R/exports.R
# Authoritative analysis bundle exporter (XLSX, JSON, CSV, RDS) with
# cryptographic configuration & dataset hashes
# Pure R module - no Shiny dependencies
# ==============================================================================

build_export_manifest <- function(dataset,
                                  model,
                                  portfolio_eval,
                                  scenario_eval = NULL,
                                  comparison_eval = NULL,
                                  state_version = 1L) {
  bundle_hash <- compute_object_hash(list(
    dataset_hash = dataset$data_hash,
    model_hash = model$model_hash,
    portfolio_config_hash = portfolio_eval$config_hash,
    scenario_id = if (!is.null(scenario_eval)) scenario_eval$scenario_id else NULL
  ))

  list(
    bundle_hash = substr(bundle_hash, 1L, 16L),
    full_bundle_sha256 = bundle_hash,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    engine_version = "1.0.0",
    state_version = as.integer(state_version),
    dataset_id = dataset$dataset_id,
    dataset_sha256 = dataset$data_hash,
    file_sha256 = dataset$file_hash,
    base_currency = model$base_currency,
    return_mode = model$return_mode,
    risk_model_id = model$model_id,
    risk_model_label = model$label,
    risk_model_hash = model$model_hash,
    estimator = model$estimator,
    target = model$target,
    shrinkage_intensity = model$shrinkage_intensity,
    sample_start = model$start_month,
    sample_end = model$end_month,
    sample_obs = model$n_obs,
    portfolio_id = portfolio_eval$portfolio_id,
    portfolio_name = portfolio_eval$portfolio_name,
    portfolio_config_hash = portfolio_eval$config_hash,
    hedge_mode = portfolio_eval$exposures$active_mode,
    vol_annualized = portfolio_eval$metrics$vol_annualized,
    vol_unhedged_annualized = portfolio_eval$metrics$vol_unhedged_annualized,
    scenario_id = if (!is.null(scenario_eval)) scenario_eval$scenario_id else NA_character_,
    scenario_cagr_5y = if (!is.null(scenario_eval) && isTRUE(scenario_eval$valid)) scenario_eval$metrics$scenario_cagr_hedged_5y else NA_real_
  )
}

export_analysis_bundle <- function(dataset,
                                   model,
                                   portfolio_eval,
                                   scenario_eval = NULL,
                                   comparison_eval = NULL,
                                   format = c("XLSX", "JSON", "CSV", "RDS"),
                                   output_path = NULL,
                                   state_version = 1L) {
  format <- match.arg(toupper(format), c("XLSX", "JSON", "CSV", "RDS"))

  manifest <- build_export_manifest(
    dataset = dataset,
    model = model,
    portfolio_eval = portfolio_eval,
    scenario_eval = scenario_eval,
    comparison_eval = comparison_eval,
    state_version = state_version
  )

  if (is.null(output_path)) {
    ext <- tolower(format)
    output_path <- tempfile(pattern = paste0("maps_export_", manifest$portfolio_config_hash, "_"), fileext = paste0(".", ext))
  }

  manifest_df <- data.frame(
    Field = names(manifest),
    Value = vapply(manifest, function(x) if (is.null(x) || length(x) == 0L) "" else as.character(x[1L]), character(1)),
    stringsAsFactors = FALSE
  )

  cov_df <- as.data.frame(model$Sigma_annualized, check.names = FALSE)
  cov_df <- cbind(Component = rownames(cov_df), cov_df)
  corr_df <- as.data.frame(model$correlation_matrix, check.names = FALSE)
  corr_df <- cbind(Component = rownames(corr_df), corr_df)

  issues_all <- c(dataset$issues, model$issues, portfolio_eval$issues)
  issues_df <- if (length(issues_all) > 0L) {
    data.frame(
      Code = vapply(issues_all, function(x) x$code %||% "", character(1)),
      Severity = vapply(issues_all, function(x) x$severity %||% "", character(1)),
      Scope = vapply(issues_all, function(x) x$scope %||% "", character(1)),
      Series = vapply(issues_all, function(x) x$series_code %||% "", character(1)),
      Message = vapply(issues_all, function(x) x$message %||% "", character(1)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(Code = character(0), Severity = character(0), Scope = character(0), Series = character(0), Message = character(0))
  }

  sheets_list <- list(
    Manifest = manifest_df,
    Currency_Exposures = as.data.frame(portfolio_eval$exposures$currency_table),
    Asset_Risk_Decomp = as.data.frame(portfolio_eval$asset_contributions),
    Component_Risk_Decomp = as.data.frame(portfolio_eval$component_contributions),
    Class_Summary = as.data.frame(portfolio_eval$class_contributions),
    Covariance_Ann = cov_df,
    Correlation = corr_df,
    Metadata_Coverage = as.data.frame(dataset$meta),
    Audit_Warnings = issues_df
  )

  if (!is.null(scenario_eval) && isTRUE(scenario_eval$valid)) {
    sheets_list[["Scenario_Path_5Y"]] <- as.data.frame(scenario_eval$yearly_path)
    sheets_list[["Scenario_Assets_5Y"]] <- as.data.frame(scenario_eval$asset_table)
  }
  if (!is.null(comparison_eval) && isTRUE(comparison_eval$valid)) {
    sheets_list[["SAA_Comparison"]] <- as.data.frame(comparison_eval$summary_table)
  }

  if (identical(format, "XLSX")) {
    if (requireNamespace("writexl", quietly = TRUE)) {
      writexl::write_xlsx(sheets_list, path = output_path)
    } else if (requireNamespace("openxlsx", quietly = TRUE)) {
      openxlsx::write.xlsx(sheets_list, file = output_path, overwrite = TRUE)
    } else {
      stop("Neither writexl nor openxlsx is installed for XLSX export.", call. = FALSE)
    }
  } else if (identical(format, "JSON")) {
    json_payload <- sanitize_for_json(list(
      manifest = manifest,
      exposures = portfolio_eval$exposures$currency_table,
      asset_contributions = portfolio_eval$asset_contributions,
      component_contributions = portfolio_eval$component_contributions,
      covariance_annualized = list(
        component_ids = model$component_ids,
        matrix = model$Sigma_annualized
      ),
      correlation_matrix = list(
        component_ids = model$component_ids,
        matrix = model$correlation_matrix
      ),
      scenario_evaluation = if (!is.null(scenario_eval) && isTRUE(scenario_eval$valid)) {
        list(
          metrics = scenario_eval$metrics,
          yearly_path = scenario_eval$yearly_path,
          asset_table = scenario_eval$asset_table
        )
      } else NULL,
      comparison = if (!is.null(comparison_eval) && isTRUE(comparison_eval$valid)) {
        comparison_eval$summary_table
      } else NULL,
      issues = issues_df
    ))
    writeLines(jsonlite::toJSON(json_payload, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 10), con = output_path, useBytes = TRUE)
  } else if (identical(format, "CSV")) {
    utils::write.csv(sheets_list$Asset_Risk_Decomp, file = output_path, row.names = FALSE, fileEncoding = "UTF-8")
  } else if (identical(format, "RDS")) {
    saveRDS(list(manifest = manifest, sheets = sheets_list), file = output_path)
  }

  list(
    valid = file.exists(output_path),
    format = format,
    output_path = output_path,
    manifest = manifest
  )
}
