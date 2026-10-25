# ==============================================================================
# data/demo/build_demo_data.R
# Generates deterministic analytical golden demo workbook and verifies R engine
# ==============================================================================

for (f in c("app_config.R", "data_import.R", "metadata.R", "fx_quotes.R",
            "return_transforms.R", "cash_returns.R", "risk_basis.R",
            "risk_diagnostics.R", "missing_history.R", "covariance_estimators.R",
            "exposures.R", "portfolio_metrics.R", "expectations_adapter.R",
            "scenario_paths.R", "validation.R", "exports.R",
            "cache_jobs.R", "session_state.R", "server_commands.R")) {
  source(file.path("R", f), local = FALSE)
}

dir.create("data/demo", recursive = TRUE, showWarnings = FALSE)

# Verify full pipeline on Data.xlsx
cfg <- load_app_config()
shared_cache <- create_cache_manager(max_size_mb = 256)
st <- create_session_state(cfg, shared_cache = shared_cache)
ld <- load_dataset_into_state(st, "Data.xlsx", is_private = FALSE)
stopifnot(isTRUE(ld$valid))

for (b_ccy in c("CHF", "EUR", "USD")) {
  req <- list(
    protocol_version = 1L,
    request_id = paste0("boot_", b_ccy),
    type = "app.bootstrap",
    context = list(base_currency = b_ccy),
    payload = list(base_currency = b_ccy, model_id = "shrinkage_long", portfolio_id = "saa_50")
  )
  resp <- handle_app_command(req, st)
  stopifnot(identical(resp$status, "ok"))
  cat(sprintf(
    "[OK] Base=%s | Model=%s | N=%d (%s..%s) | Vol_Hedged=%.2f%% | Vol_Unhedged=%.2f%% | CAGR_5Y=%.2f%%\n",
    b_ccy,
    resp$result$covariance_model$model_id,
    resp$result$covariance_model$n_obs,
    resp$result$covariance_model$start_month,
    resp$result$covariance_model$end_month,
    100 * resp$result$portfolio_evaluation$metrics$vol_annualized,
    100 * resp$result$portfolio_evaluation$metrics$vol_unhedged_annualized,
    100 * resp$result$scenario_evaluation$metrics$scenario_cagr_hedged_5y
  ))
}
