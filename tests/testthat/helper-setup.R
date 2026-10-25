# ==============================================================================
# tests/testthat/helper-setup.R
# Loads pure R engine and adapter modules for testthat suite
# ==============================================================================

proj_root <- normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(proj_root, "Data.xlsx")) && file.exists("Data.xlsx")) {
  proj_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

r_files <- c(
  "app_config.R",
  "data_import.R",
  "metadata.R",
  "fx_quotes.R",
  "return_transforms.R",
  "cash_returns.R",
  "risk_basis.R",
  "risk_diagnostics.R",
  "missing_history.R",
  "covariance_estimators.R",
  "exposures.R",
  "portfolio_metrics.R",
  "expectations_adapter.R",
  "scenario_paths.R",
  "validation.R",
  "exports.R",
  "cache_jobs.R",
  "session_state.R",
  "server_commands.R"
)

for (rf in r_files) {
  source(file.path(proj_root, "R", rf), local = FALSE)
}

TEST_CONFIG <- load_app_config(
  config_path = file.path(proj_root, "config", "defaults.json"),
  schema_path = file.path(proj_root, "config", "metadata-schema.json")
)
TEST_DATA_PATH <- file.path(proj_root, "Data.xlsx")
