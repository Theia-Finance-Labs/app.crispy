# Define an endpoint that accepts POST requests
# Assume the JSON payload is directly analogous to the R list structure for trisk_run_param

source(file.path(".","trisk_compute.R"))
source(file.path(".","utils.R"))

# Create a plumber router
pr <- plumber::Plumber$new()

# hardcoded input fp while the data is still part of the docker image
TRISK_INPUT_PATH <- file.path(".", "st_inputs")
tables <- c(
  "Scenarios_AnalysisInput",
   "abcd_stress_test_input", 
   "ngfs_carbon_price",
   "prewrangled_capacity_factors",
    "prewrangled_financial_data_stress_test", 
    "price_data_long"
    )

download_db_tables_postgres(tables=tables, folder_path=TRISK_INPUT_PATH)


validate_trisk_run_params <- function(trisk_run_params) {
  required_keys <- names(formals(r2dii.climate.stress.test::run_trisk))
  param_keys <- names(trisk_run_params)

  if (!all(names(param_keys) %in% required_keys)) {
    stop("trisk_run_params does not contain the correct keys")
  }
}

pr$handle("POST", "/compute_trisk", function(req, res) {
  trisk_run_params <- jsonlite::fromJSON(req$postBody)$trisk_run_params
  validate_trisk_run_params(trisk_run_params)

  postgres_conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("POSTGRES_DB"),
    host = Sys.getenv("POSTGRES_HOST"),
    port = Sys.getenv("POSTGRES_PORT"),
    user = Sys.getenv("POSTGRES_USERNAME"),
    password = Sys.getenv("POSTGRES_PASSWORD"),
    sslmode="require"
  )

  run_id <- run_trisk_and_upload_results_to_db_conn(
    trisk_run_params = trisk_run_params,
    trisk_input_path = TRISK_INPUT_PATH,
    postgres_conn = postgres_conn
  )

  print("TRISK run & upload complete")

  response <- list(trisk_run_id = run_id)
  response <- jsonlite::toJSON(response, auto_unbox = TRUE)
  return(response)
})

pr$handle("GET", "/get_possible_trisk_combinations", function(req, res) {
  possible_trisk_combinations <- r2dii.climate.stress.test::get_scenario_geography_x_ald_sector(TRISK_INPUT_PATH)
  response <- list(possible_trisk_combinations = possible_trisk_combinations)
  response <- jsonlite::toJSON(response, auto_unbox = TRUE)
  return(response)
})

# Run the plumber API on port 8080
pr$run(port = 8080, host = "0.0.0.0")
