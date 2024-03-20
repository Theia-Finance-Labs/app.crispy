# Define an endpoint that accepts POST requests
# Assume the JSON payload is directly analogous to the R list structure for trisk_run_param

source("./trisk_compute.R")
source("./utils.R")

# Create a plumber router
pr <- plumber::Plumber$new()

# hardcoded input fp while the data is still part of the docker image
trisk_input_path <- file.path(".", "st_inputs")
s3_folder_path <- "st_inputs/"



if (!dir.exists(trisk_input_path)) {
  download_files_from_s3(
    s3_url = Sys.getenv("S3_URL"),
    s3_folder_path = s3_folder_path,
    local_folder_path = trisk_input_path,
    s3_access_key = Sys.getenv("S3_ACCESS_KEY"),
    s3_secret_key = Sys.getenv("S3_SECRET_KEY"),
    s3_bucket = Sys.getenv("S3_BUCKET"),
    s3_region = Sys.getenv("S3_REGION")
  )
}


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
    user = Sys.getenv("ST_POSTGRES_USERNAME"),
    password = Sys.getenv("POSTGRES_PASSWORD")
  )

  run_id <- run_trisk_and_upload_results_to_db_conn(
    trisk_run_params = trisk_run_params,
    trisk_input_path = trisk_input_path,
    postgres_conn = postgres_conn
  )

  print("TRISK run & upload complete")

  response <- list(trisk_run_id = run_id)
  response <- jsonlite::toJSON(response, auto_unbox = TRUE)
  return(response)
})

pr$handle("GET", "/get_possible_trisk_combinations", function(req, res) {
  possible_trisk_combinations <- r2dii.climate.stress.test::get_scenario_geography_x_ald_sector(trisk_input_path)
  response <- list(possible_trisk_combinations = possible_trisk_combinations)
  response <- jsonlite::toJSON(response, auto_unbox = TRUE)
  return(response)
})

# Run the plumber API on port 8080
pr$run(port = 8080, host = "0.0.0.0")
