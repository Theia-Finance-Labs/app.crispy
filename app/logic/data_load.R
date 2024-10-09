download_db_tables_postgres <- function(conn, tables, save_dir, dbname, host, port, user, password) {
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = dbname,
    host = host,
    port = port,
    user = user,
    password = password,
    sslmode = "require"
  )
  # Ensure the directory exists
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }

  for (table_name in tables) {
    query <- sprintf("SELECT * FROM public.\"%s\"", table_name)
    data <- DBI::dbGetQuery(conn, query)
    file_path <- file.path(save_dir, paste0(table_name, ".csv"))
    readr::write_csv(data, file = file_path)
  }
}

get_possible_trisk_combinations <- function(scenarios_data) {
  scenarios_data <- scenarios_data |>
    dplyr::mutate(prefix = stringr::str_extract(scenario, "^[^_]+"))

  # Baseline dataframe
  scenarios_data_baseline <- scenarios_data |>
    dplyr::filter(.data$scenario_type == "baseline") |>
    dplyr::select(.data$prefix, .data$scenario_geography, .data$scenario) |>
    dplyr::rename(baseline_scenario = .data$scenario) |>
    dplyr::distinct_all()

  # Target dataframe
  scenarios_data_target <- scenarios_data |>
    dplyr::filter(.data$scenario_type == "target") |>
    dplyr::select(.data$prefix, .data$scenario_geography, .data$scenario) |>
    dplyr::rename(target_scenario = .data$scenario) |>
    dplyr::distinct_all()

  # Merging the two dataframes
  merged_scenarios_data <- scenarios_data_baseline |>
    dplyr::inner_join(scenarios_data_target, by = c("prefix", "scenario_geography"))

  # Extract unique values
  possible_trisk_combinations <- merged_scenarios_data |>
    dplyr::distinct(.data$scenario_geography, .data$baseline_scenario, .data$target_scenario)



  BENCH_REGIONS <- readr::read_csv(file.path("app", "data", "bench_regions_renamed.csv"))
  available_geographies <- BENCH_REGIONS|> dplyr::distinct(.data$scenario_geography) |> dplyr::pull()
  possible_trisk_combinations <- possible_trisk_combinations |>
    dplyr::filter(.data$scenario_geography %in% c("Global", available_geographies))  

  return(possible_trisk_combinations)
}
