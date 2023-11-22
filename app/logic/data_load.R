box::use(
  app / logic / constant[scenario_data_path, use_ald_sector, backend_crispy_data_path]
)


load_backend_crispy_data <- function() {
  if (file.exists(backend_crispy_data_path)) {
    backend_crispy_data <- arrow::read_parquet(backend_crispy_data_path) |>
      dplyr::filter(.data$term == 5) |>
      dplyr::filter(.data$ald_sector %in% use_ald_sector)
  } else {
    backend_crispy_data <- tibble::tibble(
      run_id = character(),
      scenario_geography = character(),
      ald_sector = character(),
      term = numeric(),
      roll_up_type = character(),
      baseline_scenario = character(),
      shock_scenario = character(),
      risk_free_rate = numeric(),
      discount_rate = numeric(),
      dividend_rate = numeric(),
      growth_rate = numeric(),
      shock_year = numeric(),
      net_present_value_baseline = numeric(),
      net_present_value_shock = numeric(),
      pd_baseline = numeric(),
      pd_shock = numeric()
    )
  }
  return(backend_crispy_data)
}

load_scenario_data <- function() {
  if (file.exists(scenario_data_path)) {
    scenario_data <- readr::read_csv(scenario_data_path) |>
      dplyr::filter(.data$ald_sector %in% use_ald_sector) |>
      dplyr::group_by(scenario_geography, scenario, ald_sector, units, year) |>
      dplyr::summarise(fair_share_perc = sum(fair_share_perc), .groups = "drop")
  } else {
    tibble::tibble(
      scenario_geography = character(),
      scenario = character(),
      ald_sector = character(),
      units = character(),
      ald_business_unit = character(),
      year = numeric(),
      direction = character(),
      fair_share_perc = numeric()
    )
  }
  return(scenario_data)
}
