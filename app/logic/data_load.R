box::use(
  shiny[eventReactive]
)

box::use(
  app / logic / constant[backend_trajectories_data_path, backend_crispy_data_path, backend_trisk_run_data_path]
)


load_backend_crispy_data <- function() {
  if (file.exists(backend_crispy_data_path)) {
    backend_crispy_data <- arrow::read_parquet(backend_crispy_data_path) 
  } else {
    backend_crispy_data <- tibble::tibble(
      run_id = character(),
      scenario_geography = character(),
      ald_sector = character(),
      term = numeric(),
      net_present_value_baseline = numeric(),
      net_present_value_shock = numeric(),
      pd_baseline = numeric(),
      pd_shock = numeric()
    )
  }
  return(backend_crispy_data)
}

load_backend_trajectories_data <- function() {
  if (file.exists(backend_trajectories_data_path)) {
    backend_trajectories_data <- arrow::read_parquet(backend_trajectories_data_path) 
  } else {
    backend_trajectories_data <- tibble::tibble(
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
  return(backend_trajectories_data)
}

load_backend_trisk_run_data <- function(){
    if (file.exists(backend_trisk_run_data_path)) {
    backend_trisk_run_data <- arrow::read_parquet(backend_trisk_run_data_path) 
  } else {
    backend_trisk_run_data <- tibble::tibble(
      run_id = character(),
      roll_up_type = character(),
      baseline_scenario = character(),
      shock_scenario = character(),
      risk_free_rate = numeric(),
      discount_rate = numeric(),
      dividend_rate = numeric(),
      growth_rate = numeric(),
      shock_year = numeric(),
    )
  }
  return(backend_trisk_run_data)

}


get_run_id_from_params_selection <- function(backend_trisk_run_data, trisk_params_selection){
    run_id_r <- eventReactive(
      c(
        trisk_params_selection$discount_rate,
        trisk_params_selection$risk_free_rate,
        trisk_params_selection$growth_rate,
        trisk_params_selection$shock_year,
        trisk_params_selection$baseline_scenario,
        trisk_params_selection$shock_scenario,
        trisk_params_selection$scenario_geography
      ),
      ignoreInit = TRUE,
      {
        backend_trisk_run_data |>
          dplyr::filter(
            .data$discount_rate == trisk_params_selection$discount_rate,
            .data$risk_free_rate == trisk_params_selection$risk_free_rate,
            .data$growth_rate == trisk_params_selection$growth_rate,
            .data$shock_year == trisk_params_selection$shock_year,
            .data$baseline_scenario == trisk_params_selection$baseline_scenario,
            .data$shock_scenario == trisk_params_selection$shock_scenario,
            .data$scenario_geography == trisk_params_selection$scenario_geography
          ) |>
          dplyr::pull(run_id)
      }
    )
    return(run_id_r)
}
