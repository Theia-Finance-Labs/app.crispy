# Function to run trisk analysis and return reactive values
run_trisk_analysis <- function(assets_data, scenarios_data, financial_data, carbon_data, trisk_run_params, selected_country) {
  tryCatch(
    {
      # Run trisk analysis
      st_results <- trisk.analysis::run_trisk_sa(
        assets_data = assets_data,
        scenarios_data = scenarios_data,
        financial_data = financial_data,
        carbon_data = carbon_data,
        run_params = list(trisk_run_params),
        country_iso2 = selected_country
      )

      # Return results as a list
      list(
        params = st_results$params |>
          dplyr::mutate(country_iso2 = selected_country),
        trajectories = st_results$trajectories,
        npv_results = st_results$npv_results,
        pd_results = st_results$pd_results
      )
    },
    error = function(e) {
      # Handle the error gracefully (log, show message, etc.)
      shiny::showNotification("Trisk run failed. No data added.", type = "error")
      NULL
    }
  )
}
