source(file.path("scripts", "trisk_knative", "trisk_compute.R"))


trisk_run_params <- list(
  baseline_scenario = "NGFS2023GCAM_CP", shock_scenario = "NGFS2023GCAM_B2DS",
  scenario_geography = "Global", shock_year = 2025,
  discount_rate = 0.02, risk_free_rate = 0, growth_rate = 0.01,
  div_netprofit_prop_coef = 0.8, carbon_price_model = "no_carbon_tax",
  market_passthrough = 0
)

st_results_wrangled_and_checked <- run_trisk_with_params(
  trisk_run_params = trisk_run_params,
  trisk_input_path = file.path("scripts", "trisk_knative", "st_inputs")
)
