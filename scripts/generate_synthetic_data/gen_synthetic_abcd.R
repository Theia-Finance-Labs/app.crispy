box::use(
  scripts / constant[abcd_stress_test_input_path]
)


# ===================================
# ABCD STRESS TEST INPUT
# ===================================

start_year <- 2020
time_horizon <- 10
additional_year <- NULL
sector_list <- c("Automotive", "Power", "Oil&Gas", "Coal")
km_per_vehicle <- 15000

abcd_stress_test_input <-
  STDataMGMT::prepare_abcd_data(
    company_activities = STDataMGMT::synthetic_company_activities,
    company_emissions = STDataMGMT::synthetic_company_emissions,
    scenarios_geographies = STDataMGMT::scenarios_geographies,
    start_year = start_year,
    time_horizon = time_horizon,
    additional_year = additional_year,
    km_per_vehicle = km_per_vehicle,
    sector_list = sector_list
  )

arrow::write_parquet(abcd_stress_test_input, abcd_stress_test_input_path)
