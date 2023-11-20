box::use(
  scripts / constant[prewrangled_financial_data_stress_test_path]
)


companies_data <- STDataMGMT::synthetic_company_activities |> dplyr::distinct(company_id, ald_sector, ald_location)

prewrangled_financial_data_stress_test <- STDataMGMT::prepare_financial_data(
  financial_data = STDataMGMT::synthetic_eikon_data,
  companies_data = STDataMGMT::synthetic_company_activities,
  ownership_tree = NULL,
  minimum_sample_size = 1,
  minimum_ratio_sample = 0,
  allowed_range_npm = c(-Inf, Inf)
)

arrow::write_parquet(prewrangled_financial_data_stress_test, prewrangled_financial_data_stress_test_path)
