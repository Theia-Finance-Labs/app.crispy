
#' Title
#'
#' @param company_trajectories_data company_trajectories_data
#' @param max_trajectories_granularity max_trajectories_granularity
#'
#' @export
#'
main_data_load_trajectories_data <- function(
    company_trajectories_data,
    granularity=c("company_id", "company_name", "sector", "technology"),
    param_cols = c(
      "run_id", "year","scenario_geography", "baseline_scenario",
      "target_scenario", "risk_free_rate", "discount_rate", "div_netprofit_prop_coef",
      "carbon_price_model", "market_passthrough", 
      "growth_rate", "shock_year")
      ) {
  group_cols <- unique(c(granularity, param_cols))

  company_trajectories_data <- company_trajectories_data |>
    aggregate_trajectories_facts(group_cols = group_cols)  
  return(company_trajectories_data)
}


load_multiple_trajectories <- function(crispy_outputs_dir) {
  # Required Libraries

  # Get file paths
  files_path <- list.files(
    path = crispy_outputs_dir,
    pattern = "^company_trajectories_(.*).csv",
    recursive = TRUE,
    full.names = TRUE
  )

  stopifnot(length(files_path) > 0)

  # Load all files into a list and add a run_id column for each dataframe
  data_list <- purrr::map(files_path, function(fp) {
    df <- readr::read_csv(fp,
      col_types = readr::cols_only(
        run_id = "c",
        company_id = "c",
        company_name = "c",
        sector = "c",
        technology = "c",
        year = "d",
        phase_out = "d",
        baseline_scenario = "c",
        target_scenario = "c",
        scenario_geography = "c",
        risk_free_rate = "d",
        discount_rate = "d",
        div_netprofit_prop_coef = "d",
        growth_rate = "d",
        shock_year = "d",
        carbon_price_model = "c",
        market_passthrough = "d",
        production_plan_company_technology = "d",
        production_baseline_scenario = "d",
        production_target_scenario = "d",
        production_target_scenario = "d",
        price_baseline_scenario = "d",
        price_target_scenario = "d",
        net_profits_baseline_scenario = "d",
        net_profits_target_scenario = "d",
        discounted_net_profits_baseline_scenario = "d",
        discounted_net_profits_target_scenario = "d"
      )
    ) |>
    dplyr::rename(
      baseline_scenario = baseline_scenario,
      target_scenario = target_scenario,
      scenario_geography = scenario_geography,
      risk_free_rate=risk_free_rate,
      discount_rate=discount_rate,
      div_netprofit_prop_coef=div_netprofit_prop_coef,
      growth_rate=growth_rate,
      shock_year=shock_year,
      carbon_price_model=carbon_price_model,
      market_passthrough=market_passthrough
    ) |>
    dplyr::filter(.data$year < max(.data$year)) # removes last year that is NA
  })

  multi_trajectories_data <- dplyr::bind_rows(data_list)

  return(multi_trajectories_data)
}

#' Aggregate numerical trajectories columns
#'
#' @param multi_trajectories dataframe of trajectories from 1 or multiple trisk truns
#' @param group_cols group_cols
#'
#' @export
#'
aggregate_trajectories_facts <- function(multi_trajectories, group_cols) {
    multi_trajectories <- multi_trajectories |>
      dplyr::group_by_at(group_cols) |>
      dplyr::summarise(
        production_baseline_scenario = sum(.data$production_baseline_scenario, na.rm = TRUE),
        production_target_scenario = sum(.data$production_target_scenario, na.rm = TRUE),
        production_shock_scenario = sum(.data$production_shock_scenario, na.rm = TRUE),
        # price_baseline_scenario= mean(.data$price_baseline_scenario, na.rm = TRUE),
        # price_target_scenario = mean(.data$price_target_scenario, na.rm = TRUE),
        # net_profits_baseline_scenario = sum(.data$net_profits_baseline_scenario, na.rm = TRUE),
        # net_profits_target_scenario = sum(.data$net_profits_target_scenario, na.rm = TRUE),
        # discounted_net_profits_baseline_scenario = sum(.data$discounted_net_profits_baseline_scenario, na.rm = TRUE),
        # discounted_net_profits_target_scenario = sum(.data$discounted_net_profits_target_scenario, na.rm = TRUE),
        .groups = "drop"
      )
    return(multi_trajectories)
  }

