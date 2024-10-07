box::use(
  shiny[
    moduleServer, NS, observe, div, tags, reactiveVal, reactiveValues, eventReactive, p, tagList, observeEvent, img,
    HTML, conditionalPanel, reactive
  ],
  shiny.semantic[slider_input, dropdown_input, segment, update_dropdown_input, update_slider],
  shinyjs[useShinyjs],
  semantic.dashboard[dashboardSidebar]
)

box::use(
  app/view/modules/params_scenarios,
  app/view/modules/params_dimensions,
  app/view/modules/params_trisk,
  app/view/modules/trisk_button,
  app/view/modules/portfolio_upload,
  app/logic/renamings[rename_string_vector]
)


####### UI
ui <- function(id, max_trisk_granularity, available_vars) {
  ns <- NS(id)
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(HTML(paste0("
        .sidebar-section {
          padding: 20px;
          background-color: #f9f9f9;
          margin: 15px 0;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .sidebar-section .ui.header {
          font-size: 18px;
          color: #333;
          margin-bottom: 15px;
        }
        .ui.button {
          background-color: #d4d4d5;
          color: white;
          border: none;
          border-radius: 4px;
          margin: 8px 0;
          display: block;
          width: 100%; /* Ensures full width */
        }
        .ui.buttons {
          width: 50%; /* Ensures full width */
        }
        .ui.divider {
          margin: 20px 0;
        }
        .ui.dropdown {
          width: 100%;
          margin-bottom: 10px;
        }
      ")))
    ),
    # Data Section
    div(
      class = "sidebar-section",
      shiny::tags$div(class = "ui header", "Data"),
      shiny::tags$div(class = "ui divider"),
      # Run TRISK button
      trisk_button$ui(ns("trisk_button")),
      # Dimensions
      params_dimensions$ui(ns("params_dimensions"), max_trisk_granularity),
      #, portfolio_upload$ui(ns("portfolio_upload"))
      # Download button
        tags$a(
          id = ns("download_scenario_data"),
          class = "ui fluid button",
          href = "https://scenarios-repository.fra1.cdn.digitaloceanspaces.com/scenario_repository.zip",
          target = "_blank",  # Opens the link in a new tab or window
          "Download Scenario Data")
  ),
    # Scenario Choice Section
    div(
      class = "sidebar-section",
      shinyjs::useShinyjs(),
      shiny::tags$div(class = "ui header", "Scenario Choice"),
      shiny::tags$div(class = "ui divider"),
      # Scenario Choice
      params_scenarios$ui(ns("params_scenarios"))
    ),
    # TRISK Parameters Section
    div(
      class = "sidebar-section",
      shiny::tags$div(class = "ui header", "TRISK Parameters"),
      shiny::tags$div(class = "ui divider"),
      params_trisk$ui(ns("params_trisk"), available_vars)
    )
  )
}


####### Server



server <- function(id, 
                  assets_data,
                  scenarios_data,
                  financial_data,
                  carbon_data,
                   possible_trisk_combinations,
                   available_vars,
                   hide_vars,
                   max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    # Update UI elements =========================

    # Collect UI elements (and compute trisks if necessary) =========================
    trisk_granularity_r <- params_dimensions$server(
      "params_dimensions",
      max_trisk_granularity = max_trisk_granularity
    )


    scenario_config_r <- params_scenarios$server(
      "params_scenarios",
      hide_vars = hide_vars,
      possible_trisk_combinations = possible_trisk_combinations
    )

    trisk_config_r <- params_trisk$server("params_trisk", available_vars)


    # reactive variable containing trisk run parameters
    trisk_run_params_r <- shiny::reactive({
      reactiveValues(
        baseline_scenario = scenario_config_r()$baseline_scenario,
        shock_scenario = scenario_config_r()$shock_scenario,
        scenario_geography = scenario_config_r()$scenario_geography,
        shock_year = trisk_config_r()$shock_year,
        discount_rate = trisk_config_r()$discount_rate,
        risk_free_rate = trisk_config_r()$risk_free_rate,
        growth_rate = trisk_config_r()$growth_rate,
        div_netprofit_prop_coef = trisk_config_r()$div_netprofit_prop_coef,
        carbon_price_model = trisk_config_r()$carbon_price_model,
        market_passthrough = trisk_config_r()$market_passthrough
      )
    })

    reactive_trisk_results <- trisk_button$server(
      "trisk_button",
      assets_data=assets_data,
      scenarios_data=scenarios_data,
      financial_data=financial_data,
      carbon_data=carbon_data,
      trisk_run_params_r = trisk_run_params_r
    )
# Helper function to return an empty dataframe with specified columns and datatypes
empty_dataframe <- function(column_info) {
  # Create an empty dataframe with specific column types
  data <- lapply(column_info, function(dtype) {
    if (dtype == "numeric") {
      return(numeric(0))
    } else if (dtype == "character") {
      return(character(0))
    } else if (dtype == "integer") {
      return(integer(0))
    } else if (dtype == "logical") {
      return(logical(0))
    } else {
      return(NA)  # Default if unknown type
    }
  })
  return(as.data.frame(data, stringsAsFactors = FALSE))
}

# Define column information for crispy data
crispy_column_info <- c(
  run_id = "character", 
  company_id = "character", 
  asset_id = "character", 
  company_name = "character", 
  asset_name = "character", 
  sector = "character", 
  technology = "character", 
  net_present_value_baseline = "numeric", 
  net_present_value_shock = "numeric", 
  pd_shock = "numeric", 
  pd_baseline = "numeric", 
  baseline_scenario = "character", 
  target_scenario = "character", 
  scenario_geography = "character", 
  carbon_price_model = "character", 
  risk_free_rate = "numeric", 
  discount_rate = "numeric", 
  growth_rate = "numeric", 
  div_netprofit_prop_coef = "numeric", 
  shock_year = "integer", 
  market_passthrough = "numeric"
)

# Define column information for trajectories data
trajectories_column_info <- c(
  run_id = "character", 
  asset_id = "character", 
  asset_name = "character", 
  company_id = "character", 
  company_name = "character", 
  year = "integer", 
  sector = "character", 
  technology = "character", 
  production_plan_company_technology = "numeric", 
  production_baseline_scenario = "numeric", 
  production_target_scenario = "numeric", 
  production_shock_scenario = "numeric", 
  pd = "numeric", 
  net_profit_margin = "numeric", 
  debt_equity_ratio = "numeric", 
  volatility = "numeric", 
  scenario_price_baseline = "numeric", 
  price_shock_scenario = "numeric", 
  net_profits_baseline_scenario = "numeric", 
  net_profits_shock_scenario = "numeric", 
  discounted_net_profits_baseline_scenario = "numeric", 
  discounted_net_profits_shock_scenario = "numeric", 
  baseline_scenario = "character", 
  target_scenario = "character", 
  scenario_geography = "character", 
  carbon_price_model = "character", 
  risk_free_rate = "numeric", 
  discount_rate = "numeric", 
  growth_rate = "numeric", 
  div_netprofit_prop_coef = "numeric", 
  shock_year = "integer", 
  market_passthrough = "numeric"
)

# Reactive for crispy data
crispy_data_r <- reactive({
  # Check if reactive values are NULL before trying to call them
  if (is.null(reactive_trisk_results$npv_results) || 
      is.null(reactive_trisk_results$pd_results) || 
      is.null(reactive_trisk_results$params)) {
    # Return an empty dataframe with appropriate datatypes
    return(empty_dataframe(crispy_column_info))
  }

  # Fetch the actual data by calling the reactive functions
  npv_results <- reactive_trisk_results$npv_results()
  pd_results <- reactive_trisk_results$pd_results()
  params <- reactive_trisk_results$params()

  # If still any of the data is NULL after fetching, return an empty dataframe
  if (is.null(npv_results) || is.null(pd_results) || is.null(params)) {
    return(empty_dataframe(crispy_column_info))
  }

  # Proceed with normal operations if all data is available
  npv_results |>
    dplyr::inner_join(pd_results) |>
    dplyr::inner_join(params) |>
    main_load_multi_crispy_data(
      granularity = trisk_granularity_r(),
      filter_outliers = FALSE
    )
})

# Reactive for trajectories data
trajectories_data_r <- reactive({
  # Check if reactive values are NULL before trying to call them
  if (is.null(reactive_trisk_results$trajectories) || 
      is.null(reactive_trisk_results$params)) {
    # Return an empty dataframe with appropriate datatypes
    return(empty_dataframe(trajectories_column_info))
  }

  # Fetch the actual data by calling the reactive functions
  trajectories <- reactive_trisk_results$trajectories()
  params <- reactive_trisk_results$params()

  # If still any of the data is NULL after fetching, return an empty dataframe
  if (is.null(trajectories) || is.null(params)) {
    return(empty_dataframe(trajectories_column_info))
  }

  # Proceed with normal operations if all data is available
  trajectories |>
    dplyr::inner_join(params) |>
    main_data_load_trajectories_data(granularity = trisk_granularity_r())
})


    portfolio_uploaded_r <- portfolio_upload$server("portfolio_upload")
    


    perimeter <- list(
      "trisk_granularity_r" = trisk_granularity_r,
      "trisk_run_params_r" = trisk_run_params_r,
      "crispy_data_r" = crispy_data_r,
      "trajectories_data_r" = trajectories_data_r
    )

    return(list(
      "perimeter" = perimeter,
      "portfolio_uploaded_r" = portfolio_uploaded_r
    ))
  })
}
