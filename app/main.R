# This file contains the main code for the CRISPY Shiny application. It defines the UI and server functions.

# Load required packages
box::use(
  shiny[moduleServer, NS, renderUI, tags, HTML, uiOutput, conditionalPanel, observe, observeEvent, div, a, reactiveVal, p, eventReactive],
  shiny.semantic[semanticPage],
  semantic.dashboard[dashboardPage, dashboardBody, dashboardSidebar, dashboardHeader, icon]
)

# Load required modules and logic files
box::use(
  # modules
  app/view/sidebar_parameters,
  app/view/tab_equities,
  app/view/tab_fixed_income,
  # logic
  app/logic/constant[
    TRISK_INPUT_PATH,
    CRISPY_MODE,
    TRISK_POSTGRES_DB,
    TRISK_POSTGRES_HOST,
    TRISK_POSTGRES_PASSWORD,
    TRISK_POSTGRES_PORT,
    TRISK_POSTGRES_USER,
    max_trisk_granularity,
    available_vars,
    hide_vars
  ],
  app/logic/data_load[download_db_tables_postgres,get_possible_trisk_combinations]
)


# Define the UI function
#' @export
ui <- function(id) {
  ns <- NS(id)

  shiny.semantic::semanticPage(
    shinyjs::useShinyjs(), # Initialize shinyjs
    # CONTENT PAGE
    tags$div(
      class = "header", # Add a loading overlay
      tags$head(
        tags$style(HTML("
            #loading-overlay {
              position: fixed;
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              background: rgba(255, 255, 255, 0.8);
              z-index: 9999;
              display: flex;
              align-items: center;
              justify-content: center;
              font-size: 2em;
            }
          "))
      ),
      div(id = "loading-overlay", "Initializing...")
    ),
    dashboardPage(
      title = "Documentation",
      # dashboardHeader
      dashboardHeader(title = "CRISPY"),
      # dashboardSidebar
      dashboardSidebar(
        tags$div(
          sidebar_parameters$ui(
            ns("sidebar_parameters"),
            max_trisk_granularity = max_trisk_granularity, # constant
            available_vars = available_vars # constant
          ),
          shiny::img(
            src = "static/logo_life_stress.jpg",
            height = "30%", width = "auto",
            style = "
              display: block;
              margin-left: auto;
              margin-right: auto;
              margin-top: 10px;
              margin-bottom: 10px;"
          )
        ),
        size = "very wide",
        visible = TRUE
      ),

      # dashboardBody
      dashboardBody(
        div(
          class = "ui container",
          if ((CRISPY_MODE == "equity") | CRISPY_MODE == "") {
            # equity tab
            tab_equities$ui(
              ns("tab_equities"),
              max_trisk_granularity = max_trisk_granularity, # constant
              available_vars = available_vars # constant
            )
          },
          if ((CRISPY_MODE == "fixed_income")) {
            # fixed_income tab
            tab_fixed_income$ui(
              ns("tab_fixed_income"),
              max_trisk_granularity = max_trisk_granularity, # constant
              available_vars = available_vars # constant
            )
          }
        )
      )
    )
  )
}


# Define the server function
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    if (!dir.exists(TRISK_INPUT_PATH)) {
      dir.create(TRISK_INPUT_PATH)
    }
    if (length(dir(TRISK_INPUT_PATH)) == 0) {
      tables <- c(
        "assets_sampled",
        "scenarios",
        "ngfs_carbon_price",
        "financial_features"
      )

      download_db_tables_postgres(
        save_dir = TRISK_INPUT_PATH,
        tables = tables,
        dbname = TRISK_POSTGRES_DB,
        host = TRISK_POSTGRES_HOST,
        port = TRISK_POSTGRES_PORT,
        user = TRISK_POSTGRES_USER,
        password = TRISK_POSTGRES_PASSWORD
      )
    }

    assets_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "assets_sampled.csv"), show_col_types = FALSE)
    scenarios_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "scenarios.csv"), show_col_types = FALSE)
    financial_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "financial_features.csv"), show_col_types = FALSE)
    carbon_data <- readr::read_csv(file.path(TRISK_INPUT_PATH, "ngfs_carbon_price.csv"), show_col_types = FALSE)

    possible_trisk_combinations <- get_possible_trisk_combinations(scenarios_data = scenarios_data)


    # the TRISK runs are generated In the sidebar module
    sidebar_parameters_out <- sidebar_parameters$server(
      "sidebar_parameters",
assets_data=assets_data,
scenarios_data=scenarios_data,
financial_data=financial_data,
carbon_data=carbon_data,
      max_trisk_granularity = max_trisk_granularity, # constant
      possible_trisk_combinations = possible_trisk_combinations, # computed constant
      available_vars = available_vars, # constant
      hide_vars = hide_vars # constant
    )

    perimeter <- sidebar_parameters_out$perimeter
    portfolio_uploaded_r <- sidebar_parameters_out$portfolio_uploaded_r

    if ((CRISPY_MODE == "equity") | CRISPY_MODE == "") {
      tab_equities$server(
        "tab_equities",
        backend_trisk_run_folder = backend_trisk_run_folder, # constant
        max_trisk_granularity = max_trisk_granularity, # constant
        perimeter = perimeter,
        portfolio_uploaded_r = portfolio_uploaded_r
      )
    }
    if ((CRISPY_MODE == "fixed_income")) {
      tab_fixed_income$server(
        "tab_fixed_income",
        backend_trisk_run_folder = backend_trisk_run_folder, # constant
        possible_trisk_combinations = possible_trisk_combinations, # computed constant
        max_trisk_granularity = max_trisk_granularity, # constant
        perimeter = perimeter,
        portfolio_uploaded_r = portfolio_uploaded_r
      )
    }
    shinyjs::runjs('$("#loading-overlay").hide();')
  })
}
