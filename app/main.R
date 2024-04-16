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
  app/view/tab_documentation,
  app/view/tab_equities,
  app/view/tab_loans,
  # logic
  app/logic/constant[
    TRISK_API_SERVICE,
    CRISPY_MODE,
    trisk_input_path,
    backend_trisk_run_folder,
    max_trisk_granularity,
    available_vars,
    hide_vars
  ],
  app/logic/cloud_logic[get_possible_trisk_combinations_from_api]
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
            src = "static/logo_1in1000.png",
            height = "10%", width = "auto",
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
            tab_loans$ui(
              ns("tab_loans"),
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
    if (Sys.getenv("CRISPY_APP_ENV") == "local") {
      possible_trisk_combinations <- r2dii.climate.stress.test::get_scenario_geography_x_ald_sector(trisk_input_path)
    } else if (Sys.getenv("CRISPY_APP_ENV") == "cloud") {
      possible_trisk_combinations <- get_possible_trisk_combinations_from_api(trisk_api_service = TRISK_API_SERVICE)
    } else {
      stop("must set environment variable CRISPY_APP_ENV to 'local' or 'cloud'")
    }
    # the TRISK runs are generated In the sidebar module
    sidebar_parameters_out <- sidebar_parameters$server(
      "sidebar_parameters",
      max_trisk_granularity = max_trisk_granularity, # constant
      possible_trisk_combinations = possible_trisk_combinations, # computed constant
      backend_trisk_run_folder = backend_trisk_run_folder, # constant
      trisk_input_path = trisk_input_path, # constant
      available_vars = available_vars, # constant
      hide_vars = hide_vars # constant
    )

    perimeter <- sidebar_parameters_out$perimeter
    portfolio_uploaded_r <- sidebar_parameters_out$portfolio_uploaded_r

    tab_documentation$server("tab_documentation")

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
      tab_loans$server(
        "tab_loans",
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
