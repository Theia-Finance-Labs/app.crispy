box::use(
  shiny[moduleServer, NS, renderUI, tags, uiOutput, eventReactive, observeEvent, div, a],
  shiny.semantic[semanticPage, segment],
  semantic.dashboard[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, icon, box],
)
box::use(
  app / view / params_picker,
  app / view / portfolio_visualizer,
  app / view / equity_change_plots,
  app / view / scenario_plots,
  app / logic / data_load[
    load_backend_crispy_data, 
    load_backend_trajectories_data, 
    load_backend_trisk_run_data, 
    get_run_id_from_params_selection
    ]
)

#######
####### UI
#######


#' @export
ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    title = "CRISPY",
    dashboardHeader(title = "Crispy app"),
    dashboardSidebar(
      params_picker$ui(ns("params_picker")),
      size = "very wide"
    ),
    dashboardBody(
      # First row with the left (1/3 width) the right (2/3 width)
      semanticPage(
        portfolio_visualizer$ui(ns("portfolio_visualizer")),
        equity_change_plots$ui(ns("equity_change_plots")),
        scenario_plots$ui(ns("scenario_plots"))
      )
    )
  )
}


#######
####### Server
#######

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    backend_crispy_data <- load_backend_crispy_data()
    backend_trajectories_data <- load_backend_trajectories_data()
    backend_trisk_run_data <- load_backend_trisk_run_data()

    trisk_params_selection <- params_picker$server("params_picker", backend_trisk_run_data)

    run_id_r <- get_run_id_from_params_selection(backend_trisk_run_data, trisk_params_selection)

    crispy_data_r <- eventReactive(run_id_r, ignoreInit=TRUE, {
      browser()
      backend_crispy_data |>
        dplyr::filter(run_id == run_id_r())
    })
    trajectories_data_r <- eventReactive(run_id_r, ignoreInit=TRUE, {
      backend_trajectories_data |>
        dplyr::filter(run_id == run_id_r())
    })

    analysis_data_r <- portfolio_visualizer$server("portfolio_visualizer", crispy_data_r)

    equity_change_plots$server("equity_change_plots", analysis_data_r)
    scenario_plots$server("scenario_plots", trajectories_data_r)
  })
}
