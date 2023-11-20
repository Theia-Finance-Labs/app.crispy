box::use(
  shiny[moduleServer, NS, renderUI, tags, uiOutput, eventReactive, observeEvent, div, a],
  shiny.semantic[semanticPage, segment],
  semantic.dashboard[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, icon, box],
)
box::use(
  app/view/crispy_generation,
  app/view/portfolio_visualizer,
  app/view/equity_change_plots,
  app/view/scenario_plots,
  app/logic/constant[backend_crispy_data_path, scenario_data_path, use_ald_sector]
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
      crispy_generation$ui(ns("crispy_generation")),
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
    backend_crispy_data <- arrow::read_parquet(backend_crispy_data_path) |>
      dplyr::filter(.data$term == 5) |>
      dplyr::filter(.data$ald_sector %in% use_ald_sector)

    scenario_data <- readr::read_csv(scenario_data_path) |>
      dplyr::filter(.data$ald_sector %in% use_ald_sector) |>
      dplyr::group_by(scenario_geography, scenario, ald_sector, units, year) |>
      dplyr::summarise(fair_share_perc = sum(fair_share_perc), .groups = "drop")

    multi_crispy_data.r <- crispy_generation$server("crispy_generation", backend_crispy_data)
    analysis_data.r <- portfolio_visualizer$server("portfolio_visualizer", multi_crispy_data.r)

    equity_change_plots$server("equity_change_plots", analysis_data.r)
    scenario_plots$server("scenario_plots", scenario_data, analysis_data.r)
  })
}
