box::use(
  shiny[moduleServer, NS, req, div, plotOutput, uiOutput, eventReactive, observeEvent, renderUI, renderPlot, reactive],
  semantic.dashboard[box]
)

box::use(
  app / logic / constant[use_ald_sector],
  app / logic / scenario_time_plot[pipeline_scenario_time_plot]
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  list(
    # Third row with 1 taking the entire page width
    box(width = 16, plotOutput(ns("baseline_scenario_plot"))),

    # Fourth row with 2 taking the entire page width
    box(width = 16, plotOutput(ns("shock_scenario_plot")))
  )
}

####### Server


server <- function(id, scenario_data, analysis_data_r) {
  moduleServer(id, function(input, output, session) {
    ### BASELINE SCENARIO

    # Reactive expression to prepare data for plotting
    baseline_scenario_data_r <- reactive({
      req(analysis_data_r()) # Ensure that analysis_data is available
      scenario_data |>
        dplyr::filter(
          .data$scenario %in% analysis_data_r()$crispy.baseline_scenario,
          .data$scenario_geography %in% analysis_data_r()$crispy.scenario_geography
        )
    })

    observeEvent(baseline_scenario_data_r(), ignoreInit = TRUE, {
      # Render plot
      scenario_time_plot <- pipeline_scenario_time_plot(baseline_scenario_data_r(),
        scenario_type = "baseline"
      )
      output$baseline_scenario_plot <- renderPlot({
        scenario_time_plot
      })
    })


    ### SHOCK SCENARIO

    # Reactive expression to prepare data for plotting
    shock_scenario_data_r <- reactive({
      req(analysis_data_r()) # Ensure that analysis_data is available
      scenario_data |>
        dplyr::filter(
          .data$scenario %in% analysis_data_r()$crispy.shock_scenario,
          .data$scenario_geography %in% analysis_data_r()$crispy.scenario_geography
        )
    })

    observeEvent(shock_scenario_data_r(), ignoreInit = TRUE, {
      # Render plot
      scenario_time_plot <- pipeline_scenario_time_plot(shock_scenario_data_r(),
        scenario_type = "shock"
      )
      output$shock_scenario_plot <- renderPlot({
        scenario_time_plot
      })
    })
  })
}
