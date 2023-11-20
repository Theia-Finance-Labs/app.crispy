box::use(
  shiny[moduleServer, NS, observe, div, tags, reactiveValues, eventReactive, p, tagList],
  shiny.semantic[slider_input, dropdown_input, segment, update_dropdown_input]
)

box::use(
  app/logic/constant[
    available_discount_rate,
    available_risk_free_rate,
    available_growth_rate,
    available_shock_year,
    available_baseline_scenario,
    available_shock_scenario,
    available_scenario_geography
  ]
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  segment(
    class = "ui grid",
    div(
      class = "ui row",
      # First segment in the left half
      div(
        class = "eight wide column",
        segment(
          p("Discount Rate"),
          slider_input(
            ns("discount_rate"),
            custom_ticks = available_discount_rate,
            value = NULL
          ),
          p("Risk-Free Rate"),
          slider_input(
            ns("risk_free_rate"),
            custom_ticks = available_risk_free_rate,
            value = NULL
          ),
          p("Growth Rate"),
          slider_input(
            ns("growth_rate"),
            custom_ticks = available_growth_rate,
            value = NULL
          ),
          p("Shock Year"),
          slider_input(
            ns("shock_year"),
            custom_ticks = available_shock_year,
            value = NULL
          )
        )
      ),
      # Second segment in the right half
      div(
        class = "eight wide column",
        segment(
          p("Baseline Scenario"),
          dropdown_input(ns("baseline_scenario"),
            choices = available_baseline_scenario
          ),
          p("Target Scenario"),
          dropdown_input(ns("shock_scenario"),
            choices = available_shock_scenario
          ),
          p("Scenario Geography"),
          dropdown_input(ns("scenario_geography"),
            choices = available_scenario_geography
          )
        )
      )
    )
  )
}


####### Server


server <- function(id, backend_crispy_data) {
  moduleServer(id, function(input, output, session) {
    # Store all inputs in a reactiveValues object
    trisk_params_range <- reactiveValues()
    observe({
      trisk_params_range$discount_rate <- input$discount_rate
      trisk_params_range$risk_free_rate <- input$risk_free_rate
      trisk_params_range$growth_rate <- input$growth_rate
      trisk_params_range$shock_year <- input$shock_year
      trisk_params_range$baseline_scenario <- input$baseline_scenario
      trisk_params_range$shock_scenario <- input$shock_scenario
      trisk_params_range$scenario_geography <- input$scenario_geography
    })

    update_dropdowns(input, session, backend_crispy_data)

    multi_crispy_data.r <- eventReactive(
      c(
        trisk_params_range$discount_rate,
        trisk_params_range$risk_free_rate,
        trisk_params_range$growth_rate,
        trisk_params_range$shock_year,
        trisk_params_range$baseline_scenario,
        trisk_params_range$shock_scenario,
        trisk_params_range$scenario_geography
      ),
      ignoreInit = TRUE,
      {
        backend_crispy_data |>
          dplyr::filter(
            .data$discount_rate == trisk_params_range$discount_rate,
            .data$risk_free_rate == trisk_params_range$risk_free_rate,
            .data$growth_rate == trisk_params_range$growth_rate,
            .data$shock_year == trisk_params_range$shock_year,
            .data$baseline_scenario == trisk_params_range$baseline_scenario,
            .data$shock_scenario == trisk_params_range$shock_scenario,
            .data$scenario_geography == trisk_params_range$scenario_geography
          )
      }
    )
    return(multi_crispy_data.r)
  })
}


update_dropdowns <- function(input, session, backend_crispy_data){
    # Observe changes in baseline_scenario dropdown and update shock_scenario dropdown
  observe({
    selected_baseline <- input$baseline_scenario

    # Filter the data based on selected baseline scenario
    new_choices <- unique(backend_crispy_data[backend_crispy_data$baseline_scenario == selected_baseline,]$shock_scenario)

    # Update shock_scenario dropdown with unique values from the filtered data
    update_dropdown_input(session, "shock_scenario", choices = new_choices)
  })

  # Observe changes in both baseline_scenario and shock_scenario dropdowns to update scenario_geography dropdown
  observe({
    selected_baseline <- input$baseline_scenario
    selected_shock <- input$shock_scenario

    # Filter the data based on selected baseline and shock scenarios
    new_choices <- unique(backend_crispy_data[backend_crispy_data$baseline_scenario == selected_baseline & backend_crispy_data$shock_scenario == selected_shock, ]$scenario_geography)

    # Update scenario_geography dropdown with unique values from the filtered data
    update_dropdown_input(session, "scenario_geography", choices = new_choices)
  })
}
