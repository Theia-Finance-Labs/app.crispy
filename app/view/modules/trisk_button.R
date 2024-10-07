# Load required packages
box::use(
  semantic.dashboard[dashboardBody, dashboardHeader, dashboardPage, dashboardSidebar],
  shiny[div, h1, moduleServer, NS, observe, observeEvent, reactive, reactiveVal, tags, reactiveValues],
  shiny.semantic[semanticPage],
  shinyjs[useShinyjs],
  app/logic/trisk_button_logic[run_trisk_analysis]
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  tags$div(
    tags$div(
      id = ns("mymodal"),
      class = "ui modal",
      tags$div(class = "header", "Processing"),
      tags$div(
        class = "content",
        tags$p("Please wait while the model is being run with the chosen parameters. This may take up to 10 minutes.")
      )
    ),
    tags$button(
      id = ns("run_trisk"),
      class = "ui fluid button ", # Added custom class for styling
      "Run Trisk"
    )
  )
}

####### Server

server <- function(
    id,
    assets_data,
    scenarios_data,
    financial_data,
    carbon_data,
    trisk_run_params_r) {
  moduleServer(id, function(input, output, session) {

    # Reactive values defined at the top
    reactive_trisk_results <- reactiveValues(
      params = NULL,
      trajectories = NULL,
      npv_results = NULL,
      pd_results = NULL
    )

    # Fetch or compute trisk on button click
    shiny::observeEvent(input$run_trisk, ignoreNULL = TRUE, {

      # Convert reactive values to a list for use in the function
      trisk_run_params <- shiny::reactiveValuesToList(trisk_run_params_r())
      selected_country <- selected_country_r()

      # Open modal dialog
      shinyjs::runjs(
        paste0(
          "$('#", session$ns("mymodal"), "').modal({closable: true}).modal('show');"
        )
      )

      # Run trisk analysis and get new results
      new_results <- run_trisk_analysis(
        assets_data = assets_data,
        scenarios_data = scenarios_data,
        financial_data = financial_data,
        carbon_data = carbon_data,
        trisk_run_params = trisk_run_params,
        selected_country = selected_country
      )

      if (!is.null(new_results)) {
        # Update the reactive values with new results
        reactive_trisk_results$params <- new_results$params
        reactive_trisk_results$trajectories <- new_results$trajectories
        reactive_trisk_results$npv_results <- new_results$npv_results
        reactive_trisk_results$pd_results <- new_results$pd_results
      }

      # Close the modal dialog
      shinyjs::runjs(
        paste0(
          "$('#", session$ns("mymodal"), "').modal('hide');"
        )
      )
    })

    return(reactive_trisk_results)
  })
}
