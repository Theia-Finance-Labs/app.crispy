box::use(
  shiny[moduleServer, NS, reactiveVal, observeEvent, observe, eventReactive, div],
  semantic.dashboard[box],
  DT[DTOutput, renderDT, datatable, JS]
)

box::use(
  app/logic/constant[max_crispy_granularity, portfolio_crispy_merge_cols]
)





####### UI

ui <- function(id) {
  ns <- NS(id)
  # Left Column (1/3 width)


  # First row with 3 taking the entire page width
  box(width = 16, DTOutput(outputId = ns("portfolio_table")))
}

####### Server


server <- function(id, multi_crispy_data.r) {
  moduleServer(id, function(input, output, session) {
    # Initial portfolio data structure
    portfolio_data.r <- reactiveVal({
      tibble::tibble(
        ald_sector = character(),
        exposure_value_usd = numeric(),
        pd_portfolio = numeric(),
        loss_given_default = numeric()
      )
    })

    analysis_data.r <- eventReactive(c(
      portfolio_data.r(),
      multi_crispy_data.r()
    ), ignoreInit = TRUE, {
      if (!is.null(portfolio_data.r()) & !is.null(multi_crispy_data.r())) {
        if (nrow(portfolio_data.r()) == 0) {
          # initialise the porfolio sector column
          portfolio_data <- portfolio_data.r()
          portfolio_data <- portfolio_data |>
            dplyr::right_join(multi_crispy_data.r() |>
              dplyr::distinct(.data$ald_sector))
          portfolio_data.r(portfolio_data)
        }

        stress.test.plot.report::main_load_analysis_data(
          portfolio_data = portfolio_data.r(),
          multi_crispy_data = multi_crispy_data.r(),
          portfolio_crispy_merge_cols = portfolio_crispy_merge_cols
        )
      }
    })

    observeEvent(analysis_data.r(), ignoreInit = TRUE, {
      table_to_display <- analysis_data.r() |>
        dplyr::select(
          .data$portfolio.ald_sector,
          .data$portfolio.exposure_value_usd,
          .data$crispy_perc_value_change,
          .data$crispy_value_loss
        )

      # Render the editable table
      output$portfolio_table <- renderDT(
        {
          datatable(table_to_display,
            editable = list(target = "cell", disable = list(columns = c(1, 3, 4))),
            options = list(
              columnDefs = list(
                list(targets = 3:4, createdCell = JS(
                  "function(cell, cellData, rowData) {
              $(cell).css('color', cellData < 0 ? 'red' : 'green');
            }"
                ))
              )
            )
          )
        },
        server = FALSE
      )
    })

    # Update data structure on cell edit
    observeEvent(input$portfolio_table_cell_edit, {
      info <- input$portfolio_table_cell_edit
      portfolio_data <- portfolio_data.r()
      # data can be edited only in the second column
      if (info$col == 2) {
        if ((typeof(info$value) == "integer") |
          (typeof(info$value) == "double")) {
          portfolio_data[info$row, info$col] <- info$value
          portfolio_data.r(portfolio_data)
        }
      }
    })



    return(analysis_data.r)
  })
}
