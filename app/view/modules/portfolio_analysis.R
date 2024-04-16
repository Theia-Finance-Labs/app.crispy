box::use(
  shiny[
    moduleServer, NS, reactiveVal, reactive, observeEvent, observe,
    selectizeInput, updateSelectizeInput, eventReactive,
    div, tags, reactiveValues, HTML
  ],
  semantic.dashboard[box, icon],
  DT[dataTableProxy, DTOutput, renderDT, datatable, JS],
  shiny.semantic[semanticPage, segment, button],
  shinyjs[runjs, useShinyjs]
)



box::use(
  app/logic/constant[DEFAULT_ASSET_EXPIRATION_DATE, FILTER_CRISPY_OUTLIERS],
  app/logic/renamings[rename_tibble_columns]
)




##################### UI

ui <- function(id, portfolio_class = "") {
  ns <- NS(id)
  box(
    title = "Portfolio", width = 16, collapsible = FALSE,
    DT::dataTableOutput(outputId = ns("portfolio_table")),
    if (portfolio_class == "fixed_income") {
      # show the Row Edition only on the Loans tab
      portfolio_edition_ui(ns)
    }
  )
}


##################### Server


server <- function(
    id,
    portfolio_class,
    portfolio_uploaded_r,
    crispy_data_r,
    trisk_granularity_r,
    max_trisk_granularity,
    display_columns, editable_columns_names, colored_columns_names,
    possible_trisk_combinations = NULL,
    editable_rows = FALSE) {
  moduleServer(id, function(input, output, session) {
    # PORTFOLIO DATA =========================

    portfolio_data_r <- initialize_portfolio(
      crispy_data_r = crispy_data_r,
      trisk_granularity_r = trisk_granularity_r,
      portfolio_uploaded_r = portfolio_uploaded_r,
      portfolio_class = portfolio_class
    )

    

    # possible_trisk_combinations is not null for the Loans tab
    if (!is.null(possible_trisk_combinations)) {
      if (portfolio_class == "fixed_income") {
        portfolio_data_r <- portfolio_edition_server(
          input=input,
          session=session,
          portfolio_data_r = portfolio_data_r,
          crispy_data_r = crispy_data_r,
          trisk_granularity_r = trisk_granularity_r,
          max_trisk_granularity = max_trisk_granularity,
          possible_trisk_combinations = possible_trisk_combinations
        )
      }
    }

    # ANALYSIS DATA ===================================


    analysis_data_r <- generate_analysis_data(
      portfolio_data_r = portfolio_data_r,
      crispy_data_r = crispy_data_r,
      trisk_granularity_r = trisk_granularity_r,
      portfolio_class = portfolio_class
    )

    # TABLE DISPLAY IN UI ===================================

    display_analysis_data(
      output = output,
      analysis_data_r = analysis_data_r,
      display_columns = display_columns,
      editable_columns_names = editable_columns_names,
      colored_columns_names = colored_columns_names,
      trisk_granularity_r = trisk_granularity_r
    )

    # TABLE INPUTS MGMT ===================================

    update_portfolio_with_user_input(
      input = input,
      analysis_data_r = analysis_data_r,
      portfolio_data_r = portfolio_data_r,
      trisk_granularity_r = trisk_granularity_r,
      display_columns = display_columns,
      editable_columns_names = editable_columns_names,
      max_trisk_granularity = max_trisk_granularity
    )

    return(analysis_data_r)
  })
}











##################### Modules


initialize_portfolio <- function(trisk_granularity_r, portfolio_uploaded_r, crispy_data_r, portfolio_class) {
  # Initial portfolio data structure
  portfolio_data_r <- reactiveVal()

  shiny::observeEvent(c(trisk_granularity_r(), portfolio_uploaded_r(), crispy_data_r()), {
    if (nrow(portfolio_uploaded_r()) > 0) {
      portfolio_data_r(portfolio_uploaded_r())
    } else {
      trisk_granularity <- trisk_granularity_r()

      # Create the portfolio dynamic columns data structure
      # TODO test all dynamic columns have to be character type
      dynamic_cols <- stats::setNames(lapply(trisk_granularity, function(x) character()), trisk_granularity)
      dynamic_cols <- dplyr::as_tibble(dynamic_cols)

      # define static columns that are required at all times (for functional or computational purposes)
      static_cols <- tibble::tibble(
        portfolio_id = character(), # is always 1
        exposure_value_usd = numeric(),
        loss_given_default = numeric(), # is always NA for Crispy Equities
        expiration_date = character(), # is always NA for Crispy Equities
        pd_portfolio = numeric(), # is always NA for Crispy Equities
        asset_type = character() # equity OR fixed_income
      )
      # creates the empty portfolio data
      portfolio_data <- dplyr::bind_cols(dynamic_cols, static_cols)

      if ((!"company_id" %in% trisk_granularity)) {
        if (!is.null(crispy_data_r())) {
          granularity <- dplyr::intersect(colnames(portfolio_data), colnames(crispy_data_r()))

          # in equity , populate the portfolio if empty, and not company granularity
          portfolio_data <- portfolio_data |>
            dplyr::right_join(
              crispy_data_r() |> dplyr::distinct_at(granularity)
            ) |>
            dplyr::mutate(
              portfolio_id = as.character(1)
            )

          portfolio_data <- portfolio_data |>
            dplyr::select(-c(expiration_date)) |>
            # Create a row for each item in DEFAULT_ASSET_EXPIRATION_DATE for each row in portfolio_data
            tidyr::crossing(expiration_date = DEFAULT_ASSET_EXPIRATION_DATE)
        }
      }
      portfolio_data <- portfolio_data |>
        dplyr::mutate(asset_type = portfolio_class)

      portfolio_data_r(portfolio_data)
    }
  })

  return(portfolio_data_r)
}


generate_analysis_data <- function(portfolio_data_r, crispy_data_r, trisk_granularity_r, portfolio_class) {
  analysis_data_r <- reactiveVal()
  crispy_data_agg_r <- reactiveVal()

  # update analysis data in case of direct changes
  observeEvent(analysis_data_r(), ignoreInit=TRUE, {
    if (!is.null(analysis_data_r())){
      analysis_data <- analysis_data_r() |>
        stress.test.plot.report:::aggregate_equities() |>
        stress.test.plot.report:::compute_analysis_metrics() |>
            dplyr::mutate(
              crispy_perc_value_change = round(.data$crispy_perc_value_change, digits = 4),
              crispy_value_loss = round(.data$crispy_value_loss, digits = 2),
              pd_shock = round(.data$pd_shock, digits = 4),
              expected_loss_shock = round(.data$expected_loss_shock, digits = 2),
              pd_difference = pd_shock - pd_baseline
            )
      analysis_data_r(analysis_data)
    }
  })

  observeEvent(c(portfolio_data_r(), crispy_data_r()), {
    if (!is.null(portfolio_data_r()) & !is.null(crispy_data_r())) {
      granularity <- dplyr::intersect(colnames(portfolio_data_r()), colnames(crispy_data_r()))
      if (all(granularity %in% trisk_granularity_r())) { # only does anything if things stable
        # Creates and aggregate Analysis data without portfolio with stress.test.plot.report fun
        if (nrow(portfolio_data_r() > 0)) {
          analysis_data <- stress.test.plot.report:::load_input_plots_data_from_tibble(
            portfolio_data = portfolio_data_r(),
            multi_crispy_data = crispy_data_r(),
            granularity = granularity,
            filter_outliers = FILTER_CRISPY_OUTLIERS
          ) |>
            dplyr::mutate(
              crispy_perc_value_change = round(.data$crispy_perc_value_change, digits = 4),
              crispy_value_loss = round(.data$crispy_value_loss, digits = 2),
              pd_shock = round(.data$pd_shock, digits = 4),
              expected_loss_shock = round(.data$expected_loss_shock, digits = 2),
              pd_difference = pd_shock - pd_baseline
            )
        } else {
          analysis_data <- dplyr::inner_join(
            portfolio_data_r(),
            crispy_data_r(),
            by = granularity
          ) |>
            dplyr::mutate(
              crispy_perc_value_change = NA,
              crispy_value_loss = NA,
              pd_shock = NA,
              expected_loss_shock = NA
            )
        }
      }
      analysis_data_r(analysis_data)
    }
  })

  return(analysis_data_r)
}

display_analysis_data <- function(output, analysis_data_r, display_columns, editable_columns_names, colored_columns_names, trisk_granularity_r) {
  observeEvent(analysis_data_r(), ignoreInit = TRUE, {
    table_to_display <- analysis_data_r() |>
      dplyr::select(
        dplyr::any_of(display_columns)
      )

    disabled_columns <- which(!colnames(table_to_display) %in% editable_columns_names)
    colored_columns <- which(colnames(table_to_display) %in% colored_columns_names)

    table_to_display <- rename_tibble_columns(table_to_display, words_class = "analysis_columns")

    n_granul_cols <- length(trisk_granularity_r())
    # Render the editable table
    output$portfolio_table <- DT::renderDT(
      {
        DT::datatable(table_to_display,
          editable = list(target = "cell", disable = list(columns = disabled_columns)),
          selection="multiple",
          options = list(
            lengthChange = FALSE, # Remove "Show XXX entries" option
            paging = FALSE, # Remove pagination
            searching = FALSE, # Remove search input
            info = FALSE, # Remove "Showing N of X entries"
            columnDefs = list( # Change colors of text in cells
              list(targets = colored_columns, createdCell = JS(
                "function(cell, cellData, rowData) {
              $(cell).css('color', cellData < 0 ? 'red' : 'green');
            }"
              ))
            )
          ),
          class = "display compact" # fit table to container
        )
      },
      server = FALSE
    )
  })
}

update_portfolio_with_user_input <- function(
    input,
    analysis_data_r,
    portfolio_data_r,
    trisk_granularity_r,
    display_columns,
    editable_columns_names,
    max_trisk_granularity) {
  # Update data structure on cell edit
  observeEvent(input$portfolio_table_cell_edit, {
    n_granul_cols <- length(trisk_granularity_r())
    info <- input$portfolio_table_cell_edit
    analysis_data <- analysis_data_r()
    # update the portfolio data with UI cell change
    displayed_display_columns <- display_columns[display_columns %in% colnames(analysis_data_r())]

    if (is.numeric(info$value)) {
      analysis_data[info$row, displayed_display_columns[info$col]] <- info$value
      analysis_data_r(analysis_data)
    }
  })
}


### PORTFOLIO EDITION

portfolio_edition_ui <- function(ns){
  tags$div(
    class = "ui grid container", # Main grid container for layout
    style = "padding: 20px;", # Add some padding around the container
    div(
      class = "row",
      div(
        class = "five wide column",
        shiny.semantic::dropdown_input(
          ns("ald_sector_dropdown"),
          default_text = "Sector",
          choices = NULL # Populate with your choices
        )
      ),
      div(
        class = "five wide column",
        shiny.semantic::dropdown_input(
          ns("ald_business_unit_dropdown"),
          default_text = "Business Unit",
          choices = NULL # Populate with your choices
        )
      ),
      div(
        class = "five wide column",
        shiny.semantic::dropdown_input(
          ns("maturity_year"),
          default_text = "Year of maturity",
          choices = 2024:2034, # TODO GO IN CONF
          value = 2034
        )
      )
    ),
    div(
      class = "row",
      div(
        class = "eight wide column",
        shiny.semantic::button(
          ns("add_row_btn"),
          "Add new row",
          icon = icon("plus"), ,
          class = "ui button fluid"
        )
      ),
      div(
        class = "eight wide column",
        shiny.semantic::button(
          ns("delete_row_btn"),
          "Delete Selected Rows",
          icon = icon("delete"),
          class = "ui button fluid"
        )
      )
    )
  )
}


portfolio_edition_server <- function(
  input,
  session,
  portfolio_data_r,
    crispy_data_r,
    trisk_granularity_r,
    max_trisk_granularity,
    possible_trisk_combinations = NULL
){
      # ADD ROW =================

    selected_maturity_year_r <- reactive({
      input$maturity_year
    })

    selected_ald_sector_r <- reactive({
      choice <- input$ald_sector_dropdown
      # renamed_choice <- rename_string_vector(choice, words_class = "scenarios", dev_to_ux = FALSE)
      # return(renamed_choice)
      return(choice)
    })
    selected_ald_business_unit_r <- reactive({
      choice <- input$ald_business_unit_dropdown
      # renamed_choice <- rename_string_vector(choice, words_class = "scenarios", dev_to_ux = FALSE)
      # return(renamed_choice)
      return(choice)
    })


    # synchronise dropdown choices  with the possible combinations
    # at the same time get the company query return value to update selected_company_name_r
    # TODO selected_company_name_r kept as legacy for later reactivation of the company
    selected_company_name_r <- update_ald_dropdowns(
      input = input,
      session = session,
      trisk_granularity_r = trisk_granularity_r,
      crispy_data_r = crispy_data_r
    )

    # EDIT ROWS =================

    # BUTTONS ADD ROWS

    portfolio_data_r <- rows_addition(
      input = input,
      portfolio_data_r = portfolio_data_r,
      selected_ald_business_unit_r = selected_ald_business_unit_r,
      selected_ald_sector_r = selected_ald_sector_r,
      selected_maturity_year_r = selected_maturity_year_r
    )

    # Delete row

    portfolio_data_r <- rows_deletion(
      input,
      portfolio_data_r = portfolio_data_r,
      selected_rows = selected_rows
    )
    return(portfolio_data_r)
}

rows_addition <- function(input, portfolio_data_r, selected_ald_business_unit_r, selected_ald_sector_r, selected_maturity_year_r) {
  # add a new row by creating it in the portfolio
  observeEvent(input$add_row_btn, {
    
    user_defined_row <- tibble::as_tibble(list(
      # company_id = ifelse(is.null(selected_company_name_r()), NA, selected_company_name_r()),
      ald_business_unit = ifelse(is.null(selected_ald_business_unit_r()), NA, selected_ald_business_unit_r()),
      ald_sector = ifelse(is.null(selected_ald_sector_r()), NA, selected_ald_sector_r()),
      expiration_date = paste0(as.character(selected_maturity_year_r()), "-01-01"),
      asset_type = "fixed_income" #HARDCODED BC ONLY LOANS APP ACCESS THIS CODE
    ))

    use_columns <- dplyr::intersect(names(user_defined_row), names(portfolio_data_r()))
    user_defined_row <- user_defined_row |>
      dplyr::select_at(use_columns)

    updated_portfolio_data <- dplyr::bind_rows(
      portfolio_data_r(),
      user_defined_row
    )
    portfolio_data_r(updated_portfolio_data)
  })

  # proxy <- DT::dataTableProxy(id)
  return(portfolio_data_r)
}


rows_deletion <- function(
    input,
    portfolio_data_r,
    selected_rows) {
  observeEvent(input$delete_row_btn, {
    selected_rows <- input$portfolio_table_rows_selected

    if (length(selected_rows)) {
      my_data_data <- portfolio_data_r()
      my_data_data <- my_data_data[-selected_rows, , drop = FALSE]
      portfolio_data_r(my_data_data)
      # DT::replaceData(proxy, my_data_data, resetPaging = FALSE)
    }
  })
  return(portfolio_data_r)
}

# Synchronise the scenarios available depending on user scenario choice
update_ald_dropdowns <- function(input, session,
                                 crispy_data_r,
                                 trisk_granularity_r) {
  # Observe changes in possible_trisk_combinations and update baseline_scenario dropdown
  observeEvent(crispy_data_r(), ignoreInit = TRUE, {
    
    possible_sectors <- unique(crispy_data_r()$ald_sector)

    # rename the scenarios to front end appropriate name
    # new_choices <- rename_string_vector(possible_shocks, words_class = "scenarios")
    new_choices <- possible_sectors
    # Update shock_scenario dropdown with unique values from the filtered data
    shiny.semantic::update_dropdown_input(session, "ald_sector_dropdown", choices = new_choices)
  })

  # Observe changes in baseline_scenario dropdown and update shock_scenario dropdown
  observeEvent(c(input$ald_sector_dropdown, crispy_data_r()), ignoreInit = TRUE, {
    if (is.null(input$ald_sector_dropdown) || length(input$ald_sector_dropdown) == 0) {
      return() # Skip further execution if no selection is made
    }
    if ("ald_business_unit" %in% trisk_granularity_r()) {
      possible_ald_business_units <- crispy_data_r() |> dplyr::filter(ald_sector == input$ald_sector_dropdown)
      possible_ald_business_units <- unique(possible_ald_business_units$ald_business_unit)
    } else {
      possible_ald_business_units <- c("")
    }
    shiny.semantic::update_dropdown_input(
      session,
      "ald_business_unit_dropdown",
      choices = possible_ald_business_units
    )
  })
}
