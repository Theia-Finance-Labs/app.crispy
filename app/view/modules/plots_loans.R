box::use(
  shiny[moduleServer, NS, plotOutput, renderPlot, observeEvent, tags]
)

box::use(
  app/logic/plots/pd_term_plot[pipeline_pd_term_plot],
  app/logic/plots/crispy_npv_change_plot[pipeline_crispy_npv_change_plot]
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  shiny::fluidRow(
    semantic.dashboard::box(title = "PD Difference", width = 8, plotOutput(ns("pd_term_plotoutput"))),
    semantic.dashboard::box(title = "Exposure Change", width = 8, plotOutput(ns("exposure_change_plot")))
  )
}

####### Server



server <- function(id, analysis_data_r, crispy_data_agg_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    observeEvent(analysis_data_r(), {
      if (nrow(analysis_data_r()) > 0) {
      granul_levels <- dplyr::intersect(colnames(analysis_data_r()), names(max_trisk_granularity))
      granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]

      # browser()

crispy_user_filtered <- crispy_data_agg_r() |> 
  dplyr::inner_join(
    analysis_data_r() |> dplyr::distinct_at(granul_top_level), 
    by=granul_top_level
    )


prepare_for_el_plot(
  analysis_data=analysis_data_r(),
  x_var=granul_top_level
)
    

      pd_term_plot <- pipeline_pd_term_plot(
        crispy_data_agg=crispy_user_filtered, 
        facet_var=granul_top_level
      )
      output$pd_term_plotoutput <- renderPlot({
        pd_term_plot
      })

      # crispy_npv_change_plot <- pipeline_crispy_npv_change_plot(analysis_data_r(), x_var = granul_top_level)
      # output$crispy_npv_change_plot <- renderPlot({
      #   crispy_npv_change_plot
      # })
      }
    })
  })
}



prepare_for_el_plot <- function(analysis_data, x_var) {
  data_expected_loss <- analysis_data |>
    tidyr::pivot_longer(
      cols = tidyr::starts_with("expected_loss_"),
      names_to = "el_type",
      values_to = "el_value",
      names_prefix = "expected_loss_"
    ) |> 
    dplyr::select_at(c(x_var, "exposure_value_usd", "el_type", "el_value"))
  return(data_expected_loss)
}
