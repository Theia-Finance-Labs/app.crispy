box::use(
  shiny[moduleServer, NS, plotOutput, renderPlot, observeEvent, tags]
)

box::use(
  app/logic/plots/pd_term_plot[pipeline_pd_term_plot],
  app/logic/plots/exposure_change_plot[draw_exposure_change_plot]
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  shiny::fluidRow(
    # style = "height: 1300px;",
    semantic.dashboard::box(
      title = "PD Difference", 
      width = 8, 
      collapsible = FALSE,
      height="1300px",
      plotOutput(ns("pd_term_plotoutput"))
      ),
    semantic.dashboard::box(
      title = "Expected Loss Baseline and Shock", 
      width = 8, 
      collapsible = FALSE,
      height="1300px",
      plotOutput(ns("expected_loss_plot_output")))
  )
}

####### Server



server <- function(id, analysis_data_r, crispy_data_agg_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    
   observeEvent(analysis_data_r(), {
      if (nrow(analysis_data_r()) > 0) {
      granul_levels <- dplyr::intersect(colnames(analysis_data_r()), names(max_trisk_granularity))
      granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]

      expected_loss_plot <- pipeline_expected_loss_plot(
        analysis_data_r(), 
        facet_var = granul_top_level
        )
      output$expected_loss_plot_output <- renderPlot({
        expected_loss_plot
      })
  }})

    observeEvent(crispy_data_agg_r(), {
      if (nrow(crispy_data_agg_r()) > 0) {
      granul_levels <- dplyr::intersect(colnames(crispy_data_agg_r()), names(max_trisk_granularity))
      granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]

      pd_term_plot <- pipeline_pd_term_plot(
        crispy_data_agg=crispy_data_agg_r(), 
        facet_var=granul_top_level
      )
      output$pd_term_plotoutput <- renderPlot({
        pd_term_plot
      })

      }
    })
  })
}



pipeline_expected_loss_plot <- function(
  analysis_data,
  facet_var
){

data_expected_loss_plot <- prepare_for_expected_loss_plot(
 analysis_data=analysis_data,
facet_var=facet_var
)

  expected_loss_plot <- draw_exposure_change_plot(
    data_expected_loss_plot,
    x_var="el_type",
    y_exposure_var = "exposure_value_usd",
    y_value_loss_var = "el_value",
    facet_var=facet_var
    )

}

prepare_for_expected_loss_plot <- function(analysis_data, facet_var) {
  data_expected_loss_plot <- analysis_data |>
    tidyr::pivot_longer(
      cols = tidyr::starts_with("expected_loss_"),
      names_to = "el_type",
      values_to = "el_value",
      names_prefix = "expected_loss_"
    ) |> 
    dplyr::select_at(c(facet_var, "exposure_value_usd", "el_type", "el_value"))
  return(data_expected_loss_plot)
}


