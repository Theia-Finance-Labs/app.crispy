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
    shiny::uiOutput(ns("dynamicHeightBox_pd_term_plot")), # Dynamic UI for the box
    shiny::uiOutput(ns("dynamicHeightBox_expected_loss_plot")) # Dynamic UI for the box
  )

}

####### Server



server <- function(id, analysis_data_r, crispy_data_agg_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    

    base_height_per_facet <- 200 # height in pixels # TODO GO IN CONF


    # PD PLOT

    observeEvent(crispy_data_agg_r(), {
      if (nrow(crispy_data_agg_r()) > 0) {

    granul_levels <- dplyr::intersect(colnames(crispy_data_agg_r()), names(max_trisk_granularity))
    granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]

    num_facets <- length(unique(crispy_data_agg_r()[[granul_top_level]]))


  # Dynamically create the box with adjusted height
  output$dynamicHeightBox_pd_term_plot <- shiny::renderUI({
    # Assuming plot height plus some margin
    calculated_height <- (num_facets * base_height_per_facet) + 50
    semantic.dashboard::box(
      title = "PD Difference", 
      width = 8, 
      collapsible = FALSE,
      style = paste0("height: ", calculated_height, "px;"), # Setting height dynamically
      plotOutput(session$ns("pd_term_plot_output")) # this id value is used in the server, just below
      )
  })


      pd_term_plot <- pipeline_pd_term_plot(
        crispy_data_agg=crispy_data_agg_r(), 
        facet_var=granul_top_level
      )
      # id value dynamically generated in the server, just above
      output$pd_term_plot_output <- shiny::renderPlot({
          pd_term_plot
        },
        height = num_facets * base_height_per_facet
      )
      }
    })




# EXPECTED LOSS PLOT


   observeEvent(analysis_data_r(), {
      if (nrow(analysis_data_r()) > 0) {

        # Then, prepare and render the plot
        granul_levels <- dplyr::intersect(colnames(analysis_data_r()), names(max_trisk_granularity))
        granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]

    num_facets <- length(unique(analysis_data_r()[[granul_top_level]]))

    # Dynamically create the box with adjusted height
    output$dynamicHeightBox_expected_loss_plot <- shiny::renderUI({
      # Assuming plot height plus some margin
      calculated_height <- (num_facets * base_height_per_facet) + 50
      
      # Return box with dynamic height set via inline CSS
      semantic.dashboard::box(
        title = "Expected Loss Baseline and Shock", 
        width = 8, 
        collapsible = FALSE,
        style = paste0("height: ", calculated_height, "px;"), # Setting height dynamically
        plotOutput(session$ns("expected_loss_plot_output")))
    
      })


        expected_loss_plot <- pipeline_expected_loss_plot(
          analysis_data_r(), 
          facet_var = granul_top_level
          )
        output$expected_loss_plot_output <- shiny::renderPlot({
            expected_loss_plot
          },
         height = num_facets * base_height_per_facet
        )



    }
  }

)})}




















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


