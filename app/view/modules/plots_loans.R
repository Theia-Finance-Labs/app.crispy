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
    semantic.dashboard::box(
      title = "PD Difference", width = 8, collapsible = FALSE,
      plotOutput(ns("pd_term_plot_output"), height = "100%")
    ),
    semantic.dashboard::box(
      title = "Exposure Change", width = 8, collapsible = FALSE,
      plotOutput(ns("expected_loss_plot_output"), height = "100%")
    )
  )
}

####### Server



server <- function(id, analysis_data_r, crispy_data_agg_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    base_height_per_facet <- 150 # height in pixels # TODO GO IN CONF

    # PD PLOT

    observeEvent(c(crispy_data_agg_r(), analysis_data_r()), {
      if (nrow(crispy_data_agg_r()) > 0) {
        granul_levels <- dplyr::intersect(colnames(crispy_data_agg_r()), names(max_trisk_granularity))
        granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]

        num_facets <- length(unique(crispy_data_agg_r()[[granul_top_level]]))


        # crispy_data_agg_user_filtered <- crispy_data_agg_r() |>
        #   dplyr::inner_join(
        #     analysis_data_r() |> dplyr::distinct_at(granul_top_level)
        #   )


        pd_term_plot <- pipeline_pd_term_plot(
          crispy_data_agg = crispy_data_agg_r(),
          facet_var = granul_top_level
        )
        # id value dynamically generated in the server, just above
        output$pd_term_plot_output <- shiny::renderPlot(
          {
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

        analysis_data_all_granul_levels <- analysis_data_r() |> 
            dplyr::right_join(crispy_data_agg_r() |> dplyr::distinct_at(granul_top_level))

        num_facets <- length(unique(crispy_data_agg_r()[[granul_top_level]]))

        expected_loss_plot <- pipeline_expected_loss_plot(
          analysis_data=analysis_data_all_granul_levels,
          facet_var = granul_top_level
        )
        
        output$expected_loss_plot_output <- shiny::renderPlot(
          {
            expected_loss_plot
          },
          height = num_facets * base_height_per_facet
        )
      }
    })
  })
}




















pipeline_expected_loss_plot <- function(
    analysis_data,
    facet_var) {
  data_expected_loss_plot <- prepare_for_expected_loss_plot(
    analysis_data = analysis_data,
    facet_var = facet_var
  )

  expected_loss_plot <- draw_exposure_change_plot(
    data_expected_loss_plot,
    x_var = "el_type",
    y_exposure_var = "exposure_value_usd",
    y_value_loss_var = "el_value",
    facet_var = facet_var
  )
  return(expected_loss_plot)
}

prepare_for_expected_loss_plot <- function(analysis_data, facet_var) {
  data_expected_loss_plot <- analysis_data |>
    tidyr::pivot_longer(
      cols = tidyr::starts_with("expected_loss_"),
      names_to = "el_type",
      values_to = "el_value",
      names_prefix = "expected_loss_"
    ) |>
    dplyr::filter(!.data$el_type %in% c("difference", "portfolio")) |>
    dplyr::group_by_at(c(facet_var, "el_type")) |>
    dplyr::summarise(
      el_value=sum(.data$el_value, na.rm=T),
      exposure_value_usd=sum(.data$exposure_value_usd, na.rm=T)
      ) |>
    dplyr::select_at(c(facet_var, "exposure_value_usd", "el_type", "el_value"))
  return(data_expected_loss_plot)
}
