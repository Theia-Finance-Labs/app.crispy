box::use(
  shiny[moduleServer, NS, tagList, plotOutput, uiOutput, observeEvent, renderPlot]
)

box::use(
  app/logic/plots/trisk_lineplot[pipeline_trisk_line_plot]
)


####### UI

ui <- function(id) {
  ns <- NS(id)
  tagList(
    semantic.dashboard::box(
      title = "Production Trajectories",
      width = 16,
      collapsible = FALSE,
      # height = "900px",
      plotOutput(ns("trisk_line_plot_output"))
    )
  )
}


####### Server


server <- function(id, trajectories_data_r, max_trisk_granularity) {
  moduleServer(id, function(input, output, session) {
    observeEvent(trajectories_data_r(), ignoreInit = TRUE, {
      granul_levels <- dplyr::intersect(colnames(trajectories_data_r()), names(max_trisk_granularity))
      granul_top_level <- names(max_trisk_granularity[granul_levels])[which.max(unlist(max_trisk_granularity[granul_levels]))]

      # Render plot
      trisk_line_plot <- pipeline_trisk_line_plot(
        trajectories_data = trajectories_data_r(),
        facet_var = granul_top_level
      )

      output$trisk_line_plot_output <- renderPlot(
        {
          trisk_line_plot
        },
        height = function() {
          # Dynamically calculate plot height
          num_facets <- length(unique(trajectories_data_r()[[granul_top_level]]))
          base_height_per_facet <- 200 # TODO GO IN CONF
          total_plot_height <- num_facets * base_height_per_facet
          total_plot_height
        }
      )
    })
  })
}
