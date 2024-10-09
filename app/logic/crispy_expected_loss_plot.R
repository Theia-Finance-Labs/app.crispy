

#' Financial Risk Visualization via Expected Loss and Exposure Plot
#'
#' Generates a plot that visualizes financial risk by showcasing expected losses and exposure across different segments. It preprocesses data for visual representation and uses faceting to provide insights into risk distribution across specified categories, aiding in targeted risk mitigation strategies.
#'
#' @param analysis_data Dataframe with financial exposure and expected loss data, segmented by various categories.
#' @param facet_var Categorical variable for segmenting the data, enabling detailed risk analysis across segments.
#'
#' @return A ggplot object displaying financial risks segmented by `facet_var`, crucial for risk management decisions.
#' @export
pipeline_crispy_expected_loss_plot <- function(
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

#' Data Preparation for Financial Risk Visualization
#'
#' Prepares dataset for plotting by transforming financial risk data, including expected losses and exposure values, into a format that allows for aggregated analysis across specified segments. Essential for highlighting financial vulnerabilities and focusing risk management efforts.
#'
#' @param analysis_data Dataset including detailed financial risk metrics, to be transformed for visualization.
#' @param facet_var Segmentation variable used to categorize and analyze financial risk across different divisions.
#'
#' @return Dataframe optimized for visualizing financial risk, with aggregated metrics for each segment.
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
      el_value = sum(.data$el_value, na.rm = T),
      exposure_value_usd = sum(.data$exposure_value_usd, na.rm = T)
    ) |>
    dplyr::select_at(c(facet_var, "exposure_value_usd", "el_type", "el_value"))
  return(data_expected_loss_plot)
}

#' Generate Exposure Change and Value Loss Plot
#'
#' Constructs the final plot visualizing sector/category-wise changes in financial exposure and value losses, using a combination of bar and tile geoms to represent data points and their positive/negative changes. Faceting can be applied for more detailed analysis. This function integrates aesthetic elements and scales to effectively communicate the financial impact.
#'
#' @param data_exposure_change Prepared dataframe for plotting.
#' @param x_var Category or sector variable for the x-axis.
#' @param y_exposure_var Metric for exposure value.
#' @param y_value_loss_var Metric for crispy value loss.
#' @param facet_var Optional; variable to facet the plot by.
#'
#' @return A ggplot object depicting exposure changes and value losses, crucial for detailed financial impact analysis.
draw_exposure_change_plot <- function(
    data_exposure_change,
    x_var,
    y_exposure_var,
    y_value_loss_var,
    facet_var = NULL) {
  plot_bar_color <-
    r2dii.colours::palette_1in1000_plot |>
    dplyr::filter(.data$label == "grey") |>
    dplyr::pull(.data$hex)

  # HARDCODED PARAMETERS
  plot_color_gradient <- c(
    r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "red") |> dplyr::pull(.data$hex),
    r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "green") |> dplyr::pull(.data$hex)
  )
  bar_width <- 0.9 # Adjust as needed TODO variabiliser conf

  # PLOTTING

  exposure_change_plot <- ggplot2::ggplot(data_exposure_change, ggplot2::aes(x = !!rlang::sym(x_var))) +
    ggplot2::geom_col(
      ggplot2::aes(y = !!rlang::sym(y_exposure_var)),
      width = bar_width, fill = plot_bar_color
    ) +
    ggplot2::geom_tile(
      ggplot2::aes(
        y = !!rlang::sym(y_exposure_var) + (!!rlang::sym(y_value_loss_var) / 2),
        height = abs(!!rlang::sym(y_value_loss_var)),
        fill = dplyr::if_else(!!rlang::sym(y_value_loss_var) < 0, "Loss", "Gain")
      ),
      width = bar_width
    ) +
    ggplot2::scale_fill_manual(
      name = "Crispy value change",
      values = c(plot_color_gradient[1], plot_color_gradient[2]),
      breaks = c("Loss", "Gain")
    ) +
    ggplot2::labs(y = "Value USD", x = "") +
    r2dii.plot::theme_2dii() +
    ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    ggplot2::theme(
      # legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(title = "Estimated impact of the Shock on Exposure")

  if (!is.null(facet_var)) {
    exposure_change_plot <- exposure_change_plot +
      ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)), scales = "free_y", ncol = 1)
  }

  return(exposure_change_plot)
}
