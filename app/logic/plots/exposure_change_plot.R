box::use(
  dplyr[select_at],
  ggplot2[
    ggplot, aes, geom_col, geom_tile, scale_fill_gradient2, labs, theme, element_text,
    scale_x_discrete, scale_y_continuous
  ]
)

pipeline_exposure_change_plot <- function(
    analysis_data,
    x_var = "portfolio.ald_sector",
    y_exposure_var = "portfolio.exposure_value_usd",
    y_value_loss_var = "crispy_value_loss",
    fill_var = "crispy_perc_value_change") {
  plot_bar_color <- r2dii.colours::palette_1in1000_plot |>
    dplyr::filter(.data$label == "grey") |>
    dplyr::pull(.data$hex)
  plot_color_gradient <- c(
    r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "red") |> dplyr::pull(.data$hex),
    r2dii.colours::palette_1in1000_plot |> dplyr::filter(.data$label == "green") |> dplyr::pull(.data$hex)
  )


  data_exposure_change <- prepare_for_exposure_change_plot(analysis_data, x_var, y_exposure_var, y_value_loss_var, fill_var)

  exposure_change_plot <- draw_exposure_change_plot(
    data_exposure_change,
    x_var, y_exposure_var,
    y_value_loss_var,
    plot_bar_color, plot_color_gradient
  )

  return(exposure_change_plot)
}

prepare_for_exposure_change_plot <- function(analysis_data, x_var, y_exposure_var, y_value_loss_var, fill_var) {
  data_exposure_change <- analysis_data |>
    select_at(
      c(x_var, y_exposure_var, y_value_loss_var, fill_var)
    )
  return(data_exposure_change)
}


draw_exposure_change_plot <- function(
    data_exposure_change,
    x_var,
    y_exposure_var,
    y_value_loss_var,
    plot_bar_color, plot_color_gradient) {
  bar_width <- 0.9 # Adjust as needed TODO variabiliser conf

  exposure_change_plot <- ggplot(data_exposure_change, aes(x = !!rlang::sym(x_var))) +
    geom_col(aes(y = !!rlang::sym(y_exposure_var)), width = bar_width, fill = plot_bar_color) +
    geom_tile(
      aes(
        y = !!rlang::sym(y_exposure_var) + (!!rlang::sym(y_value_loss_var) / 2),
        height = abs(!!rlang::sym(y_value_loss_var)),
        fill = !!rlang::sym(y_value_loss_var)
      ),
      width = bar_width
    ) +
    scale_fill_gradient2(
      low = plot_color_gradient[1],
      high = plot_color_gradient[2],
      midpoint = 0,
      name = "Crispy value change %",
      labels = scales::unit_format(unit = "M", scale = 1e-6)
    ) +
    labs(y = "Value USD", x = "Sector") +
    r2dii.plot::theme_2dii() +
    # scale_x_discrete(position = "bottom", labels = r2dii.plot::to_title) +
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    theme(legend.title = element_text())

  return(exposure_change_plot)
}
