plot_indices <- function(data,
                         save_loc = here::here("plots"),
                         file_name = "index.png",
                         legend_loc = "right") {
  if (!"area" %in% colnames(data)) {
    data[["area"]] <- ""
  }
  
  gg <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = year,
      y = est,
      group = area,
      colour = area,
      fill = area
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(lty = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lwr, ymax = upr)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      # If we want legend on the figure
      #   legend.justification = c(0, 1),
      #   legend.direction = "horizontal"
      legend.position = legend_loc,
    ) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Index (mt)") +
    ggplot2::expand_limits(y = 0)
  
  suppressMessages(ggplot2::ggsave(
    plot = gg,
    filename = fs::path(save_loc, file_name),
    width = 10,
    height = 7,
    dpi = 300,
    pointsize = 12
  ))
  
  return(gg)
}