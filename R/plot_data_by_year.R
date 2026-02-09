#' Plot all the data
#'
#'
#' @param dir Directory location to save the combined data frame
#' @param data add definition
#' @param year add definition
#'
#' @author Chantel Wetzel
#' @export
#'
plot_data_by_year <- function(
  dir = here::here("plots", "state_comparisons"),
  data
) {
  year_range <- min(data$Year):max(data$Year)
  data$Source <- as.factor(data$Source)

  cbp1 <- c(
    '#56B4E9',
    '#0072B2',
    '#009E73',
    '#F0E442',
    '#E69F00',
    '#D55E00',
    '#AA3377'
  )
  cbp2 <- c('#CCBB44', '#4477AA', '#66CCEE', '#EE6677', '#228833', '#AA3377')
  cbp3 <- c('#332288', '#88CCEE', '#44AA99', '#117733', '#DDCC77', '#CC6677')
  #for just surveys
  cbp4 <- c('#AA3377', '#66CCEE')

  for (sp in unique(data$Common_name)) {
    lims <- data |>
      dplyr::filter(
        Common_name == sp
      ) |>
      dplyr::group_by(Year, State) |>
      dplyr::summarise(
        n_length = sum(total_lengths)
      ) |>
      dplyr::ungroup() |>
      dplyr::summarise(
        min = 0,
        max = plyr::round_any(max(n_length), 10, f = ceiling)
      )

    lengths <- ggplot2::ggplot(
      data |>
        dplyr::filter(
          Common_name == sp
        ),
      ggplot2::aes(fill = Source, x = Year, y = total_lengths)
    ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = total_lengths),
        position = "stack",
        stat = "identity",
        color = "#000000",
        show.legend = TRUE
      ) +
      ggplot2::xlab("Year") +
      ggplot2::ylab("# of Lengths") +
      ggplot2::ylim(as.numeric(lims[1]), as.numeric(lims[2])) +
      ggplot2::facet_wrap("State", ncol = 3) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(unique(sp)) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 20, hjust = 0.5),
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 14),
        strip.text.x = ggplot2::element_text(size = 14),
        legend.text = ggplot2::element_text(size = 14),
        strip.background = ggplot2::element_rect(colour = "black", fill = "#FFFFFF")
      ) +
      ggplot2::scale_fill_manual(values = cbp4, drop = FALSE)

    lims <- data |>
      dplyr::filter(
        Common_name == sp
      ) |>
      dplyr::group_by(Year, State) |>
      dplyr::summarise(
        n_ages = sum(total_ages),
        n_otoliths = sum(total_otoliths)
      ) |>
      dplyr::ungroup() |>
      dplyr::summarise(
        min = 0,
        max = plyr::round_any(max(n_ages, n_otoliths), 10, f = ceiling)
      )

    ages <- ggplot2::ggplot(
      data |>
        dplyr::filter(
          Common_name == sp
        ),
      ggplot2::aes(fill = Source, y = total_ages, x = Year)
    ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = total_ages),
        position = "stack",
        stat = "identity",
        color = "#000000",
        show.legend = TRUE
      ) +
      ggplot2::xlab("Year") +
      ggplot2::ylab("# of Ages") +
      ggplot2::theme_bw() +
      ggplot2::ylim(as.numeric(lims[1]), as.numeric(lims[2])) +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 14),
        strip.text.x = ggplot2::element_text(size = 14),
        legend.text = ggplot2::element_text(size = 14),
        strip.background = ggplot2::element_rect(colour = "black", fill = "#FFFFFF")
      ) +
      ggplot2::facet_wrap("State", ncol = 3) +
      ggplot2::scale_fill_manual(values = cbp4, drop = FALSE)

    otoliths <- ggplot2::ggplot(
      data |>
        dplyr::filter(
          Common_name == sp
        ),
      ggplot2::aes(fill = Source, y = total_otoliths, x = Year)
    ) +
      ggplot2::geom_bar(
        ggplot2::aes(y = total_otoliths),
        position = "stack",
        stat = "identity",
        color = "#000000",
        show.legend = TRUE
      ) +
      ggplot2::xlab("Year") +
      ggplot2::ylab("# of Age Structures") +
      ggplot2::theme_bw() +
      ggplot2::ylim(as.numeric(lims[1]), as.numeric(lims[2])) +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 14),
        strip.text.x = ggplot2::element_text(size = 14),
        legend.text = ggplot2::element_text(size = 14),
        strip.background = ggplot2::element_rect(colour = "black", fill = "#FFFFFF")
      ) +
      ggplot2::facet_wrap("State", ncol = 3) +
      ggplot2::scale_fill_manual(values = cbp4, drop = FALSE)

    cowplot::plot_grid(lengths, ages, otoliths, ncol = 1, nrow = 3)
    ggplot2::ggsave(
      file.path(
        dir,
        paste0(
          stringr::str_replace_all(tolower(sp), "[^a-z0-9]+", "_"),
          "_state_compositions.png"
        )
      ),
      height = 12,
      width = 12,
      dpi = 100
    )
  }
  return(NULL)
}
