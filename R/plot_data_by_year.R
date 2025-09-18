#' Plot all the data
#'
#'
#' @param dir Directory location to save the compbined data frame
#' @param data add definition
#' @param year add definition
#'
#' @author Chantel Wetzel
#' @export
#'
plot_data_by_year <- function(dir = here::here("plots"), data) {
  year_range <- min(data$Year):max(data$Year)
  data$Source <- as.factor(data$Source)

  cbp1 <- c(
    "#56B4E9",
    "#0072B2",
    "#009E73",
    "#F0E442",
    "#E69F00",
    "#D55E00",
    '#AA3377'
  )

  cbp2 <- c('#CCBB44', '#4477AA', '#66CCEE', '#EE6677', '#228833', '#AA3377')

  cbp3 <- c('#332288', '#88CCEE', '#44AA99', '#117733', '#DDCC77', '#CC6677')

  for (sp in unique(data$Common_name)) {
    lims <- data |>
      dplyr::filter(
        Common_name == sp
      ) |>
      dplyr::summarise(
        min = 0,
        max = plyr::round_any(max(total_lengths), 10, f = ceiling)
      )

    lengths <- ggplot2::ggplot(
      data |>
        dplyr::filter(
          Common_name == sp
        ),
      ggplot2::aes(fill = Source, x = Year, y = total_lengths)
    ) +
      geom_bar(
        aes(y = total_lengths),
        position = "stack",
        stat = "identity",
        color = "#000000"
      ) +
      ggplot2::xlab("Year") +
      ggplot2::ylab("# of Lengths") +
      ggplot2::ylim(as.numeric(lims[1]), as.numeric(lims[2])) +
      ggplot2::facet_wrap("State", ncol = 3) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(unique(sp)) +
      theme(
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.background = element_rect(colour = "black", fill = "#FFFFFF")
      ) +
      #ggplot2::scale_fill_viridis_d(drop = FALSE)
      #ggplot2::scale_fill_brewer(palette = "Set2", drop = FALSE)
      ggplot2::scale_fill_manual(values = cbp1, drop = FALSE)
    #ggplot2::scale_fill_manual(values = PNWColors::pnw_palette("Bay", n = 6), drop = FALSE)

    lims <- data |>
      dplyr::filter(
        Common_name == sp
      ) |>
      dplyr::summarise(
        min = 0,
        max = plyr::round_any(max(total_ages, total_otoliths), 10, f = ceiling)
      )

    ages <- ggplot2::ggplot(
      data |>
        dplyr::filter(
          Common_name == sp
        ),
      aes(fill = Source, y = total_ages, x = Year)
    ) +
      geom_bar(
        aes(y = total_ages),
        position = "stack",
        stat = "identity",
        color = "#000000"
      ) +
      xlab("Year") +
      ylab("# of Ages") +
      ggplot2::theme_bw() +
      ggplot2::ylim(as.numeric(lims[1]), as.numeric(lims[2])) +
      ggplot2::theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.background = element_rect(colour = "black", fill = "#FFFFFF")
      ) +
      facet_wrap("State", ncol = 3) +
      ggplot2::scale_fill_manual(values = cbp1, drop = FALSE)
    #scale_fill_viridis_d(drop = FALSE)

    otoliths <- ggplot2::ggplot(
      data |>
        dplyr::filter(
          Common_name == sp
        ),
      aes(fill = Source, y = total_otoliths, x = Year)
    ) +
      geom_bar(
        aes(y = total_otoliths),
        position = "stack",
        stat = "identity",
        color = "#000000"
      ) +
      xlab("Year") +
      ylab("# of Age Structures") +
      ggplot2::theme_bw() +
      ggplot2::ylim(as.numeric(lims[1]), as.numeric(lims[2])) +
      ggplot2::theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.background = element_rect(colour = "black", fill = "#FFFFFF")
      ) +
      ggplot2::facet_wrap("State", ncol = 3) +
      ggplot2::scale_fill_manual(values = cbp1, drop = FALSE)
    #ggplot2::scale_fill_viridis_d(drop = FALSE)

    cowplot::plot_grid(lengths, ages, otoliths, ncol = 1, nrow = 3)
    ggsave(
      file.path(dir, paste0(sp, "_state_compositions.png")),
      height = 12,
      width = 12,
      dpi = 100
    )
  }
}
