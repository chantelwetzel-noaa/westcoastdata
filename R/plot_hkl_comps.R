#' Plot NWFSC HKL data
#'
#' @param dir add definition
#' @param data add definition
#'
#' @author Chantel Wetzel
#' @export
#'
#'
plot_hkl_comps <- function(dir = here::here(), data) {
  hkl_species <- get_hkl_species()
  species_to_plot <- unique(data[
    which(data$common_name %in% tolower(hkl_species)),
    "Common_name"
  ])

  # One random gigantic and small bocaccio
  data <- data[which(data$Length_cm < 150 & data$Length_cm >= 10), ]
  data$Length_cm <- floor(data$Length_cm)

  for (sp in species_to_plot) {
    tmp <- data[which(data$Common_name == sp), ]
    ind <- !is.na(tmp$length_cm)
    min_len <- ifelse(
      floor(min(tmp$Length_cm[ind])) > 10,
      floor(min(tmp$Length_cm[ind])),
      10
    )
    max_len <- floor(max(tmp$Length_cm[ind]))
    bin_size <- 2 #ifelse(max_len - min_len > 60, 4, 2)
    len.bins <- seq(min_len, max_len - 2 * bin_size, bin_size)

    tmp <- tmp[, colnames(tmp) != "sex"]
    tmp$Sex <- "U"

    lfs <- process_comps_unexpanded(
      datL = tmp,
      lgthBins = len.bins,
      partition = 0,
      fleet = 1,
      month = 7,
      two_sex_model = FALSE
    )

    lfs <- as.data.frame(lfs$unsexed)

    plot_comps(
      dir = dir,
      add_0_ylim = FALSE,
      add_save_name = paste0(sp, "_nwfsc_hkl"),
      data = lfs,
      plot = 1
    )
  }
}
