#' Plot NWFSC HKL data
#'
#' @param dir add definition
#' @param data add definition
#'
#' @author Chantel Wetzel
#' @export
#'
#'
plot_hkl_comps <- function(
  dir = here::here("plots", "hkl_comps"),
  data
) {
  total_obs <- data |>
    dplyr::group_by(Common_name) |>
    dplyr::summarise(
      n = dplyr::n()
    ) |>
    dplyr::filter(n >= 500) |>
    dplyr::ungroup()
  species_to_plot <- total_obs[["Common_name"]]

  # One random small 0.14 fish
  filtered_data <- data[which(data$Length_cm >= 10), ]
  if ("common_name" %in% colnames(filtered_data)) {
    filtered_data <- filtered_data |>
      dplyr::select(-common_name)
  }
  if (!"project" %in% colnames(filtered_data)) {
    filtered_data$project <- "NWFSC_HKL"
  }
  for (sp in species_to_plot) {
    tmp <- filtered_data[which(filtered_data$Common_name == sp), ]
    ind <- !is.na(tmp$Length_cm)
    min_len <- ifelse(
      floor(min(tmp$Length_cm[ind])) > 10,
      floor(min(tmp$Length_cm[ind])),
      10
    )
    max_len <- max(tmp$Length_cm[ind])
    bin_size <- 2 #ifelse(max_len - min_len > 60, 4, 2)
    len_bins <- seq(min_len, max_len - 2 * bin_size, bin_size)

    lfs <- nwfscSurvey::get_raw_comps(
      data = tmp,
      comp_bins = len_bins,
      input_n_method = "total_samples",
      partition = 0,
      fleet = 1,
      month = 7,
      two_sex_comps = FALSE
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
