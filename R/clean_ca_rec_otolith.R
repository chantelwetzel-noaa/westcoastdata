#' Format otolith data
#'
#' @param data add definition
#' @param species add definition
#' @param year add definition
#'
#' @author Chantel Wetzel
#' @export
#'
#'
clean_ca_rec_otolith <- function(data, species, year) {
  # Need to pivot longer the data frame first
  data_long <- data |>
    tidyr::pivot_longer(
      cols = 2:ncol(data),
      names_to = "year",
      values_to = "otolith_total"
    ) |>
    dplyr::filter(!is.na(otolith_total)) |>
    tidyr::uncount(otolith_total, .id = "Otolith") |>
    dplyr::mutate(
      Otolith = 1
    ) |>
    dplyr::rename(common_name = species) |>
    dplyr::filter(common_name %in% species)

  format_data <- data_long |>
    dplyr::filter(year >= year) |>
    dplyr::rename(
      Common_name = common_name,
      Year = year
    ) |>
    dplyr::mutate(
      State = "California",
      Source = "Recreational",
      State_Source = paste0(Source, "-", State),
      Fleet = NA,
      set_tow_id = 0,
      Lengthed = 0,
      Aged = 0,
      Weight_kg = NA,
      Length_cm = NA,
      Age = NA,
      Sex = "U",
      age_method = NA
    )

  out <- format_data |>
    dplyr::select(
      Common_name,
      Year,
      State,
      Source,
      State_Source,
      Fleet,
      set_tow_id,
      Lengthed,
      Aged,
      Otolith,
      Weight_kg,
      Length_cm,
      Age,
      Sex,
      age_method
    )

  return(out)
}
