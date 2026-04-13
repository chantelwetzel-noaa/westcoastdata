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
clean_cdfw_otolith_files <- function(data, species, year) {
  format_data <- data |>
    tidyr::uncount(CountOffish, .id = "Otolith") |>
    dplyr::mutate(
      Otolith = 1
    ) |>
    dplyr::rename(
      Common_name = species
    ) |>
    dplyr::filter(Year >= year) |>
    dplyr::mutate(
      State = "California",
      Source = "Commercial",
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
