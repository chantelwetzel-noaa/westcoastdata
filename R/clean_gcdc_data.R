#' Clean and standardize CCFRP data
#'
#' @param data
#' @param species add definition
#' @param year
#'
#' @author Chantel Wetzel
#' @export
#'
#'
clean_gcdc_data <- function(data, species, year = 2000) {
  format_data <- data |>
    dplyr::filter(year >= year) |>
    dplyr::rename(
      Common_name = common_name,
      Year = year
    ) |>
    tidyr::uncount(otolith, .id = "Otolith") |>
    dplyr::mutate(
      State = "California",
      Source = "GCDC",
      State_Source = paste0(Source, "-", State),
      Fleet = NA,
      set_tow_id = 0,
      Lengthed = 1,
      Aged = 0,
      Otolith = 1,
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
