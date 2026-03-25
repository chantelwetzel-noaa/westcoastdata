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
clean_ccfrp_otolith <- function(data, species, year) {
  format_data <- data |>
    dplyr::filter(year >= year) |>
    dplyr::rename(
      Common_name = common_name,
      Year = year
    ) |>
    tidyr::uncount(otoliths, .id = "Otolith") |>
    dplyr::mutate(
      State = "California",
      Source = "CCFRP",
      State_Source = paste0(Source, "-", State),
      Fleet = "Hook-and-Line Survey",
      set_tow_id = 0,
      Lengthed = 0,
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
