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
clean_or_com_otolith <- function(data, species, year) {
  # Need to pivot longer the data frame first
  data_long <- data |>
    dplyr::select("COMMON", "YEAR", "UNAGED") |>
    tidyr::uncount(UNAGED, .id = "Otolith") |>
    dplyr::mutate(
      Otolith = 1
    ) |>
    dplyr::mutate(COMMON = tolower(COMMON)) |>
    dplyr::rename(
      Common_name = COMMON,
      Year = YEAR
    ) |>
    dplyr::filter(Common_name %in% species[, "name"])

  format_data <- data_long |>
    dplyr::filter(year >= year) |>
    dplyr::mutate(
      State = "Oregon",
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
    ) |>
    dplyr::mutate(
      Common_name = dplyr::case_when(
        Common_name == "pacific spiny dogfish" ~ "Pacific spiny dogfish",
        Common_name == "lingcod" ~ "lingcod north",
        .default = Common_Name
      )
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
