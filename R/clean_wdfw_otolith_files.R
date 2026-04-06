#' Format otolith data
#'
#' @param data add definition
#' @param source add definition
#' @param state add definition
#'
#' @author Chantel Wetzel
#' @export
#'
#'
clean_wdfw_otolith_files <- function(data, species, year) {
  format_data <- dplyr::left_join(
    x = data |>
      dplyr::mutate(
        name = tolower(species_name)
      ),
    y = tibble::as_tibble(species),
    by = "name"
  ) |>
    dplyr::filter(sample_year >= year) |>
    dplyr::rename(
      Common_name = use_name,
      Year = sample_year,
      Otolith = Unaged_structure
    ) |>
    dplyr::mutate(
      Common_name = dplyr::case_when(
        Common_name == "yellowtail rockfish" ~ "yellowtail rockfish north",
        Common_name == "lingcod" ~ "lingcod north",
        .default = Common_name
      ),
      State = "Washington",
      Source = dplyr::case_when(
        data_type_name == "Sport" ~ "Recreational",
        .default = data_type_name
      ),
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
      Sex,
      Weight_kg,
      Length_cm,
      Age,
      age_method
    )

  return(out)
}
