#' Function to remove all unneeded species data and to format column names as needed for the NWFS HKL data.
#'
#' @param dir Directory location to save the cleaned data frame
#' @param species A list of species names created by the get_species_list function
#' @param data Data frame of NWFSC HKL data
#'
#' @author Chantel Wetzel
#' @export
#'
#'
clean_nwfsc_hkl <- function(
  dir = dir,
  data,
  species
) {
  formatted <- data |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(
      lower_name = tolower(common_name),
      Common_name = dplyr::case_when(
        lower_name == "yellowtail rockfish" ~ "yellowtail rockfish south",
        lower_name == "blackspotted rockfish" ~
          "rougheye and blackspotted rockfish",
        lower_name == "blue rockfish" ~ "blue and deacon rockfish",
        lower_name == "spiny dogfish" ~ "Pacific spiny dogfish",
        lower_name == "vermilion rockfish" ~ "vermilion and sunset rockfish",
        lower_name == "california scorpionfish" ~ "California scorpionfish",
        lower_name == "lingcod" ~ "lingcod south",
        .default = lower_name
      ),
      State = "CA",
      Source = "NWFSC HKL",
      Fleet = NA,
      Lengthed = dplyr::case_when(!is.na(length_cm) ~ 1, .default = 0),
      Aged = dplyr::case_when(!is.na(age_years) ~ 1, .default = 0),
      Otolith = dplyr::case_when(
        otolith_number != "" & is.na(age_years) ~ 1,
        .default = 0
      )
    ) |>
    dplyr::rename(
      set_tow_id = set_id,
      Year = year,
      Length_cm = length_cm,
      Age = age_years,
      Weight_kg = weight_kg,
      Sex = sex
    ) |>
    dplyr::filter(Common_name %in% species[, "name"])

  #NOTE: yellowtail rockfish south is only in column Common_name, not lower_name or common_name

  cleaned <- formatted |>
    dplyr::filter(
      !Common_name %in%
        c(
          "black rockfish",
          "copper rockfish",
          "cabezon",
          "china rockfish",
          "kelp greenling",
          "blue and deacon rockfish",
          "quillback rockfish"
        )
    )

  save(
    cleaned,
    file = file.path("data-processed", "2026", "nwfsc_hkl_filtered.Rdata")
  )
  return(cleaned)
}
