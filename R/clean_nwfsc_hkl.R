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
  dir = here::here("data-processed"),
  data,
  species
) {
  cleaned <- data |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(
      lower_name = tolower(common_name),
      Common_name = dplyr::case_when(
        lower_name == "yellowtail rockfish" ~ "yellowtail rockfish south",
        .default = lower_name
      ),
      State = "California",
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

  for (a in 1:nrow(species)) {
    find <- grep(species[a, "name"], cleaned[, "Common_name"])
    cleaned[find, "Common_name"] <- species[a, "use_name"]
  }

  save(cleaned, file = file.path(dir, "nwfsc_hkl_filtered.Rdata"))
  return(cleaned)
}
