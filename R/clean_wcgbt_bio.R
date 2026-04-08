#' Clean NWFSC WCGBTS biological data
#'
#' @param dir Directory location to save the cleaned data frame
#' @param species A list of species names created by the get_species_list function
#' @param data List of catch and bio data to clean up
#'
#' @author Chantel Wetzel
#' @export
#'
clean_wcgbt_bio <- function(dir = dir, species, data) {
  bio <- data$bio |>
    dplyr::filter(Common_name %in% species[, "name"]) |>
    dplyr::mutate(
      Source = "NWFSC WCGBTS",
      State_area = dplyr::case_when(
        Latitude_dd > 46.25 ~ "WA",
        Latitude_dd > 42.0 & Latitude_dd <= 46.25 ~ "OR",
        Latitude_dd > 40.167 & Latitude_dd <= 42.0 ~ "NCA",
        Latitude_dd > 34.47 & Latitude_dd <= 40.167 ~ "CCA",
        .default = "SCA"
      ),
      State = dplyr::case_when(
        Latitude_dd > 46.25 ~ "WA",
        Latitude_dd <= 42.0 ~ "CA",
        .default = "OR"
      ),
      Fleet = "NA",
      Sex = nwfscSurvey::codify_sex(Sex),
      Lengthed = dplyr::case_when(!is.na(Length_cm) ~ 1, .default = 0),
      Aged = dplyr::case_when(!is.na(Age_years) ~ 1, .default = 0),
      Otolith = dplyr::case_when(
        !is.na(Otosag_id) & is.na(Age_years) ~ 1,
        .default = 0
      ),
      set_tow_id = Trawl_id
    )

  wcgbt_bio <- rename_wcgbt_species(data = bio)

  remove <- c(
    which(wcgbt_bio$Common_name == "black rockfish" & wcgbt_bio$State == "CA"),
    which(
      wcgbt_bio$Common_name == "blue and deacon rockfish" &
        wcgbt_bio$State == "CA"
    ),
    which(
      wcgbt_bio$Common_name == "cabezon" & wcgbt_bio$State %in% c("CA", "OR")
    ),
    which(wcgbt_bio$Common_name == "China rockfish" & wcgbt_bio$State == "CA"),
    which(wcgbt_bio$Common_name == "copper rockfish" & wcgbt_bio$State == "CA"),
    which(
      wcgbt_bio$Common_name == "quillback rockfish" & wcgbt_bio$State == "CA"
    ),
    which(
      wcgbt_bio$Common_name == "kelp greenling" &
        wcgbt_bio$State %in% c("CA", "OR")
    )
  )
  wcgbt_bio <- wcgbt_bio[-remove, ]
  return(wcgbt_bio)
}
