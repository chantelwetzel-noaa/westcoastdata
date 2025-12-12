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
    dplyr::filter(
      !Common_name %in% c("vermilion rockfish", "blue / deacon rockfish")
    ) |>
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
      Fleet = NA,
      Common_name = dplyr::case_when(
        Common_name == "tree rockish" ~ "treefish",
        Common_name %in% c("rougheye rockfish", "blackspotted rockfish") ~
          "rougheye and blackspotted rockfish",
        Common_name %in%
          c("blue / deacon rockfish", "blue rockfish", "deacon rockfish") ~
          "blue and deacon rockfish",
        Common_name %in% c("vermilion rockfish", "Sunset rockfish") ~
          "vermilion and sunset rockfish",
        Common_name == "gopher rockfish" ~
          "gopher and black and yellow rockfish",
        Common_name == "yellowtail rockfish" & Latitude_dd >= 40.167 ~
          "yellowtail rockfish north",
        Common_name == "yellowtail rockfish" & Latitude_dd < 40.167 ~
          "yellowtail rockfish south",
        .default = Common_name
      ),
      Sex = nwfscSurvey::codify_sex(Sex),
      Lengthed = dplyr::case_when(!is.na(Length_cm) ~ 1, .default = 0),
      Aged = dplyr::case_when(!is.na(Age_years) ~ 1, .default = 0),
      Otolith = dplyr::case_when(
        !is.na(Otosag_id) & is.na(Age_years) ~ 1,
        .default = 0
      )
    ) |>
    dplyr::filter(Common_name %in% species[, "use_name"])
  
  wcgbt_bio <- bio
  
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
    which(wcgbt_bio$Common_name == "kelp greenling" & wcgbt_bio$State == "CA")
  )
  wcgbt_bio <- wcgbt_bio[-remove, ]
  
  bio <- wcgbt_bio
  
  save(bio, file = file.path(dir, "wcgbt_bio_filtered.Rdata"))
  return(bio)
}
