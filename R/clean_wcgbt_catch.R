#' Clean NWFSC WCGBTS survey catch file
#'
#' @param dir Directory location to save the cleaned data frame
#' @param species A list of species names created by the get_species_list function
#' @param data List of catch and bio data to clean up
#'
#' @author Chantel Wetzel
#' @export
#'
clean_wcgbt_catch <- function(dir = dir, species, data) {
  catch <- data$catch |>
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
        Latitude_dd > 46.25 ~ "Washington",
        Latitude_dd <= 42.0 ~ "California",
        .default = "Oregon"
      ),
      Fleet = NA,
      positive_tow = dplyr::case_when(total_catch_wt_kg > 0 ~ 1, .default = 0),
      set_tow_id = Trawl_id
    )

  wcgbt_catch <- rename_wcgbt_species(data = catch)

  remove <- c(
    which(
      wcgbt_catch$Common_name == "black rockfish" & wcgbt_catch$State == "California"
    ),
    which(
      wcgbt_catch$Common_name == "blue and deacon rockfish" &
        wcgbt_catch$State == "California"
    ),
    which(
      wcgbt_catch$Common_name == "cabezon" &
        wcgbt_catch$State %in% c("California", "Oregon")
    ),
    which(
      wcgbt_catch$Common_name == "China rockfish" & wcgbt_catch$State == "California"
    ),
    which(
      wcgbt_catch$Common_name == "copper rockfish" & wcgbt_catch$State == "California"
    ),
    which(
      wcgbt_catch$Common_name == "quillback rockfish" &
        wcgbt_catch$State == "California"
    ),
    which(
      wcgbt_catch$Common_name == "kelp greenling" &
        wcgbt_catch$State %in% c("California", "Oregon")
    )
  )
  wcgbt_catch <- wcgbt_catch[-remove, ]
  return(wcgbt_catch)
}
