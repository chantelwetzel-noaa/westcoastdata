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
        Latitude_dd > 46.25 ~ "WA",
        Latitude_dd <= 42.0 ~ "CA",
        .default = "OR"
      ),
      Fleet = NA,
      positive_tow = dplyr::case_when(total_catch_wt_kg > 0 ~ 1, .default = 0),
      set_tow_id = Trawl_id
    )

  wcgbt_catch <- rename_wcgbt_species(data = catch)

  remove <- c(
    which(
      wcgbt_catch$Common_name == "black rockfish" & wcgbt_catch$State == "CA"
    ),
    which(
      wcgbt_catch$Common_name == "blue and deacon rockfish" &
        wcgbt_catch$State == "CA"
    ),
    which(
      wcgbt_catch$Common_name == "cabezon" &
        wcgbt_catch$State %in% c("CA", "OR")
    ),
    which(
      wcgbt_catch$Common_name == "China rockfish" & wcgbt_catch$State == "CA"
    ),
    which(
      wcgbt_catch$Common_name == "copper rockfish" & wcgbt_catch$State == "CA"
    ),
    which(
      wcgbt_catch$Common_name == "quillback rockfish" &
        wcgbt_catch$State == "CA"
    ),
    which(
      wcgbt_catch$Common_name == "kelp greenling" &
        wcgbt_catch$State %in% c("CA", "OR")
    )
  )
  wcgbt_catch <- wcgbt_catch[-remove, ]

  catch_areas <- wcgbt_catch |>
    dplyr::filter(positive_tow == 1) |>
    dplyr::group_by(Common_name) |>
    dplyr::summarise(
      min_depth = min(Depth_m),
      min_depth_01 = quantile(Depth_m, 0.01),
      max_depth = max(Depth_m),
      max_depth_99 = quantile(Depth_m, 0.99),
      min_lat = min(Latitude_dd),
      min_lat_01 = quantile(Latitude_dd, 0.01),
      max_lat = max(Latitude_dd),
      max_lat_99 = quantile(Latitude_dd, 0.99)
    )

  utils::write.csv(
    catch_areas,
    here::here(dir, "wcgbt_catch_areas.csv"),
    row.names = FALSE
  )
  save(wcgbt_catch, file = file.path(dir, "wcgbt_catch_filtered.Rdata"))
  return(wcgbt_catch)
}
