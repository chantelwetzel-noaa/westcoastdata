#' Clean NWFSC WCGBTS catch and biological data
#'
#' @param data Dataframe of WCGBT data
#'
#' @author Chantel Wetzel
#' @export
#'
rename_wcgbt_species <- function(data) {
  renamed_data <- data |>
    dplyr::mutate(
      formatted_name = dplyr::case_when(
        Common_name %in%
          c(
            "blue rockfish",
            "deacon rockfish",
            "blue/deacon rockfish",
            "blue / deacon rockfish"
          ) ~
          "blue and deacon rockfish",
        Common_name %in% c("blackspotted rockfish", "rougheye rockfish") ~
          "rougheye and blackspotted rockfish",
        Common_name %in% c("vermilion rockfish", "sunset rockfish") ~
          "vermilion and sunset rockfish",
        Common_name == "lingcod" & Latitude_dd >= 40.167 ~ "lingcod north",
        Common_name == "lingcod" & Latitude_dd < 40.167 ~ "lingcod south",
        Common_name == "yellowtail rockfish" & Latitude_dd >= 40.167 ~
          "yellowtail rockfish north",
        Common_name == "yellowtail rockfish" & Latitude_dd < 40.167 ~
          "yellowtail rockfish south",
        .default = Common_name
      )
    ) |>
    dplyr::select(-Common_name) |>
    dplyr::rename(Common_name = formatted_name)
  return(renamed_data)
}
