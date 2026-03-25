#' Clean and standardize CCFRP data
#'
#' @param data
#' @param species add definition
#' @param year
#'
#' @author Chantel Wetzel
#' @export
#'
#'
clean_ccfrp <- function(species, data, year = 2000) {
  format_data <- dplyr::left_join(
    x = data |> dplyr::mutate(name = tolower(Common_Name)),
    y = tibble::as_tibble(species)
  ) |>
    dplyr::filter(
      Year >= year,
      !is.na(Length_cm),
      # Remove locations that were only sampled a couple of years or less
      !Area %in%
        c("Farallon Islands", "Point Conception", "Trinidad", "Laguna Beach")
    ) |>
    dplyr::rename(Common_name = use_name) |>
    dplyr::mutate(
      State = "California",
      Source = "CCFRP",
      State_Source = paste0(Source, "-", State),
      Fleet = "Hook-and-Line Survey",
      set_tow_id = 0,
      Lengthed = 1,
      Aged = 0,
      Otolith = 0,
      Weight_kg = NA,
      Age = NA,
      Sex = "U",
      age_method = NA
    )

  yellowtail_south <- which(
    format_data$Common_name == "yellowtail rockfish" &
      format_data$Area != "Cape Mendocino"
  )
  format_data$Common_name[yellowtail_south] <- "yellowtail rockfish south"
  yellowtail_north <- which(
    format_data$Common_name == "yellowtail rockfish" &
      format_data$Area == "Cape Mendocino"
  )
  format_data$Common_name[yellowtail_north] <- "yellowtail rockfish north"
  lingcod_south <- which(
    format_data$Common_name == "lingcod" &
      format_data$Area != "Cape Mendocino"
  )
  format_data$Common_name[lingcod_south] <- "lingcod south"
  lingcod_north <- which(
    format_data$Common_name == "lingcod" &
      format_data$Area == "Cape Mendocino"
  )
  format_data$Common_name[lingcod_north] <- "lingcod north"

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
