#' Clean RecFIN length data
#'
#'
#' @param or_data add definition
#' @param wa_data add definition
#' @param ca_data add definition
#' @param species add definition
#' @param year add definition
#'
#' @author Chantel Wetzel
#' @export
#'
#'
clean_recfin_lengths <- function(
  data,
  species,
  year
) {
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }

  format_data <- data |>
    dplyr::mutate(
      Common_name = tolower(SPECIES_NAME)
    ) |>
    dplyr::filter(
      Common_name %in% species[, "use_name"],
      RECFIN_YEAR >= year,
      IS_RETAINED == "RETAINED",
      AGENCY_WATER_AREA_NAME %in%
        c(
          "NOT KNOWN", # All Washington records are not know so keeping this across the board
          "OCEAN",
          "OCEAN <= 3 MILES",
          "OCEAN <= 3 MILES (AREAB AND P1B IMPORT)",
          "OCEAN > 3 MILES",
          "OCEAN > 3 MILES (AREAB AND P1B IMPORT)"
        )
    ) |>
    dplyr::mutate(
      State = firstup(tolower(STATE_NAME)),
      Source = "Recreational",
      State_Source = paste0(Source, "-", State),
      Fleet = RECFIN_MODE_NAME,
      set_tow_id = 0,
      Sex = dplyr::case_when(
        !is.na(RECFIN_SEX_CODE) ~ RECFIN_SEX_CODE,
        .default = "U"
      ),
      Length_cm = NA,
      Weight_kg = 0,
      Lengthed = dplyr::case_when(!is.na(AGENCY_LENGTH) ~ 1, .default = 0),
      Age = NA,
      Aged = 0,
      Otolith = 0,
      age_method = NA
    ) |>
    dplyr::rename(
      Year = RECFIN_YEAR
    )

  # Split yellowtail north and south of 40.167
  yellowtail_north <- c(
    which(
      format_data$Common_name == "yellowtail rockfish" &
        format_data$RECFIN_PORT_NAME ==
          "WINE (MENDOCINO COUNTY AND SHELTER COVE AREA IN HUMBOLDT COUNTY)"
    ),
    which(
      format_data$Common_name == "yellowtail rockfish" &
        format_data$State %in% c("Oregon", "Washington")
    )
  )
  format_data$Common_name[yellowtail_north] <- "yellowtail rockfish north"
  yellowtail_south <- which(
    format_data$Common_name == "yellowtail rockfish" &
      format_data$RECFIN_PORT_NAME !=
        "WINE (MENDOCINO COUNTY AND SHELTER COVE AREA IN HUMBOLDT COUNTY)",
    format_data$State == "California"
  )
  format_data$Common_name[yellowtail_south] <- "yellowtail rockfish south"

  data_out <- format_data |>
    dplyr::select(
      Year,
      Common_name,
      State,
      Source,
      State_Source,
      Fleet,
      set_tow_id,
      Weight_kg,
      Lengthed,
      Aged,
      Otolith,
      Length_cm,
      Age
    )

  return(data_out)
}
