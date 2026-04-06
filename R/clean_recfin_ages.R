#' Clean RecFIN data
#'
#' @param species add definition
#' @param data add definition
#' @param year add definition
#'
#' @author Chantel Wetzel
#' @export
#'
#'
clean_recfin_ages <- function(species, data, year) {
  format_data <- data |>
    dplyr::mutate(
      Common_name = tolower(RECFIN_SPECIES_NAME)
    ) |>
    dplyr::filter(
      Common_name %in% species[, "use_name"],
      SAMPLE_YEAR >= year,
      RECFIN_READ_NUMBER %in% c(0, 1)
    ) |>
    dplyr::mutate(
      State = dplyr::case_when(
        SAMPLING_AGENCY_NAME == "ODFW" ~ "Oregon",
        .default = "Washington"
      ),
      Source = "Recreational",
      State_Source = paste0(Source, "-", State),
      Fleet = RECFIN_MODE_NAME,
      set_tow_id = 0,
      Sex = dplyr::case_when(
        is.na(RECFIN_SEX_CODE) ~ "U",
        .default = RECFIN_SEX_CODE
      ),
      Lengthed = 0,
      Aged = dplyr::case_when(!is.na(USE_THIS_AGE) ~ 1, .default = 0),
      Otolith = dplyr::case_when(Aged == 0 ~ 1, .default = 0),
      Age = dplyr::case_when(
        !is.na(USE_THIS_AGE) ~ USE_THIS_AGE,
        .default = NA
      ),
      Weight_kg = NA,
      Length_cm = NA,
      age_method = NA
    ) |>
    dplyr::rename(
      Year = SAMPLE_YEAR
    )

  # Add yellowtail north & lingcod north
  yt <- which(format_data$Common_name == "yellowtail rockfish")
  format_data$Common_name[yt] <- "yellowtail rockfish north"
  ln <- which(format_data$Common_name == "lingcod")
  format_data$Common_name[ln] <- "lingcod north"

  find <- which(
    format_data$AGE_READABILITY_DESCRIPTION %in%
      c(
        "NOT AGED-PROCESS STORAGE OR COLLECTORS ERROR",
        "NOT AGED-STRUCTURE NOT DISCERNABLE",
        "MINIMUM AGE ONLY GIVEN-DIFFICULT TO ASSIGN AGE"
      )
  )
  format_data[find, c("Otolith", "Aged")] <- 0

  data_out <- format_data |>
    dplyr::select(
      Year,
      Common_name,
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

  return(data_out)
}
