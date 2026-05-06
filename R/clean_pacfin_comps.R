#' Clean pulled PacFIN data
#'
#' @param bds_pacfin Dataframe of PacFIN data
#' @param species list of species name to retain
#' @param spid_key csv file with PacFIN species codes and full species name
#' @param year Integer to filter PacFIN data to retain all data from this year
#'   and beyond
#'
#' @author Chantel Wetzel
#' @export
#'
#'
clean_pacfin_comps <- function(
  bds_pacfin,
  species,
  spid_key,
  year = 1980
) {
  Pdata <- pacfintools::cleanPacFIN(
    Pdata = bds_pacfin |>
      dplyr::filter(SAMPLE_YEAR >= year),
    clean = TRUE,
    keep_age_method = c("B", "BB", "S", "T", ""),
    verbose = FALSE
  )

  data <- dplyr::left_join(
    Pdata,
    spid_key |>
      dplyr::rename(PACFIN_SPECIES_CODE = pacfin.code, Common_name = species)
  ) |>
    dplyr::filter(
      Common_name %in% species[, "use_name"]
    ) |>
    dplyr::mutate(
      State = dplyr::case_when(
        state == "OR" ~ "Oregon",
        state == "WA" ~ "Washington",
        .default = "California"
      ),
      Source = "Commercial",
      Fleet = dplyr::case_when(
        fleet %in% c("TWL", "TWS") ~ "Trawl",
        .default = "Non-trawl"
      ),
      State_Source = paste0(Source, "-", State),
      set_tow_id = 0,
      Weight_kg = dplyr::case_when(!is.na(weightkg) ~ 1, .default = 0),
      Lengthed = dplyr::case_when(!is.na(lengthcm) ~ 1, .default = 0),
      Aged = dplyr::case_when(
        !is.na(Age) ~ 1,
        .default = 0
      ),
      Otolith = 0
    ) |>
    dplyr::rename(
      Year = SAMPLE_YEAR,
      Sex = SEX_CODE,
      Length_cm = lengthcm
    )
  # AGE_STRUCTURE_CODE values:
  # L = length only
  # O = otolith
  # F = fin ray
  # SP = spine

  # Comment out since this information no longer gets us correct counts
  # AGE_STRUCTURE_DESC1
  # find <- c(
  #   which(
  #     data$AGE_STRUCTURE_CODE1 %in%
  #       c("F", "O", "SP") &
  #       is.na(data$Age)
  #   ),
  #   which(
  #     !data$AGE_STRUCTURE_CODE1 %in% c("F", "O", "SP") &
  #       data$AGE_STRUCTURE_CODE2 %in% c("F", "O", "SP") &
  #       is.na(data$Age)
  #   ),
  #   which(
  #     !data$AGE_STRUCTURE_CODE1 %in% c("F", "O", "SP") &
  #       !data$AGE_STRUCTURE_CODE2 %in% c("F", "O", "SP") &
  #       data$AGE_STRUCTURE_CODE3 %in% c("F", "O", "SP") &
  #       is.na(data$Age)
  #   )
  # )
  # data$Otolith[find] <- 1

  # Since both California and Washington do not push otolith information to
  # PacFIN for unaged fish - set these values to 0
  # data[which(data$State != "Oregon" & data$Otolith != 0), "Otolith"] <- 0

  # Remove select data areas and identify yellowtail north and south
  remove <- which(
    data$State == "California" & data$Common_name == "black rockfish"
  )
  filtered_data <- data[-remove, ]
  yellowtail_north <- c(
    which(
      filtered_data$Common_name == "yellowtail rockfish" &
        filtered_data$PACFIN_PORT_NAME %in%
          c("CRESCENT", "FIELDS LDG", "EUREKA")
    ),
    which(
      filtered_data$Common_name == "yellowtail rockfish" &
        filtered_data$State %in% c("Oregon", "Washington")
    )
  )
  filtered_data$Common_name[yellowtail_north] <- "yellowtail rockfish north"
  yellowtail_south <- which(filtered_data$Common_name == "yellowtail rockfish")
  filtered_data$Common_name[yellowtail_south] <- "yellowtail rockfish south"

  lingcod_north <- c(
    which(
      filtered_data$Common_name == "lingcod" &
        filtered_data$PACFIN_PORT_NAME %in%
          c("CRESCENT", "FIELDS LDG", "EUREKA")
    ),
    which(
      filtered_data$Common_name == "lingcod" &
        filtered_data$State %in% c("Oregon", "Washington")
    )
  )
  filtered_data$Common_name[lingcod_north] <- "lingcod north"
  lingcod_south <- which(filtered_data$Common_name == "lingcod")
  filtered_data$Common_name[lingcod_south] <- "lingcod south"

  data_out <- filtered_data |>
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
