#' Clean RecFIN data
#'
#' @param dir Directory location to save the cleaned data frame
#' @param species add definition
#' @param data add definition
#' @param year add definition
#'
#' @author Chantel Wetzel
#' @export
#'
#'
clean_recfin_ages <- function(dir, species, data, year) {
  # filter down to only unique records with the same SAMPLE_ID
  # There are 602 duplicate records (394 in OR and 208 in WA)
  data <- data[which(data$RECFIN_READ_NUMBER %in% c(0, 1)), ]

  data$species_name <- tolower(data$RECFIN_SPECIES_NAME)
  data$Common_name <- NA

  data <- data[which(data$species_name %in% species[, 1]), ]
  for (a in 1:dim(species)[1]) {
    find <- grep(species[a, "name"], data[, "species_name"])
    data[find, "Common_name"] <- species[a, "use_name"]
  }

  data$State <- ifelse(
    data$SAMPLING_AGENCY_NAME == "W",
    "Washington",
    ifelse(data$SAMPLING_AGENCY_NAME == "O", "Oregon", NA)
  )
  data$Source <- "Recreational"
  data$State_Source <- paste0(data$Source, "-", data$State)
  data$State_area <- ifelse(data$State == "Washington", "WA", "OR")

  data$Year <- data$SAMPLE_YEAR
  data <- data[data$Year >= year, ]

  # Add yellowtail north
  yt <- data[data$Common_name == "yellowtail rockfish", ]
  yt$Common_name <- "yellowtail rockfish north"
  data <- rbind(data, yt)

  data$Length_cm <- NA
  data$Length_cm[which(data$LENGTH_UNITS %in% c("C", ""))] <-
    data[which(data$LENGTH_UNITS %in% c("C", "")), "MEASURED_LENGTH"]
  data$Length_cm[which(data$LENGTH_UNITS %in% c("M", "MM"))] <-
    data[which(data$LENGTH_UNITS %in% c("M", "MM")), "MEASURED_LENGTH"] / 10

  data$Lengthed <- 0
  data$Weight_kg <- NA

  data$Sex <- data$RECFIN_SEX_CODE
  data$Sex[which(!data$Sex %in% c("F", "M"))] <- "U"

  data$set_tow_id <- 0

  data$Age <- data$USE_THIS_AGE
  data$Aged <- 0
  data$Aged[!is.na(data$Age)] <- 1

  data$Otolith <- 0
  # Oregon does not always record an entry in the RECFIN_STRUCTURE_DESCRIPTION column but
  # the sample would not be in this table if there was not a structure collected
  data$Otolith[which(is.na(data$Age))] <- 1

  data[
    which(
      data$AGE_READABILITY_DESCRIPTION %in%
        c(
          "NOT AGED-PROCESS STORAGE OR COLLECTORS ERROR",
          "NOT AGED-STRUCTURE NOT DISCERNABLE"
        ) &
        !is.na(data$Age)
    ),
    "Otolith"
  ] <- 0

  data$Fleet <- data$RECFIN_MODE_NAME

  save(data, file = file.path(dir, "recfin_ages_filtered.Rdata"))
  return(data)
}
