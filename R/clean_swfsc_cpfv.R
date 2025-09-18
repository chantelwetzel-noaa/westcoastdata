#' Clean COOP data from Melissa Monk and Rachel Brooks
#'
#' @param data add definition
#' @param species add definition
#'
#' @author Chantel Wetzel
#' @export
#'
#'
clean_coop_samples <- function(data, species) {
  data$SPECIES_NAME <- tolower(data$SPECIES_NAME)

  keep <- which(data[, "SPECIES_NAME"] %in% species[, "name"])
  data <- data[keep, ]

  data$Common_name <- NA
  for (a in 1:dim(species)[1]) {
    find <- grep(species[a, "name"], data[, "SPECIES_NAME"])
    data[find, "Common_name"] <- species[a, "use_name"]
  }

  yt_south <- data[data$Common_name == "yellowtail rockfish", ]
  yt_south$Common_name <- "yellowtail rockfish south"

  data <- rbind(data, yt_south)

  data$State <- "California"
  data$Source <- "SWFSC-CPFV Coop."
  data$State_Source <- paste0("SWFSC-CPFV Coop-", data$State)
  data$Sex <- "U"
  data$Lengthed <- data[, "Lengths"]
  data$Otolith <- data[, "Unread_Otoliths"]
  data$Fleet <- "CPFV"
  data$set_tow_id <- 0
  data$Length_cm <- NA
  data$Age <- NA
  data$Weight_kg <- NA

  return(data)
}
