#' Clean RecFIN length data
#'
#'
#' @param dir Directory location to save the cleaned data frame
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
  dir,
  or_data = NULL,
  wa_data = NULL,
  ca_data = NULL,
  species,
  year
) {
  all_data <- rbind(wa_data, or_data, ca_data)
  all_data$species_name <- tolower(all_data$SPECIES_NAME)
  all_data$Common_name <- NA
  #data <- combine_species(data = all_data, name = "Common_name")

  data <- all_data[which(all_data$species_name %in% species[, 1]), ]
  for (a in 1:dim(species)[1]) {
    find <- grep(species[a, "name"], data[, "species_name"])
    data[find, "Common_name"] <- species[a, "use_name"]
  }

  data <- data[which(data$RECFIN_YEAR >= year), ]

  data$State <- tolower(data$STATE_NAME)
  substr(data$State, 1, 1) <- toupper(substr(data$State, 1, 1))

  # Split yellowtail north and south of 40.167
  yt_north_ca <- data[
    which(
      data$Common_name == "yellowtail rockfish" &
        data$RECFIN_PORT_NAME ==
          "WINE (MENDOCINO COUNTY AND SHELTER COVE AREA IN HUMBOLDT COUNTY)"
    ),
  ]
  yt_north <- rbind(
    yt_north_ca,
    data[
      which(
        data$Common_name == "yellowtail rockfish" &
          data$State %in% c("Oregon", "Washington")
      ),
    ]
  )
  yt_north$Common_name <- "yellowtail rockfish north"

  yt_south_ca <- data[
    which(
      data$Common_name == "yellowtail rockfish" &
        data$State == "California" &
        data$RECFIN_PORT_NAME !=
          "WINE (MENDOCINO COUNTY AND SHELTER COVE AREA IN HUMBOLDT COUNTY)"
    ),
  ]
  yt_south_ca$Common_name <- "yellowtail rockfish south"

  data <- rbind(data, yt_north, yt_south_ca)
  data$State_area <- ifelse(
    data$State == "Washington",
    "WA",
    ifelse(
      data$State == "Oregon",
      "OR",
      ifelse(
        data$State == "California" &
          data$RECFIN_PORT_NAME ==
            "WINE (MENDOCINO COUNTY AND SHELTER COVE AREA IN HUMBOLDT COUNTY)",
        "NCA",
        "SCA"
      )
    )
  )

  data$Source <- "Recreational"
  data$State_Source <- paste0(data$Source, "-", data$State)

  data$Year <- data$RECFIN_YEAR

  keep <- which(
    data$AGENCY_WATER_AREA_NAME %in%
      c(
        "NOT KNOWN", # All Washington records are not know so keeping this across the board
        "OCEAN",
        "OCEAN <= 3 MILES",
        "OCEAN <= 3 MILES (AREAB AND P1B IMPORT)",
        "OCEAN > 3 MILES",
        "OCEAN > 3 MILES (AREAB AND P1B IMPORT)"
      )
  )
  data <- data[keep, ]

  remove <- which(is.na(data$RECFIN_LENGTH_MM))
  if (length(remove) > 0) {
    data <- data[-remove, ]
  }
  remove <- which(data$RECFIN_LENGTH_MM == 0)
  if (length(remove) > 0) {
    data <- data[-remove, ]
  }
  remove <- which(data$RECFIN_LENGTH_MM > 2000)
  if (length(remove) > 0) {
    data <- data[-remove, ]
  }

  data$Length_cm <- data$RECFIN_LENGTH_MM / 10
  find <- which(data$AGENCY_LENGTH_UNITS == "C")
  data$Length_cm[find] <- data$AGENCY_LENGTH[find]
  data$Lengthed <- 1

  data$Weight_kg <- data$AGENCY_WEIGHT
  find <- which(data$AGENCY_WEIGHT_UNITS %in% c("G "))
  data$Weight_kg[find] <- data$AGENCY_WEIGHT[find] / 1000

  data$Sex <- data$RECFIN_SEX_CODE
  data$Sex[which(!data$Sex %in% c("F", "M"))] <- "U"

  data$set_tow_id <- 0
  data$Aged <- 0
  data$Age <- NA
  data$Otolith <- 0

  data$Fleet <- data$RECFIN_MODE_NAME

  save(data, file = file.path(dir, "recfin_lengths_filtered.Rdata"))
  return(data)
}
