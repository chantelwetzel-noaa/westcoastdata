#' Summarize new survey data available for a new assessment
#'
#' @param dir add description
#' @param stock_year add description
#' @param wcgbt description
#' @param hkl add description
#'
#' @author Chantel Wetzel
#' @export
#'
#'
summarize_survey_new_information <- function(dir, stock_year, wcgbt, hkl) {
  species <- get_species_list()
  stock_year[, "species"] <- tolower(stock_year[, "species"])
  for (a in 1:dim(stock_year)[1]) {
    stock_year[a, "species"] <- species[
      grep(stock_year[a, "species"], species[, "name"])[1],
      "use_name"
    ]
  }

  wcgbt_bio <- wcgbt
  # Subset the data prior to the most recent assessment
  wcgbt_year <- stock_year
  wcgbt_year[is.na(wcgbt_year$year), "year"] <- 2003
  sub_data <- NA
  for (a in unique(wcgbt_bio$Common_name)) {
    years_to_keep <- wcgbt_year[which(wcgbt_year$species == a), "year"]
    sub_data <- rbind(
      sub_data,
      wcgbt_bio[
        which(
          wcgbt_bio$Common_name == a &
            wcgbt_bio$Year >= years_to_keep
        ),
      ]
    )
  }

  hkl_stock_year <- stock_year
  # Fix yellowtail year since the southern assessment was withdrawn
  hkl_stock_year[
    which(hkl_stock_year$species == "yellowtail rockfish"),
    "year"
  ] <- 2004
  hkl_stock_year[is.na(hkl_stock_year$year), "year"] <- 2004

  sub_hkl <- NA
  for (a in unique(hkl$Common_name)) {
    years_to_keep <- hkl_stock_year[which(hkl_stock_year$species == a), "year"]
    sub_hkl <- rbind(
      sub_hkl,
      hkl[which(hkl$Common_name == a & hkl$Year >= years_to_keep), ]
    )
  }

  cols_to_keep <- c(
    "Year",
    "Common_name",
    "Source",
    "set_tow_id",
    "Lengthed",
    "Age",
    "Otolith"
  )

  data <- rbind(
    sub_data[, cols_to_keep],
    sub_hkl[, cols_to_keep]
  )

  wcgbt_total <-
    sub_data |>
    dplyr::group_by(Common_name) |>
    dplyr::summarise(
      set_tows = dplyr::n_distinct(set_tow_id),
      total_lengths = sum(Lengthed),
      total_ages = sum(!is.na(Age)),
      total_otoliths = sum(Otolith),
      years_since_assessment = dplyr::n_distinct(Year),
      ave_set_tows = floor(
        dplyr::n_distinct(set_tow_id) / dplyr::n_distinct(Year)
      ),
      ave_lengths = floor(sum(Lengthed) / dplyr::n_distinct(Year)),
      ave_ages = floor(sum(!is.na(Age)) / dplyr::n_distinct(Year)),
      ave_otoliths = floor(sum(Otolith) / dplyr::n_distinct(Year))
    )

  hkl_total <-
    sub_hkl |>
    dplyr::group_by(Common_name) |>
    dplyr::summarise(
      set_tows = dplyr::n_distinct(set_tow_id),
      total_lengths = sum(Lengthed),
      total_ages = sum(!is.na(Age)),
      total_otoliths = sum(Otolith),
      years_since_assessment = dplyr::n_distinct(Year),
      ave_set_tows = floor(
        dplyr::n_distinct(set_tow_id) / dplyr::n_distinct(Year)
      ),
      ave_lengths = floor(sum(Lengthed) / dplyr::n_distinct(Year)),
      ave_ages = floor(sum(!is.na(Age)) / dplyr::n_distinct(Year)),
      ave_otoliths = floor(sum(Otolith) / dplyr::n_distinct(Year))
    )

  survey_total <-
    data[!is.na(data$Common_name), ] |>
    dplyr::group_by(Common_name) |>
    dplyr::summarise(
      set_tow = dplyr::n_distinct(set_tow_id),
      total_lengths = sum(Lengthed),
      total_ages = sum(!is.na(Age)),
      total_otoliths = sum(Otolith),
      years_since_assessment = dplyr::n_distinct(Year),
      ave_set_tows = floor(
        dplyr::n_distinct(set_tow_id) / dplyr::n_distinct(Year)
      ),
      ave_lengths = floor(sum(Lengthed) / dplyr::n_distinct(Year)),
      ave_ages = floor(sum(!is.na(Age)) / dplyr::n_distinct(Year)),
      ave_otoliths = floor(sum(Otolith) / dplyr::n_distinct(Year)),
      wcgbt = sum(Source == "NWFSC WCGBT"),
      nwfsc_hkl = sum(Source == "NWFSC HKL"),
      wcgbt_percent = round(wcgbt / (wcgbt + nwfsc_hkl), 2)
    )

  readr::write_csv(wcgbt_total, file.path(dir, "wcgbt_new_information.csv"))
  readr::write_csv(hkl_total, file.path(dir, "nwfsc_hkl_new_information.csv"))
  readr::write_csv(
    survey_total,
    file.path(dir, "all_nwfsc_survey_new_information.csv")
  )

  return(survey_total)
}
