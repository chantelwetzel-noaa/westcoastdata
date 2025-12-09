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
  stock_year <- stock_year |>
    dplyr::rename(species = Species)
  for (a in 1:dim(stock_year)[1]) {
    stock_year[a, "species"] <- species[
      grep(stock_year[a, "species"], species[, "name"])[1],
      "use_name"
    ]
  }

  wcgbt_bio <- wcgbt |>
    dplyr::filter(Common_name %in% unique(species[, "use_name"]))
  remove <- c(
    which(wcgbt_bio$Common_name == "black rockfish" & wcgbt_bio$State == "CA"),
    which(
      wcgbt_bio$Common_name == "blue and deacon rockfish" &
        wcgbt_bio$State == "CA"
    ),
    which(
      wcgbt_bio$Common_name == "cabezon" & wcgbt_bio$State %in% c("CA", "OR")
    ),
    which(wcgbt_bio$Common_name == "China rockfish" & wcgbt_bio$State == "CA"),
    which(wcgbt_bio$Common_name == "copper rockfish" & wcgbt_bio$State == "CA"),
    which(
      wcgbt_bio$Common_name == "quillback rockfish" & wcgbt_bio$State == "CA"
    ),
    which(wcgbt_bio$Common_name == "kelp greenling" & wcgbt_bio$State == "CA")
  )
  wcgbt_bio <- wcgbt_bio[-remove, ]

  # Subset the data prior to the most recent assessment
  wcgbt_year <- stock_year[, c("species", "year")]
  wcgbt_year[is.na(wcgbt_year$year), "year"] <- 2003

  # Modify common name for area-based species
  wcgbt_year <- wcgbt_year |>
    dplyr::mutate(
      species = dplyr::case_when(
        species == "yellowtail rockfish" ~ "yellowtail rockfish north",
        .default = species
      )
    )
  wcgbt_year <- rbind(wcgbt_year, c("yellowtail rockfish south", "2003"))
  wcgbt_bio <- wcgbt_bio |>
    dplyr::mutate(
      years_since_assessment = NA
    )
  sub_data <- NULL
  for (a in unique(wcgbt_bio$Common_name)) {
    if (wcgbt_year[which(wcgbt_year$species == a), "year"] != 2025) {
      years_to_keep <- wcgbt_year[which(wcgbt_year$species == a), "year"]
      check <- wcgbt_bio |>
        dplyr::filter(Common_name == a) |>
        dplyr::mutate(
          years_since_assessment = as.numeric(years_to_keep)
        ) |>
        dplyr::filter(Year >= years_since_assessment)
      add_data <- check
    } else {
      check <- NULL
    }

    if (!is.null(check)) {
      if (nrow(check) > 0) {
        sub_data <- rbind(
          sub_data,
          add_data
        )
      }
    }
  }

  hkl_stock_year <- stock_year
  # Fix yellowtail year since the southern assessment was withdrawn
  hkl_stock_year[
    which(hkl_stock_year$species == "yellowtail rockfish"),
    "year"
  ] <- 2004
  hkl_stock_year[
    which(hkl_stock_year$species == "yellowtail rockfish"),
    "species"
  ] <- "yellowtail rockfish south"
  hkl_stock_year[is.na(hkl_stock_year$year), "year"] <- 2004
  hkl[
    which(hkl$Common_name == "vermilion rockfish"),
    "Common_name"
  ] <- "vermilion and sunset rockfish"
  hkl[
    which(hkl$Common_name == "blue rockfish"),
    "Common_name"
  ] <- "blue and deacon rockfish"
  hkl_bio <- hkl |>
    dplyr::filter(
      !Common_name %in%
        c(
          "black rockfish",
          "copper rockfish",
          "cabezon",
          "china rockfish",
          "kelp greenling",
          "blue and deacon rockfish",
          "quillback rockfish",
          "yellowtail rockfish" #this wasn't listed in hkl_stock_year, so I removed it, what should it be instead?
        )
    ) |>
    dplyr::mutate(
      years_since_assessment = NA
    ) |>
    dplyr::filter(Common_name %in% unique(species[, "use_name"]))
  sub_hkl <- NULL
  for (a in unique(hkl_bio$Common_name)) {
    years_to_keep <- hkl_stock_year[which(hkl_stock_year$species == a), "year"]
    add_hkl <- hkl_bio |>
      dplyr::filter(Common_name == a) |>
      dplyr::mutate(years_since_assessment = as.numeric(years_to_keep)) |>
      dplyr::filter(Year >= years_since_assessment)
    if (nrow(add_hkl) > 0) {
      sub_hkl <- rbind(
        sub_hkl,
        add_hkl
      )
    }
    
    print(a)
  }
  sub_data <- sub_data |>
    dplyr::rename(
      set_tow_id = Tow
    )

  cols_to_keep <- c(
    "Year",
    "Common_name",
    "Source",
    "set_tow_id",
    "Lengthed",
    "Aged",
    "Otolith",
    "years_since_assessment"
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
      years_since_assessment = unique(years_since_assessment),
      ave_set_tows = floor(
        dplyr::n_distinct(set_tow_id) / dplyr::n_distinct(Year)
      ),
      ave_lengths = floor(sum(Lengthed) / dplyr::n_distinct(Year)),
      ave_ages = floor(sum(!is.na(Aged)) / dplyr::n_distinct(Year)),
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
      years_since_assessment = unique(years_since_assessment),
      ave_set_tows = floor(
        dplyr::n_distinct(set_tow_id) / dplyr::n_distinct(Year)
      ),
      ave_lengths = floor(sum(Lengthed) / dplyr::n_distinct(Year)),
      ave_ages = floor(sum(!is.na(Age)) / dplyr::n_distinct(Year)),
      ave_otoliths = floor(sum(Otolith) / dplyr::n_distinct(Year))
    )

  survey_total <- data |>
    dplyr::group_by(Common_name) |>
    dplyr::summarise(
      set_tow = dplyr::n_distinct(set_tow_id),
      total_lengths = sum(Lengthed),
      total_ages = sum(!is.na(Aged)),
      total_otoliths = sum(Otolith),
      years_since_assessment = min(years_since_assessment),
      ave_set_tows = floor(
        dplyr::n_distinct(set_tow_id) / dplyr::n_distinct(Year)
      ),
      ave_lengths = floor(sum(Lengthed) / dplyr::n_distinct(Year)),
      ave_ages = floor(sum(!is.na(Aged)) / dplyr::n_distinct(Year)),
      ave_otoliths = floor(sum(Otolith) / dplyr::n_distinct(Year)),
      wcgbt = sum(Source == "NWFSC WCGBTS"),
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
