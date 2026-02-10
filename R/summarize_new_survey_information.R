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
  stock_year_df <- stock_year |>
    dplyr::mutate(
      species = dplyr::case_when(
        Species == "lingcod" ~ "lingcod north",
        Species == "yellowtail rockfish" ~ "yellowtail rockfish north",
        .default = Species
      )
    )
  stock_year_df <- dplyr::bind_rows(
    stock_year_df,
    stock_year_df[which(stock_year_df == "lingcod"), ] |>
      dplyr::mutate(species = "lingcod south"),
    stock_year_df[which(stock_year_df == "yellowtail rockfish"), ] |>
      dplyr::mutate(
        species = "yellowtail rockfish south",
        Last_Assess = NA,
        year = NA,
        type = NA,
        SSC_Rec = NA
      )
  )

  # Subset the data prior to the most recent assessment
  wcgbt_year <- stock_year_df[, c("species", "year")]
  wcgbt_year[is.na(wcgbt_year$year), "year"] <- 2003

  wcgbt_bio <- wcgbt |>
    dplyr::mutate(
      years_since_assessment = NA
    )
  sub_data <- NULL
  for (a in sort(unique(wcgbt_bio$Common_name))) {
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

  hkl_stock_year <- stock_year_df
  hkl_stock_year[is.na(hkl_stock_year$year), "year"] <- 2004
  hkl_bio <- hkl |>
    dplyr::mutate(
      years_since_assessment = NA
    )
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
  }

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
      nwfsc_hkl = sum(Source == "NWFSC HKLS"),
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
