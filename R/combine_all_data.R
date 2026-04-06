#' Combine all data into a data frame
#'
#'
#' @param dir Directory location to save the combined data frame
#' @param data_list List of data frames that will be combined
#'
#' @author Chantel Wetzel
#' @export
#'
combine_all_data <- function(
  dir,
  data_list
) {
  #Combine data sets into a single data frame
  cols_to_keep <- c(
    "Year",
    "State",
    "Source",
    "Common_name",
    "Fleet",
    "set_tow_id",
    "Lengthed",
    "Otolith",
    "Age",
    "Aged",
    "Length_cm",
    "Weight_kg",
    "Sex"
  )

  data <- NULL
  for (a in 1:length(data_list)) {
    data_list[[a]][, "set_tow_id"] <- as.character(data_list[[a]][,
      "set_tow_id"
    ])
    data_list[[a]][, "Year"] <- as.numeric(data_list[[a]]$Year)
    data <- dplyr::bind_rows(
      data,
      data_list[[a]][, cols_to_keep]
    )
  }
  save(data, file = file.path(dir, "combined_data.Rdata"))

  group_vars = c("Common_name", "State", "Source")
  data_total <- data |>
    dplyr::group_by_at(group_vars) |>
    dplyr::summarise(
      set_tows = dplyr::n_distinct(set_tow_id),
      total_lengths = sum(Lengthed),
      total_ages = sum(Aged),
      total_otoliths = sum(Otolith),
      n_years = dplyr::n_distinct(Year),
      ave_set_tows = floor(
        dplyr::n_distinct(set_tow_id) / dplyr::n_distinct(Year)
      ),
      ave_lengths = floor(sum(Lengthed) / dplyr::n_distinct(Year)),
      ave_ages = floor(sum(Aged) / dplyr::n_distinct(Year)),
      ave_otoliths = floor(sum(Otolith) / dplyr::n_distinct(Year))
    ) |>
    as.data.frame()

  group_vars <- c("Common_name", "State", "Source", "Year")
  data_total_by_year <- data |>
    dplyr::group_by_at(group_vars) |>
    dplyr::summarise(
      set_tows = dplyr::n_distinct(set_tow_id),
      total_lengths = sum(Lengthed),
      total_ages = sum(Aged),
      total_otoliths = sum(Otolith)
    ) |>
    as.data.frame()

  write.csv(data_total, file.path(dir, "data_summaries.csv"), row.names = FALSE)
  write.csv(
    data_total_by_year,
    file.path(dir, "data_summaries_by_year.csv"),
    row.names = FALSE
  )
  return(data_total_by_year)
}
