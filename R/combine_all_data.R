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
    data_list[[a]][, "Fleet"] <- as.character(data_list[[a]][,
      "Fleet"
    ])
    data_list[[a]][, "Year"] <- as.numeric(data_list[[a]]$Year)
    data <- dplyr::bind_rows(
      data,
      data_list[[a]][, cols_to_keep]
    )
  }

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
  save(data_total_by_year, file = file.path(dir, "data_total_by_year.Rdata"))
  return(data_total_by_year)
}
