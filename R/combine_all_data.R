#' Combine all data into a data frame
#'
#'
#' @param dir Directory location to save the combined data frame
#' @param wcgbt add definition
#' @param nwfsc_hkl add definition
#'
#' @author Chantel Wetzel
#' @export
#'
combine_all_data <- function(
  dir = dir,
  wcgbt,
  nwfsc_hkl
) {
  #Combine data sets into a single data frame
  cols_to_keep <- c(
    "Year", #yes, yes
    "State", #yes, yes
    "Source", #yes, yes
    "Common_name", #yes, yes
    "Fleet", #yes, yes
    "set_tow_id", #no, yes
    "Lengthed", #yes, yes
    "Otolith", #yes, yes
    "Age", #no, yes
    "Aged", #yes, yes
    "Length_cm", #yes, yes
    "Weight_kg", #yes, yes
    "Sex" #yes, yes
  )

  data <- rbind(
    wcgbt[, cols_to_keep],
    nwfsc_hkl[, cols_to_keep]
  )

  save(data, file = file.path(dir, "combined_data.Rdata"))
  #data$read_age <- 0
  #data$read_age[!is.na(data$Age)] <- 1
  #data[is.na(data)] <- 0

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

  save(data_total_by_year, file = file.path(dir, "data_total_by_year.Rdata"))

  return(data_total_by_year)
}
