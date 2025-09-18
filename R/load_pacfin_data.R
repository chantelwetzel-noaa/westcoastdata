#' Function to load in a PacFIN Rdata file.
#'
#' The function expects the Rdata file to have a name of bds.pacfin.
#'
#' @param dir Directory location to save the cleaned data frame
#' @param file_name PacFIN Rdata file to be loaded
#'
#' @author Chantel Wetzel
#' @export
#'
#'
load_pacfin_data <- function(dir, file_name) {
  load(file.path(dir, file_name))
  return(bds.pacfin)
}
