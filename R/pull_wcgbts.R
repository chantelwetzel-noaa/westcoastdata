#' Wrapper function to pull all species catch and biological data from the NWFSC data wharehouse.
#'
#'
#' @param dir Directory location to save the cleaned data frame
#' @param species A list of species names created by the get_species_list function
#' @param survey A character entry from one of the following options that
#' specifies which survey to pull the data for:
#'   * Triennial,
#'   * AFSC.Slope,
#'   * NWFSC.Combo,
#'   * NWFSC.Slope,
#'   * NWFSC.Shelf
#' @param load Option to load in existing Rdata files with the catch and biological data.
#' @param convert A logical as to whether to convert the data column names
#' to the old names as currently used in the nwfscSurvey processing code
#' @param verbose A logical value specifying whether or not to print out
#' message statements to the screen while using this function.
#' The default is to print all messages, i.e., `verbose = TRUE`.
#'
#' @author Chantel Wetzel
#' @export
#'
pull_wcgbts <- function(
  dir = here::here("data-raw"),
  species,
  survey = 'NWFSC.Combo',
  load = FALSE,
  convert = TRUE,
  verbose = FALSE
) {
  if (load) {
    file <- list.files(dir)[grep("bio_pull all", list.files(dir))]
    load(file.path(dir, file))
    bio <- x

    file <- list.files(dir)[grep("catch_pull all", list.files(dir))]
    load(file.path(dir, file))
    catch <- x
  } else {
    catch <- nwfscSurvey::pull_catch(
      survey = survey,
      dir = dir,
      convert = convert,
      verbose = verbose
    )

    bio <- nwfscSurvey::pull_bio(
      survey = survey,
      dir = dir,
      convert = convert,
      verbose = verbose
    )
  }

  data <- list()
  data$catch <- catch
  data$bio <- bio
  return(data)
}
