#' Clean up source names
#'
#' @param data add definition
#'
#' @author Chantel Wetzel
#' @export
#'
#'
get_source <- function(data) {
  use <- unique(data$sources_to_use)

  if ("all" %in% use) {
    sources <- c("NWFSC WCGBT", "NWFSC HKL")
  }
  if ("wcgbt" %in% use) {
    sources <- c("NWFSC WCGBT")
  }
  if ("hkl" %in% use) {
    sources <- c("NWFSC HKL")
  }
  return(sources)
}
