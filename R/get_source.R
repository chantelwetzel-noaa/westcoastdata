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

  if ("all_plus_plus" %in% use) {
    sources <- c("CCFRP", "Commercial", "GCDC", "NWFSC HKL", "NWFSC WCGBT", "Recreational")
  }
  if ("all_plus" %in% use) {
    sources <- c("CCFRP", "Commercial", "NWFSC HKL", "NWFSC WCGBT", "Recreational")
  }
  if ("all" %in% use) {
    sources <- c("NWFSC WCGBT", "NWFSC HKL")
  }
  if ("wcgbt" %in% use) {
    sources <- c("NWFSC WCGBT")
  }
  if ("hkl" %in% use) {
    sources <- c("NWFSC HKL")
  }
  if ("comm_wcgbt_rec" %in% use) {
    sources <- c("Commercial", "NWFSC WCGBT", "Recreational")
  }
  if ("ccfrp_comm_wcbgt_rec" %in% use) {
    sources <- c("CCFRP", "Commercial", "NWFSC WCGBT", "Recreational")
  }
  if ("comm_wcgbt" %in% use) {
    sources <- c("Commercial", "NWFSC WCGBT")
  }
  if ("ccfrp_comm_hkl_wcgbt" %in% use) {
    sources <- c("CCFRP", "Commercial", "NWFSC HKL", "NWFSC WCGBT")
  }
  return(sources)
}
