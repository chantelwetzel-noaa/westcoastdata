#' Load rdata file
#'
#' @param dir Directory where a rdata object is located
#' @param rdata_name
#' @param object
#'
#' @author Chantel Wetzel
#' @export
#'
#'
load_rdata_object <- function(dir, rdata_name, object_name) {
  load(file.path(dir, rdata_name))
  return(object_name)
}

dir <- "G:/My Drive/prioritization/westcoastdata/data-raw/2026/"
rdata_name <- "PacFIN.bds.19.Mar.2026.RData"
object_name <- "bds.pacfin"
