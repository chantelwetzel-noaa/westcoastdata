#' Load rdata file
#'
#' @param dir Directory where a rdata object is located
#' @param rdata_name file name
#'
#' @author Chantel Wetzel
#' @export
#'
#'
load_rdata_object <- function(dir, rdata_name) {
  tmp_env <- new.env()
  load(file.path(dir, rdata_name), envir = tmp_env)
  object_name <- ls(tmp_env)
  return(get(object_name[1], envir = tmp_env))
}
