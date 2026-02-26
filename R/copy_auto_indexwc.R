copy_auto_indexwc <- function(files, copy_dir) {
  dir.create(copy_dir, recursive = TRUE, showWarnings = FALSE)
  
  copy_paths <- file.path(copy_dir, basename(files))
  
  purrr::walk2(files, copy_paths, ~ file.copy(.x, .y, overwrite = TRUE))
  
  copy_paths
}