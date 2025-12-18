pull_indices <- function(dir){
  
  updated_indices <- list.files(
    dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  updated_indices_use <- updated_indices[!grepl("^biomass", basename(updated_indices), ignore.case = TRUE)]
 
  #this isn't recognizing when the values in these files in this folder change 
  #for each one, should I save each as a target?
  filter_coastwide <- function(file) {
    readr::read_csv(file, show_col_types = FALSE) %>%
      dplyr::filter(index == "Coastwide")
  }
  
  combined_coastwide <- purrr::map_df(updated_indices_use, filter_coastwide)
  
  out_path <- file.path(here::here("data-processed", "2026"), "coastwide_indices.csv")
  readr::write_csv(combined_coastwide, out_path)
  
  invisible(NULL)
}



#or maybe, right away, I save all the output files as target "files" in a folder in westcoastdata and then run this function pointing to that folder!