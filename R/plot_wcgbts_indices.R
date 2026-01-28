plot_wcgbts_indices <- function(data, 
                         save_dir = here::here("plots", "wcgbts_indices"), 
                         width = 10,
                         height = 7,
                         dpi = 300,
                         pointsize = 12) {
  
  species_to_plot <- unique(data$common_name)
  
  purrr::walk(species_to_plot, function(this_species){
    
    df_this_species <- data %>%
      dplyr::filter(common_name == this_species) %>%
      dplyr::arrange(year)
    
    p <- ggplot2::ggplot(df_this_species, aes(x = year, y = est)) +
      ggplot2::geom_point()+
      ggplot2::geom_line(lty = 2)+
      ggplot2::geom_errorbar(aes(ymin = lwr, ymax = upr))+
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = this_species,
        x = "Year",
        y = "Index (mt)")+
      ggplot2::expand_limits(y = 0)
    
    file_name <- paste0(
      stringr::str_replace_all(tolower(this_species), "[^a-z0-9]+", "_"),
      "_index.png"
    )
 
    new_file_name <- case_when(
          stringr::str_detect(file_name, "^lingcod") ~
            stringr::str_replace(file_name, "^lingcod", "lingcod_north"),
          stringr::str_detect(file_name, "^rougheye") ~
            stringr::str_replace(file_name, "^rougheye_rockfish", "rougheye_and_blackspotted_rockfish"),          
          stringr::str_detect(file_name, "^yellowtail") ~
            stringr::str_replace(file_name, "^yellowtail_rockfish", "yellowtail_rockfish_north"),
          TRUE ~ file_name
        )  
    
    file_name <- new_file_name

    ggplot2::ggsave(
      filename = file.path(save_dir, file_name),
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      pointsize = pointsize
    )
  })
}