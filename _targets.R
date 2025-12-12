library(targets)

# Set target-specific options such as packages:
# tar_option_set(packages = "utils")
tar_option_set(
  packages = c(
    "here",
    "dplyr",
    "ggplot2",
    "cowplot",
    "nwfscSurvey",
    "pacfintools" # This is the keep-age-structure branch
  )
)

targets::tar_source(here::here("R")) #functions are sourced from the "R" folder

# End this file with a list of target objects.
list(
  # Load in raw data and species lists
  tar_target(year, 2000),
  tar_target(
    species, 
    get_species_list()
  ),
  tar_target(
    stock_year_file,
    command = "data-raw/2026/assess_year_ssc_rec.csv",
    format = "file"
  ),
  tar_target(
    stock_year, 
    readr::read_csv(stock_year_file)
  ), 
  # Survey data
  # Pull the WCGBT survey data
  tar_target(
    wcgbt_data,
    pull_wcgbts(
      dir = here::here("data-raw", "2026"),
      load = TRUE,
      species = species
    )
  ),
  # Clean NWFSC WCGBT data
  tar_target(
    wcgbt_catch,
    clean_wcgbt_catch(
      dir = here::here("data-processed", "2026"),
      species = species,
      data = wcgbt_data
    )
  ),
  tar_target(
    wcgbt_filtered,
    clean_wcgbt_bio(
      dir = here::here("data-processed", "2026"),
      species = species,
      data = wcgbt_data
    )    
  ),
    # NWFSC HKL Survey Data
  tar_target(
    nwfsc_hkl_file,
    command = "data-raw/2026/nwfsc_hkl_DWarehouse_version_09032025.csv",
    format = "file"
  ),
    tar_target(
      nwfsc_hkl,
      readr::read_csv(nwfsc_hkl_file)
  ),
  # Clean NWFSC HKL data
  tar_target(
    nwfsc_hkl_filtered,
    clean_nwfsc_hkl(
      dir = here::here("data-processed", "2026"),
      species = species,
      data = nwfsc_hkl
    )
  ),
  # Summarize the amount of new data
  tar_target(
    new_info,
    summarize_survey_new_information(
      dir = here::here("data-processed", "2026"),
      stock_year = stock_year,
      wcgbt = wcgbt_filtered,
      hkl = nwfsc_hkl_filtered
    )
  ),
  # Pull all the data together
  tar_target(
    combined_data,
    combine_all_data(
      dir = here::here("data-processed", "2026"),
      wcgbt = wcgbt_filtered,
      nwfsc_hkl = nwfsc_hkl_filtered
    )
  ),
  #Plot the data
  tar_target(
    plots,
    plot_data_by_year(
      data = combined_data
    )
  ),
  #indices
  tar_target(
    coastwide_indices,
    pull_indices(
      dir = "C:/Users/Claire.Rosemond/Documents/GitHub/auto-indexwc/output"
    )
  )
)

# NWFSC HKL NWFSC WCGBT

# targets::tar_make()
# targets::tar_glimpse()
# targets::tar_visnetwork()
# targets::tar_load_everything()