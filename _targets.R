library(targets)

# Create targets for all objects
# targets::tar_make(script = "_targets.R")
# Load existing targets
# targets::tar_load_everything()

# View network plots
# targets::tar_visnetwork(targets_only = TRUE)
# targets::tar_glimpse()

# Use the following commands to remove one or all files when getting errors
# targets::tar_delete("rank")
# targets::tar_destroy("all")

# Set target-specific options such as packages:
# tar_option_set(packages = "utils")
tar_option_set(
  packages = c(
    "here",
    "dplyr",
    "ggplot2",
    "cowplot",
    "stringr",
    "nwfscSurvey",
    "pacfintools", # This is the keep-age-structure branch
    "readr"
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
    wcgbt_raw_data,
    pull_wcgbts(
      dir = here::here("data-raw", "2026"),
      load = TRUE,
      species = species
    )
  ),
  # Clean NWFSC WCGBT data
  tar_target(
    wcgbt_catch_filtered,
    clean_wcgbt_catch(
      dir = here::here("data-processed", "2026"),
      species = species,
      data = wcgbt_raw_data
    )
  ),
  tar_target(
    wcgbt_bio_filtered,
    clean_wcgbt_bio(
      dir = here::here("data-processed", "2026"),
      species = species,
      data = wcgbt_raw_data
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
      wcgbt = wcgbt_bio_filtered,
      hkl = nwfsc_hkl_filtered
    )
  ),
  # Pull all the data together
  tar_target(
    combined_data,
    combine_all_data(
      dir = here::here("data-processed", "2026"),
      wcgbt = wcgbt_bio_filtered,
      nwfsc_hkl = nwfsc_hkl_filtered
    )
  ),
  #Plot the data
  tar_target(
    state_comparison_plots,
    plot_data_by_year(
      data = combined_data
    )
  ),
  # WCGBTS indices
  tar_target(
    auto_indexwc_output,
    list.files(
      "~/GitHub/auto-indexwc/output",
      pattern = "\\.csv$",
      full.names = TRUE
    ),
    format = "file"
  ),
  tar_target(
    copy_auto_indexwc_output,
    copy_auto_indexwc(
      files = auto_indexwc_output,
      copy_dir = here::here("data-processed", "2026", "indices")
      ),
    format = "file"
  ),
  tar_target(
    coastwide_indices,
    pull_indices(
      dir = here::here("data-processed", "2026", "indices")
    )
  ),
  tar_target(
    coastwide_indices_output_file,
    command = "data-processed/2026/coastwide_indices.csv",
    format = "file"
  ),
  tar_target(
    coastwide_indices_output,
    readr::read_csv(coastwide_indices_output_file)
  ),
  tar_target(
    plot_coastwide_indices,
    plot_wcgbts_indices(
      data = coastwide_indices_output
    )
  ),
  # NWFSC HKL NWFSC indices
  #see sandbox/run_hkl_indices.R
  tar_target(
    plot_wcgbt_comps,
    plot_wcgbt_comps(
      dir = here::here("plots", "wcgbts_comps"),
      wcgbt_catch = wcgbt_catch_filtered,
      wcgbt_bio = wcgbt_bio_filtered,
      verbose = TRUE
    )
  ),
  tar_target(
    plot_hkl_comps,
    plot_hkl_comps(
      dir = here::here("plots", "hkl_comps"),
      data = nwfsc_hkl_filtered
    )
  )  
)

# targets::tar_make()
# targets::tar_glimpse()
# targets::tar_visnetwork()
# targets::tar_load_everything()
