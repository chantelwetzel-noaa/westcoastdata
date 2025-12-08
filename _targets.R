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
  tar_target(species, get_species_list()),
  tar_target(
    spid_key,
    read.csv(
      here::here("data-raw", "2026", "pacfin_species_codes.csv")
    )
  ),
  tar_target(stock_year, read.csv(here::here("data-raw", "2026", "stock_year.csv"))),
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
      dir = here::here("data-raw", "2026"),
      species = species,
      data = wcgbt_data
    )
  ),
  tar_target(
    wcgbt_filtered,
    clean_wcgbt_bio(
      dir = here::here("data-raw", "2026"),
      species = species,
      data = wcgbt_data
    )
  )
)

# NWFSC HKL NWFSC WCGBT

# targets::tar_make()
# targets::tar_glimpse()
# targets::tar_visnetwork()
# targets::tar_load_everything()