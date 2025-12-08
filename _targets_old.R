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

targets::tar_source()

# End this file with a list of target objects.
list(
  # Load in raw data and species lists
  tar_target(year, 2000),
  tar_target(species, get_species_list()),
  tar_target(
    spid_key,
    read.csv(
      here::here("data-raw", "pacfin_species_codes.csv")
    )
  ),
  tar_target(stock_year, read.csv(here::here("data-raw", "stock_year.csv"))),
  # Survey data
  # Pull the WCGBT survey data
  tar_target(
    wcgbt_data,
    pull_wcgbts(
      dir = here::here("data-raw"),
      load = TRUE,
      species = species
    )
  ),
  # NWFSC HKL Survey Data
  tar_target(
    nwfsc_hkl,
    read.csv(
      here::here("data-raw", "qryDataSummaryForChantel2004-2023.csv")
    )
  ),
  # CCFRP lengths
  tar_target(
    ccfrp_data,
    read.csv(
      here::here("data-raw", "CCFRP_derived_length_table.csv")
    )
  ),
  # Fishery Data
  # RecFIN data
  tar_target(
    recfin_wa_len,
    read.csv(
      here::here("data-raw", "SD501-WASHINGTON-1983---2023.csv")
    )
  ),
  tar_target(
    recfin_or_len,
    read.csv(
      here::here("data-raw", "SD501-OREGON-1983---2023.csv")
    )
  ),
  tar_target(
    recfin_ca_len,
    read.csv(
      here::here("data-raw", "SD501-CALIFORNIA-1983---2023.csv")
    )
  ),
  #tar_target(recfin_ages, read.csv(
  #  here::here("data-raw", "SD506--1984---2023.csv"))),
  tar_target(
    recfin_ages,
    read.csv(
      here::here("data-raw", "RecFIN_Ageing.csv")
    )
  ),
  # Pull PacFIN data
  tar_target(
    bds_pacfin,
    load_pacfin_data(
      dir = here::here("data-raw"),
      file_name = "PacFIN_.bds.13.Dec.2023.RData"
    )
  ),
  # WA otoliths
  tar_target(
    wa_oto_raw,
    read.csv(
      here::here('data-raw', "wa_com_UnagedStructures.csv")
    )
  ),
  tar_target(
    wa_com_oto,
    format_cdfw_otolith_files(
      data = wa_oto_raw,
      source = "Commercial",
      state = "Washington"
    )
  ),
  # CA otoliths
  tar_target(
    coop_rec,
    read.csv(
      here::here("data-raw", "cooperative-recreational-summary-table.csv")
    )
  ),
  tar_target(
    ca_com_oto_raw,
    read.csv(
      here::here("data-raw", "PSMFC_ca_commercial_otoliths_2023.csv")
    )
  ),
  tar_target(
    ca_com_oto,
    format_cdfw_otolith_files(
      data = ca_com_oto_raw,
      source = "Commercial",
      state = "California"
    )
  ),
  tar_target(
    ca_com_pilot_oto_raw,
    read.csv(
      here::here("data-raw", "cdfw-com-pilot-otoliths.csv")
    )
  ),
  tar_target(
    ca_rec_oto_raw,
    read.csv(
      here::here("data-raw", "cdfw-rec-otoliths.csv")
    )
  ),
  tar_target(
    ca_rec_carcass_oto_raw,
    read.csv(
      here::here("data-raw", "cdfw-rec-carcass-otoliths.csv")
    )
  ),
  tar_target(
    ca_rec_oto,
    format_cdfw_otolith_files(
      data = ca_rec_oto_raw,
      source = "CDFW-Ad hoc",
      state = "California"
    )
  ),
  tar_target(
    ca_rec_carcass_oto,
    format_cdfw_otolith_files(
      data = ca_rec_carcass_oto_raw,
      source = "CDFW-Ad hoc",
      state = "California"
    )
  ),
  tar_target(
    ca_com_pilot_oto,
    format_cdfw_otolith_files(
      data = ca_com_pilot_oto_raw,
      source = "CDFW-Ad hoc",
      state = "California"
    )
  ),
  # Clean RecFIN data
  tar_target(
    recfin_len_filtered,
    clean_recfin_lengths(
      dir = here::here("data-raw"),
      or_data = recfin_or_len,
      wa_data = recfin_wa_len,
      ca_data = recfin_ca_len,
      species = species,
      year = year
    )
  ),
  tar_target(
    recfin_ages_filtered,
    clean_recfin_ages(
      dir = here::here("data-raw"),
      species = species,
      data = recfin_ages,
      year = year
    )
  ),
  tar_target(
    coop_filtered,
    clean_coop_samples(
      data = coop_rec,
      species = species
    )
  ),
  # Clean CCFRP
  tar_target(
    ccfrp_filtered,
    clean_ccfrp(
      species = species,
      data = ccfrp_data
    )
  ),
  # Clean the PacFIN data
  tar_target(
    pacfin_bio_filtered,
    clean_pacfin_comps(
      species = species,
      bds_pacfin = bds_pacfin,
      dir = here::here("data-raw"),
      spid_key = spid_key,
      year = year
    )
  ),
  # Clean NWFSC WCGBT data
  tar_target(
    wcgbt_catch,
    clean_wcgbt_catch(
      dir = here::here("data-raw"),
      species = species,
      data = wcgbt_data
    )
  ),
  tar_target(
    wcgbt_filtered,
    clean_wcgbt_bio(
      dir = here::here("data-raw"),
      species = species,
      data = wcgbt_data
    )
  ),
  # Clean NWFSC HKL data
  tar_target(
    nwfsc_hkl_filtered,
    clean_nwfsc_hkl(
      dir = here::here("data-raw"),
      species = species,
      data = nwfsc_hkl
    )
  ),
  # Summarize the amount of new data
  tar_target(
    new_info,
    summarize_survey_new_information(
      dir = here::here("data-processed"),
      stock_year = stock_year,
      wcgbt = wcgbt_filtered,
      hkl = nwfsc_hkl_filtered
    )
  ),
  # Pull all the data together
  tar_target(
    combined_data,
    combine_all_data(
      dir = here::here("data-processed"),
      wcgbt = wcgbt_filtered,
      nwfsc_hkl = nwfsc_hkl_filtered,
      pacfin = pacfin_bio_filtered,
      recfin_lengths = recfin_len_filtered,
      recfin_ages = recfin_ages_filtered,
      ca_rec_oto = ca_rec_oto,
      ca_rec_carcass_oto = ca_rec_carcass_oto,
      ca_com_pilot_oto = ca_com_pilot_oto,
      ca_com_oto = ca_com_oto,
      wa_com_oto = wa_com_oto,
      coop_rec = coop_filtered,
      ccfrp = ccfrp_filtered
    )
  ),
  #Plot the data
  tar_target(
    plots,
    plot_data_by_year(
      data = combined_data
    )
  ),
  #tar_target(com_plots, plot_wcgbt_comps(
  #  dir = here::here(),
  #  wcgbt_catch = wcgbt_catch,
  #  wcgbt_bio = wcgbt_filtered[which(wcgbt_filtered$Common_name %in% c("yellowtail rockfish north", "yellowtail rockfish south")),]
  #)),
  #tar_target(hkl_plots, plot_hkl_comps(
  #  data = nwfsc_hkl_filtered
  #))
  # quillback ages
  tar_target(
    qb_ca,
    read.csv(here::here("data-raw", "quillback_aged_04112024.csv"))
  )
)

# NWFSC HKL NWFSC WCGBT

# targets::tar_make()
# targets::tar_glimpse()
# targets::tar_visnetwork()
# targets::tar_load_everything()
