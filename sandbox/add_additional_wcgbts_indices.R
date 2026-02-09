library(dplyr)
library(readr)

raw_url <- "https://raw.githubusercontent.com/pfmc-assessments/indexwc/refs/heads/main/data-raw/configuration.csv"
configuration <- read.csv(raw_url) %>%
  dplyr::filter(source == "NWFSC.Combo")

lingcod_south <- read.csv(here::here("data-processed", "2026", "additional_indices", "lingcod", "wcgbts", "delta_gamma", "fit_1", "indices", "est_by_area.csv"))
lingcod_south$common_name <- "lingcod south"
config_lingcod_south <- configuration %>%
  dplyr::filter(species == "lingcod")
config_lingcod_south$species <- "lingcod south"
config_lingcod_south$min_latitude <- 31.9
config_lingcod_south$max_latitude <- 40.1667

#greenspotted_rockfish <- read.csv(here::here("data-processed", "2026", "additional_indices", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_1", "indices", "est_by_area.csv"))
#greenspotted_rockfish$common_name <- "greenspotted rockfish"
#config_greenspotted_rockfish <- configuration %>%
#  dplyr::filter(species == "greenspotted rockfish")

#greenstriped_rockfish <- read.csv(here::here("data-processed", "2026", "additional_indices", "greenstriped_rockfish", "wcgbts", "delta_gamma", "fit_1", "indices", "est_by_area.csv"))
#greenstriped_rockfish$common_name <- "greenstriped rockfish"
#config_greenstriped_rockfish <- configuration %>%
#  dplyr::filter(species == "greenstriped rockfish")

#rex_sole <- read.csv(here::here("data-processed", "2026", "additional_indices", "rex_sole", "wcgbts", "delta_gamma", "fit_3", "indices", "est_by_area.csv"))
#rex_sole$common_name <- "rex sole"
#config_rex_sole <- configuration %>%
#  dplyr::filter(species == "rex sole")
#config_rex_sole$share_range <- TRUE

#splitnose_rockfish <- read.csv(here::here("data-processed", "2026", "additional_indices", "splitnose_rockfish", "wcgbts", "delta_lognormal", "fit_3", "indices", "est_by_area.csv"))
#splitnose_rockfish$common_name <- "splitnose rockfish"
#config_splitnose_rockfish <- configuration %>%
#  dplyr::filter(species == "splitnose rockfish")
#config_splitnose_rockfish$family <- "sdmTMB::delta_lognormal()"
#config_splitnose_rockfish$spatiotemporal2 <- "iid"

#longspine_thornyhead <- read.csv(here::here("data-processed", "2026", "additional_indices", "longspine_thornyhead", "wcgbts", "delta_lognormal", "fit_4", "indices", "est_by_area.csv"))
#longspine_thornyhead$common_name <- "longspine thornyhead"
#config_longspine_thornyhead <- configuration %>%
#  dplyr::filter(species == "longspine thornyhead")
#config_longspine_thornyhead$family <- "sdmTMB::delta_lognormal()"
#config_longspine_thornyhead$formula <- "catch_weight ~ 0 + fyear + pass_scaled + depth_scaled + depth_scaled_squared"
#config_longspine_thornyhead$share_range <- FALSE
#config_longspine_thornyhead$anisotropy <- TRUE

rosethorn_rockfish <- read.csv(here::here("data-processed", "2026", "additional_indices", "rosethorn_rockfish", "wcgbts", "delta_gamma", "fit_2", "indices", "est_by_area.csv"))
rosethorn_rockfish$common_name <- "rosethorn rockfish"
config_rosethorn_rockfish <- configuration %>%
  dplyr::filter(species == "rosethorn rockfish")
config_rosethorn_rockfish$spatiotemporal1 <- "off"

shortspine_thornyhead <- read.csv(here::here("data-processed", "2026", "additional_indices", "shortspine_thornyhead", "wcgbts", "delta_lognormal", "fit_1", "indices", "est_by_area.csv"))
shortspine_thornyhead$common_name <- "shortspine thornyhead"
config_shortspine_thornyhead <- configuration %>%
  dplyr::filter(species == "shortspine thornyhead")

all_additonal_indices <- rbind(lingcod_south, rosethorn_rockfish, shortspine_thornyhead) %>%
  dplyr::filter(area == "Coastwide" & year != 2025) #will have to update this for year filter if we include 2025
readr::write_csv(all_additonal_indices, "data-processed/2026/additional_coastwide_indices.csv")

all_config_additional_indices <- rbind(config_lingcod_south, config_rosethorn_rockfish) #only ones that have changed or need to be added
readr::write_csv(all_config_additional_indices, "data-processed/2026/configuration_additional_coastwide_indices.csv")

