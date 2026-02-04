library(dplyr)

raw_url <- "https://raw.githubusercontent.com/pfmc-assessments/indexwc/refs/heads/main/data-raw/configuration.csv"
configuration <- read.csv(raw_url)

longspine_thornyhead

lingcod_south <- read.csv(here::here("data-processed", "2026", "additional_indices", "lingcod", "wcgbts", "delta_gamma", "fit_1", "indices", "est_by_area.csv"))
lingcod_south$common_name <- "lingcod south"
config_lingcod_south <- configuration %>%
  dplyr::filter(species == "lingcod")
config_lingcod_south$species <- "lingcod south"
config_lingcod_south$min_latitude <- 31.9
config_lingcod_south$max_latitude <- 40.1667

greenspotted_rockfish <- read.csv(here::here("data-processed", "2026", "additional_indices", "greenspotted_rockfish", "wcgbts", "delta_lognormal", "fit_1", "indices", "est_by_area.csv"))
greenspotted_rockfish$common_name <- "greenspotted rockfish"
config_greenspotted_rockfish <- configuration %>%
  dplyr::filter(species == "greenspotted rockfish")

greenstriped_rockfish <- read.csv(here::here("data-processed", "2026", "additional_indices", "greenstriped_rockfish", "wcgbts", "delta_gamma", "fit_1", "indices", "est_by_area.csv"))
greenstriped_rockfish$common_name <- "greenstriped rockfish"
config_greenstriped_rockfish <- configuration %>%
  dplyr::filter(species == "greenstriped rockfish")

rex_sole <- read.csv(here::here("data-processed", "2026", "additional_indices", "rex_sole", "wcgbts", "delta_gamma", "fit_1", "indices", "est_by_area.csv"))
rex_sole$common_name <- "rex sole"
config_rex_sole <- configuration %>%
  dplyr::filter(species == "rex sole")

shortspine_thornyhead

splitnose_rockfish



all_additonal_indices <- rbind(lingcod_south, greenspotted_rockfish, greenstriped_rockfish, rex_sole) %>%
  dplyr::filter(area == "Coastwide" & year != 2025) #will have to update this for year filter if we include 2025
#write csv

all_config_additional_indices <- rbind(config_lingcod_south) #only ones that have changed or need to be added
#write csv
