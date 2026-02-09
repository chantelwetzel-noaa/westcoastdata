library(dplyr)
library(sdmTMB)
library(ggplot2)

source(here::here("sandbox", "diagnostics.R"))
source(here::here("sandbox", "do_diagnostics.R"))
source(here::here("sandbox", "format_hkl_data.R"))
source(here::here("sandbox", "format_index.R"))
source(here::here("sandbox", "get_index.R"))
source(here::here("sandbox", "match.f.R"))
source(here::here("sandbox", "plot_betas.R"))
source(here::here("sandbox", "plot_index.R"))
source(here::here("sandbox", "plot_qq_indexwc.R"))
source(here::here("sandbox", "sdmtmb_run_hkl_indices.R"))

species <- get_hkl_species() #should I change this function to include the correct species names?
#Lingcod to lingcod south, vermilion to vermilion and sunset rockfish, yellowtail rockfish to yellowtail rockfish south

hkl_data <- read.csv(here::here(
  "data-raw",
  "2026",
  "nwfsc_hkl_DWarehouse_version_abbreviated_02032026.csv"
))

# why can't I used the cleaned version of this
# The format_hkl_data code expects certain column names and we would need to
# modify that code in order to use the cleaned data object
for (sp in unique(species)) {
  species_data <- format_hkl_data(
    common_name = sp,
    data = hkl_data
  )

  run_hkl_index(
    dir = here::here("plots", "hkl_indices"),
    data = species_data
  )
}

#change the names of the files here for lingcod, vermilion, and yellowtail
files <- list.files(here::here("plots", "hkl_indices"), full.names = TRUE)

newfilenames <- tibble(files) |>
  dplyr::mutate(
    speciesname = basename(files),
    newspeciesname = dplyr::case_when(
      stringr::str_detect(speciesname, "^lingcod") ~
        stringr::str_replace(speciesname, "^lingcod", "lingcod south"),
      stringr::str_detect(speciesname, "^vermilion") ~
        stringr::str_replace(
          speciesname,
          "^vermilion rockfish",
          "vermilion and sunset rockfish"
        ),
      stringr::str_detect(speciesname, "^yellowtail") ~
        stringr::str_replace(
          speciesname,
          "^yellowtail rockfish",
          "yellowtail rockfish south"
        ),
      TRUE ~ speciesname
    ),
    newfilepath = file.path(here::here("plots", "hkl_indices"), newspeciesname)
  )

with(newfilenames, file.rename(files, newfilepath))
