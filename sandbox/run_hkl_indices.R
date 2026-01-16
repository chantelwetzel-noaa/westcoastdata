run_hkl_index <- function(dir = here::here("plots-index"), data) {
  species_name <- tolower(unique(data$common_name))

  subdata <- data |>
    dplyr::group_by(year, site_number, drop) |>
    dplyr::reframe(
      n = sum(number_caught),
      depth = median(drop_depth_meters),
      lat = mean(drop_latitude_degrees),
      lon = mean(drop_longitude_degrees),
      effort = length(unique(angler)) * length(unique(hook))
    ) |>
    dplyr::mutate(
      year = as.factor(year),
      site = as.factor(site_number),
      drop = as.factor(drop),
      depth_scaled = (depth - mean(depth)) / sd(depth),
      depth_scaled_2 = depth_scaled^2
    )

  # Year and Sites
  year_site <- expand.grid(
    year = unique(subdata$year),
    site_number = unique(subdata$site_number)
  )

  ## join in location info for all sites
  locs <- dplyr::group_by(subdata, site_number) |>
    dplyr::summarise(
      lat = lat[1],
      lon = lon[1],
      drop = drop[1]
    )

  grid <- dplyr::left_join(year_site, locs) |>
    dplyr::filter(!is.na(lat + lon))

  # Negative binomial model=============================================
  fit <- sdmTMB(
    n ~ 0 + year + site_number + drop,
    data = subdata,
    offset = log(subdata$effort),
    time = "year",
    spatial = "off",
    spatiotemporal = "off",
    family = nbinom2(link = "log")
  )

  if (fit$pos_def_hessian == TRUE) {
    index <- calc_index(
      dir = dir,
      fit = fit,
      add_name = paste0(tolower(species_name), '_negbinom'),
      grid = grid
    )

    do_diagnostics(
      dir = dir,
      add_name = paste0(tolower(species_name), '_negbinom'),
      fit = fit
    )
  }

  # Delta model ================================================================
  # fit_delta <- sdmTMB(
  #   n  ~ 0 + year + site_number + drop,
  #   data = subdata,
  #   offset = log(subdata$effort),
  #   time = "year",
  #   spatial="off",
  #   spatiotemporal = "off",
  #   family = delta_gamma()
  # )
  #
  # if(fit_delta$pos_def_hessian == TRUE){
  #   index <- calc_index(
  #     dir = dir,
  #     fit = fit_delta,
  #     add_name = paste0(species_name, "_delta_gamma"),
  #     grid = grid)
  #
  #   qq_indexwc(
  #     fit = fit_delta,
  #     file_name = file.path(dir, paste0(species_name, "_delta_gamma_qq.png"))
  #   )
  # }
}

devtools::load_all()
library(dplyr)
library(sdmTMB)
#library(indexwc)
#remotes::install_github("pfmc-assessments/indexwc@sap-indices")
library(ggplot2)

all <- list.files(here::here("sandbox"))
for (a in 1:length(all)) {
  source(here::here("sandbox", all[a]))
}

species <- get_hkl_species() #should I change this function to include the correct species names?

hkl_data <- read.csv(here::here(
  "data-raw",
  "2026",
  "nwfsc_hkl_DWarehouse_version_09032025.csv"
))
#in data, change lingcod to lingcod south, vermilion to vermilion and sunset rockfish, and yellowtail to yellowtail rockfish south
hkl_data <- hkl_data |>
dplyr::rename_with(tolower) |>
  dplyr::mutate(
    lower_name = tolower(common_name),
    Common_name = dplyr::case_when(
      lower_name == "yellowtail rockfish" ~ "yellowtail rockfish south",
      lower_name == "blackspotted rockfish" ~
        "rougheye and blackspotted rockfish",
      lower_name == "blue rockfish" ~ "blue and deacon rockfish",
      lower_name == "spiny dogfish" ~ "Pacific spiny dogfish",
      lower_name == "vermilion rockfish" ~ "vermilion and sunset rockfish",
      lower_name == "california scorpionfish" ~ "California scorpionfish",
      lower_name == "lingcod" ~ "lingcod south",
      .default = lower_name
    )
  )

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
