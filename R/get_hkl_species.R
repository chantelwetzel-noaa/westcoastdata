#' List of species name to estimate indices for NWFSC HKL survey data
#'
#'
#' @author Chantel Wetzel
#' @export
#'
#'
get_hkl_species <- function() {
  species_list = c(
    "Bank Rockfish", # 3143
    "Bocaccio", #18899
    "Chilipepper", # 2389
    "Cowcod", #  856
    "Greenspotted Rockfish", # 5347
    "Greenstriped Rockfish", #  942
#    "Lingcod", #  938
    "Lingcod South",
    "Squarespot Rockfish", # 1934
    "Starry Rockfish", # 2721
#    "Vermilion Rockfish", #27812
    "Vermilion and Sunset Rockfish",
#    "Yellowtail Rockfish", # 1794
    "Yellowtail Rockfish South"
  )

  return(species_list)
}
