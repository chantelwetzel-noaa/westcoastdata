# List of known ongoing research projects
research_list <- function(species_name){

if (species_name %in% c("black rockfish", "blue and deacon rockfish")){
  glue::glue("ODFW is currently planning another acoustic visual survey for 2025 that will provide an additional measure of
             absolute abundance of {species_name} in Oregon waters.") |> cat()
}


if (species_name == "China rockfish"){
  glue::glue("There is ongoing research on otolith morphology and stock structure of {species_name} 
             in Oregon waters being led by ODFW. This research is expected to conclude in 2024.")
  
  glue::glue("Additionally, larval drift modeling in concert with population genetics of {species_name} has be
             conducted by researchers at Oregon State University.") |> cat()
}

if (species_name == "Pacific spiny dogfish"){
  glue::glue("Tagging studies of are currently being conducted to better understand the 
             movement of {species_name} off the U.S. West Coast. This research is being conducted by scientists
             at the NWFSC and ODFW and is anticipated to conclude in 2025.") |> cat()
}


if (species_name == "rougheye and blackspotted rockfish"){
  glue::glue("Research to better understand the species-specific biology of rougheye rockfish and blackspotted
             rockfish and the proportion of each species observed off the West Coast in select data sets (NWFSC WCGBT, At-sea Pacific
             hake fishery, West Coast Groundfish Observer Program, and ODFW) has been conducted by researchers
             at the NWFSC. Some of the key determinations from this research are 1) across all data sources approximately 85 percent
             and 15 percent of the genetic samples were determined to be rougheye rockfish and blackspotted rockfish, respectively,
             2) the rate of field identification erorr in the NWFSC WCGBT survey samples is 10 percent for blackspotted 
             rockfish, 3) the NWFSC WCGBT survey observes very few blackpotted rockfish of 25 cm or less compared to observations
             of rougheye rockfish of similar sizes, and 4) there are differences in otolith weights and growth between the 
             two species (personal communication, Peter Frey, NWFSC).") |> cat()
}

if (species_name == "sablefish"){
  glue::glue("Time-varying estimates of weight-at-age of {species_name} are currently being developed by researchers
             at the NWFSC which can better account for growth variability in future assessments.") |> cat()
}


if (species_name == "yelloweye rockfish"){
  glue::glue("A tagging study examining {species_name} movement in Oregon waters (Stonewall Bank) has been conducted 
             and subsequent species distribution model is underway led by ODFW and NWFSC researchers.") |> cat()
}
  
}