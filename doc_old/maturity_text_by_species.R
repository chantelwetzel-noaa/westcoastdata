mat <- read.csv('C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/data-raw/maturity_totals.csv')

maturity_text <- data.frame(
  species = mat$Species,
  collected = mat$Count,
  read = mat$Read,
  text_to_add = rep("", length(mat$Species))
)

maturity_text$text_to_add[maturity_text$species == "quillback rockfish"] <-
  "There are ongoing additional data collections led by SWFSC staff being conducted this year 
  to inform maturity and fecundity of quillback rockfish in California waters. Any samples
  collected by these efforts will be available to inform a potential 2025 assessment."


maturity_text$text_to_add[maturity_text$species == "rougheye and blackspotted rockfish"] <-
  "Research being led by the NWFSC has incorporated genetic samples to inform species-specific
  maturity for rougheye rockfish and blackspotted rockfish that could inform a potential 2025 assessment."

maturity_text$text_to_add[maturity_text$species %in% c("yellowtail rockfish south", "yellowtail rockfish north")] <- 
  "There is ongoing research being led by staff at the NWFSC to estimate spatial and temporal biological and functional length-at-maturity
 for yellowtail rockfish across the coast that will be available for a potential 2025 assessment."