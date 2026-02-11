#' Define which data to plot/show based upon species
#'
#'
#' @author Chantel Wetzel
#' @export
#'
#'
get_species_data_survey <- function() {
  data_species = t(data.frame(
    c("wcgbt", "arrowtooth flounder"),
    c("wcgbt", "aurora rockfish"),
    c("all", "bank rockfish"),
    c("wcgbt", "big skate"),
    c("wcgbt", "black rockfish"),
    c("all", "blackgill rockfish"),
    c("all", "bocaccio"),
    c("all", "California scorpionfish"),
    c("all", "canary rockfish"),
    c("all", "chilipepper"),
    c("all", "cowcod"),
    c("wcgbt", "darkblotched rockfish"),
    c("wcgbt", "Dover sole"),
    c("wcgbt", "English sole"),
    c("wcgbt", "flathead sole"),
    c("all", "greenspotted rockfish"),
    c("all", "greenstriped rockfish"),
    c("wcgbt", "kelp greenling"),
    c("wcgbt", "lingcod north"),
    c("all", "lingcod south"),
    c("wcgbt", "longnose skate"),
    c("wcgbt", "longspine thornyhead"),
    c("wcgbt", "Pacific cod"),
    c("wcgbt", "Pacific ocean perch"),
    c("wcgbt", "Pacific sanddab"),
    c("all", "Pacific spiny dogfish"),
    c("all", "petrale sole"),
    c("wcgbt", "quillback rockfish"),
    c("wcgbt", "redbanded rockfish"),
    c("wcgbt", "redstripe rockfish"),
    c("wcgbt", "rex sole"),
    c("all", "rosethorn rockfish"),
    c("wcgbt", "rougheye and blackspotted rockfish"),
    c("wcgbt", "sablefish"),
    c("all", "sharpchin rockfish"),
    c("wcgbt", "shortraker rockfish"),
    c("wcgbt", "shortspine thornyhead"),
    c("all", "silvergray rockfish"),
    c("wcgbt", "splitnose rockfish"),
    c("all", "squarespot rockfish"),
    c("all", "starry rockfish"),
    c("all", "stripetail rockfish"),
    c("all", "vermilion and sunset rockfish"),
    c("all", "widow rockfish"),
    c("all", "yelloweye rockfish"),
    c("wcgbt", "yellowmouth rockfish"),
    c("wcgbt", "yellowtail rockfish north"),
    c("all", "yellowtail rockfish south")
  ))

  data_species <- as.data.frame(data_species)
  colnames(data_species) <- c('sources_to_use', 'Common_name')
  rownames(data_species) <- NULL
  return(data_species)
}
