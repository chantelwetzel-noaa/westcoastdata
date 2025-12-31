#' Define which data to plot/show based upon species
#'
#'
#' @author Chantel Wetzel
#' @export
#'
#'
get_species_data <- function() {
  data_species = t(data.frame(
    c("wcgbt", "arrowtooth flounder"),
    c("wcgbt", "aurora rockfish"),
    c("all", "bank rockfish"),
    c("wcgbt", "big skate"),
    c("wcgbt", "black rockfish"),
    c("wcgbt", "blackgill rockfish"),
#     blackspotted?
#     blue is hkl
#    c("com_rec_hkl", "blue and deacon rockfish"),
    c("all", "bocaccio"),
#    c("com_rec_wcgbt", "brown rockfish"),
    c("wcbgt", "cabezon"),
    c("hkl", "California scorpionfish"),
    c("all", "canary rockfish"),
    c("all", "chilipepper"),
#    c("com_rec", "China rockfish"),
    c("wcgbt", "copper rockfish"),
    c("all", "cowcod"),
#    c("com_wcgbt", "curlfin sole"),
    c("wcgbt", "darkblotched rockfish"),
    c("wcgbt", "Dover sole"),
    c("wcgbt", "English sole"),
#    c("all", "flag rockfish"),
    c("wcgbt", "flathead sole"),
#    c("com_rec", "gopher and black and yellow rockfish"),
#    c("com_rec", "grass rockfish"),
    c("all", "greenspotted rockfish"),
    c("all", "greenstriped rockfish"),
#    c("rec_wcgbt_hkl", "honeycomb rockfish"),
    c("wcgbt", "kelp greenling"),
#    c("com_rec", "kelp rockfish"),
#    c("rec", "leopard shark"),
    c("all", "lingcod"),
    c("wcgbt", "longnose skate"),
    c("wcgbt", "longspine thornyhead"),
#    c("com_rec_hkl", "olive rockfish"),
    c("wcgbt", "Pacific cod"),
    c("wcgbt", "Pacific ocean perch"),
    c("wcgbt", "Pacific sanddab"),
    c("wcgbt", "Pacific spiny dogfish"),
    c("wcgbt", "petrale sole"),
    c("wcgbt", "quillback rockfish"),
    c("wcgbt", "redbanded rockfish"),
    c("wcgbt", "redstripe rockfish"),
    c("wcgbt", "rex sole"),
#    c("com_rec", "rock sole"),
    c("all", "rosethorn rockfish"),
#    c("all", "rosy rockfish"),
    c("wcgbt", "rougheye and blackspotted rockfish"),
    c("wcgbt", "sablefish"),
#    c("com_rec_wcgbt", "sand sole"),
    c("wcgbt", "sharpchin rockfish"),
    c("wcgbt", "shortraker rockfish"),
    c("wcgbt", "shortspine thornyhead"),
    c("wcgbt", "silvergray rockfish"),
    c("all", "speckled rockfish"),
#   spiny dogfish? hkl
    c("wcgbt", "splitnose rockfish"),
    c("all", "squarespot rockfish"),
#    c("com_rec_wcgbt", "starry flounder"),
    c("all", "starry rockfish"),
    c("wcgbt", "stripetail rockfish"),
#    c("com_rec", "treefish"),
#    c("wcgbt", "vermilion and sunset rockfish"),
#   vermilion? hkl, need to check if this should be changed to vermilion and susut in data filtering
    c("all", "widow rockfish"),
    c("all", "yelloweye rockfish"),
    c("wcgbt", "yellowmouth rockfish"),
#    c("all", "yellowtail rockfish"),
    c("wcgbt", "yellowtail rockfish north"),
    c("all", "yellowtail rockfish south")
  ))

  data_species <- as.data.frame(data_species)
  colnames(data_species) <- c('sources_to_use', 'Common_name')
  rownames(data_species) <- NULL
  return(data_species)
}
