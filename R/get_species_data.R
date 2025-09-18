#' Define which data to plot/show based upon species
#'
#'
#' @author Chantel Wetzel
#' @export
#'
#'
get_species_data <- function() {
  data_species = t(data.frame(
    c("com_wcgbt", "arrowtooth flounder"),
    c("com_wcgbt", "aurora rockfish"),
    c("all", "bank rockfish"),
    c("com_wcgbt", "big skate"),
    c("com_rec", "black rockfish"),
    c("com_wcgbt", "blackgill rockfish"),
    c("com_rec_hkl", "blue and deacon rockfish"),
    c("all", "bocaccio"),
    c("com_rec_wcgbt", "brown rockfish"),
    c("com_rec", "cabezon"),
    c("all", "California scorpionfish"),
    c("all", "canary rockfish"),
    c("all", "chilipepper"),
    c("com_rec", "China rockfish"),
    c("all", "copper rockfish"),
    c("all", "cowcod"),
    c("com_wcgbt", "curlfin sole"),
    c("com_wcgbt", "darkblotched rockfish"),
    c("com_wcgbt", "Dover sole"),
    c("com_wcgbt", "English sole"),
    c("all", "flag rockfish"),
    c("com_wcgbt", "flathead sole"),
    c("com_rec", "gopher and black and yellow rockfish"),
    c("com_rec", "grass rockfish"),
    c("all", "greenspotted rockfish"),
    c("all", "greenstriped rockfish"),
    c("rec_wcgbt_hkl", "honeycomb rockfish"),
    c("com_rec_wcgbt", "kelp greenling"),
    c("com_rec", "kelp rockfish"),
    c("rec", "leopard shark"),
    c("all", "lingcod"),
    c("com_wcgbt", "longnose skate"),
    c("com_wcgbt", "longspine thornyhead"),
    c("com_rec_hkl", "olive rockfish"),
    c("com_rec_wcgbt", "Pacific cod"),
    c("com_wcgbt", "Pacific ocean perch"),
    c("com_rec_wcgbt", "Pacific sanddab"),
    c("com_rec_wcgbt", "Pacific spiny dogfish"),
    c("com_wcgbt", "petrale sole"),
    c("com_rec_wcgbt", "quillback rockfish"),
    c("com_wcgbt", "redbanded rockfish"),
    c("rec_wcgbt", "redstripe rockfish"),
    c("com_wcgbt", "rex sole"),
    c("com_rec", "rock sole"),
    c("all", "rosethorn rockfish"),
    c("all", "rosy rockfish"),
    c("com_wcgbt", "rougheye and blackspotted rockfish"),
    c("com_wcgbt", "sablefish"),
    c("com_rec_wcgbt", "sand sole"),
    c("com_wcgbt", "sharpchin rockfish"),
    c("com", "shortraker rockfish"),
    c("com_wcgbt", "shortspine thornyhead"),
    c("com_rec_wcgbt", "silvergray rockfish"),
    c("all", "speckled rockfish"),
    c("com_wcgbt", "splitnose rockfish"),
    c("all", "squarespot rockfish"),
    c("com_rec_wcgbt", "starry flounder"),
    c("all", "starry rockfish"),
    c("rec_wcgbt", "stripetail rockfish"),
    c("com_rec", "treefish"),
    c("all", "vermilion and sunset rockfish"),
    c("all", "widow rockfish"),
    c("all", "yelloweye rockfish"),
    c("com_wcgbt", "yellowmouth rockfish"),
    c("all", "yellowtail rockfish"),
    c("com_rec_wcgbt", "yellowtail rockfish north"),
    c("com_rec_hkl", "yellowtail rockfish south")
  ))

  data_species <- as.data.frame(data_species)
  colnames(data_species) <- c('sources_to_use', 'Common_name')
  rownames(data_species) <- NULL
  return(data_species)
}
