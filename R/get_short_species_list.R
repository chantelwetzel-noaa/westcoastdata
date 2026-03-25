#' List of select species to summarize commercial and recreational data for
#'
#' @author Chantel Wetzel
#' @export
#'
get_short_species_list <- function() {
  species <- rbind(
    c("black rockfish", "black rockfish"),
    c("petrale sole", "petrale sole"),
    c("lingcod", "lingcod"),
    c("lingcod north", "lingcod north"),
    c("lingcod south", "lingcod south"),
    c("redbanded rockfish", "redbanded rockfish"),
    c("yellowtail rockfish", "yellowtail rockfish"),
    c("yellowtail rockfish north", "yellowtail rockfish north"),
    c("yellowtail rockfish south", "yellowtail rockfish south"),
    c("widow rockfish", "widow rockfish"),
    c("Pacific spiny dogfish", "Pacific spiny dogfish"),
    c("pacific spiny dogfish", "Pacific spiny dogfish"),
    c("spiny dogfish", "Pacific spiny dogfish"),
    c("dogfish shark", "Pacific spiny dogfish"),
    c("shortspine thornyhead", "shortspine thornyhead"),
    c("bocaccio", "bocaccio"),
    c("longspine thornyhead", "longspine thornyhead"),
    c("stripetail rockfish", "stripetail rockfish")
  )

  colnames(species) <- c("name", "use_name")

  return(species)
}
