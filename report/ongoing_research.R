# List of known ongoing research projects
research_list <- function(species_name) {
  research_text <- ""
  if (species_name %in% c("blue and deacon rockfish", "black rockfish")) {
    text <- glue::glue(
      "ODFW conducted their acoustic visual survey in 2025 that will provide an additional measure of  absolute abundance of {species_name} in Oregon waters."
    )
    research_text <- glue::glue("{research_text} {text}")
  }

  if (species_name == "China rockfish") {
    text <- glue::glue(
      "There is ongoing research on otolith morphology and stock structure of {species_name} in Oregon waters being led by ODFW. This research is expected to conclude in 2024. Additionally, larval drift modeling in concert with population genetics of {species_name} has be conducted by researchers at Oregon State University."
    )
    research_text <- glue::glue("{research_text} {text}")
  }

  if (species_name == "Pacific spiny dogfish") {
    #glue::glue(
    #  "Tagging studies of are currently being conducted to better understand the
    #         movement of {species_name} off the U.S. West Coast. This research is being conducted by scientists
    #         at the NWFSC and ODFW and is anticipated to conclude in 2025."
    #)
    text <- glue::glue(
      "Researchers have conducted an analysis of the trends of {species_name} across the North Pacific indicating declining population trends across regions."
    )
    research_text <- glue::glue("{research_text} {text}")
  }

  if (
    species_name %in%
      c(
        "black rockfish",
        "yellowtail rockfish north",
        "canary rockfish"
      )
  ) {
    text <- glue::glue(
      "ODFW scientists are planning a research cruise to Cobb Sea Mount in the summer of 2026 to sample age and sex distributions of rockfish species."
    )
    research_text <- glue::glue("{research_text} {text}")
  }

  if (
    species_name %in%
      c(
        "black rockfish",
        "blue and deacon rockfish",
        "China rockfish",
        "kelp greenling",
        "lingcod north",
        "quillback rockfish"
      )
  ) {
    text <- glue::glue(
      "ODFW scientists have conducted research looking at the influences of hypoxia on the catch per unit effort for {species_name}."
    )
    research_text <- glue::glue("{research_text} {text}")
  }
  return(research_text)
}
