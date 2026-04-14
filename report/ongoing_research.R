# List of known ongoing research projects
research_list <- function(species_name) {
  research_text <- ""
  if (species_name %in% c("black rockfish")) {
    text <- paste0(
      "\n\n",
      glue::glue(
        "There are a number of ongoing research projects being led by ODFW that may inform a future 
      assessment of {species_name}. The first research project is another year of  the nearshore 
      visual-acoustic survey for semipelagic rockfish completed in 2025. During the 2025 survey, 
      no hook and line sampling occurred due to cost constraints. Data analysis is on going 
      and not expected to be finish by winter of 2026 due to extremelylarge schools of young-of-the-year 
      rockfish observed on the camera and acoustics. ODFW is finishing a manuscript with NOAA-retired 
      Dr. Dezhang Chu developing target strength models for these species which can be considered 
      when using these data. Comparison studies of the previously used Biosonic echosounder 
      and the Scientific and Statistical Committee recommended Simrad echosounder are on going. 
      Currently, ODFW anticipate completing data analysis mid-2027."
      ),
      "\n\n"
    )
    text_additional <- paste0(
      glue::glue(
        "ODFW scientists are planning a research cruise to Cobb Sea Mount in the summer of 2026 to 
      sample age and sex distributions of this rockfish species and others. This will provide 
      additional information about age and sex distribution for {species_name} in areas that 
      are likely not sampled by commercial and recreational fisheries.  The final research project 
      being led by ODFW scientists is examines the impacts of hypoxia on the catch per unit effort 
      for {species_name}."
      ),
      "\n\n"
    )
    research_text <- glue::glue("{research_text} {text} {text_additional}")
  }

  if (species_name == "China rockfish") {
    text <- glue::glue(
      "There is ongoing research on otolith morphology and stock structure of {species_name} in Oregon 
      waters being led by ODFW. This research is expected to conclude in 2024. Additionally, 
      larval drift modeling in concert with population genetics of {species_name} has be conducted by 
      researchers at Oregon State University."
    )
    research_text <- glue::glue("{research_text} {text}")
  }

  if (species_name == "Pacific spiny dogfish") {
    url <- "https://onlinelibrary.wiley.com/doi/full/10.1111/faf.70028"
    text <- paste0(
      "\n\n",
      glue::glue(
        "Research has identified similar declining population trends across regions in the
      North Pacific (i.e., Alaska, Canada, and the U.S. West Coast) for 
      Pacific spiny dogfish [(Davidson et al. 2026)]({url}).
      There has been additional research focusing on understanding movement
      of Pacific spiny dogfish. Since 2024, ODFW in collaboration with 
      Oregon State University, has tagged 76 Pacific spiny dogfish with satellite tags in Washington and Oregon. 
      To date 46 of those tags have retreived and are providing information. Likelihood profiles 
      for each tags track are being developed to determine movements of the individuals. 
      The goal is to understand the Pacific spiny dogfish movement across seasons and depth which 
      will provide improved understanding about availability to summer surveys and 
      bottom trawl gear off the U.S. West Coast."
      ),
      "\n\n"
    )
    research_text <- glue::glue("{research_text}{text}")
  }

  if (
    species_name %in%
      c(
        "yellowtail rockfish north",
        "canary rockfish"
      )
  ) {
    text <-
      glue::glue(
        "ODFW scientists are planning a research cruise to Cobb Sea Mount in the summer of 2026 to 
      sample age and sex distributions of this rockfish species and others."
      )
    research_text <- glue::glue("{research_text} {text}")
  }

  if (
    species_name %in%
      c(
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
