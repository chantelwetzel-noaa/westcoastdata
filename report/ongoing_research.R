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
      and not expected to be finish by winter of 2026 due to extremely large schools of young-of-the-year 
      rockfish observed on the camera and acoustics. ODFW is finishing a manuscript with  
      Dr. Dezhang Chu (NOAA-retired) developing target strength models for these species which can be considered 
      when using these data. Comparison studies of the previously used Biosonic echosounder 
      and the Scientific and Statistical Committee recommended Simrad echosounder are ongoing. 
      Currently, ODFW staff anticipate completing data analysis mid-2027."
      ),
      "\n\n"
    )
    text_additional <- paste0(
      glue::glue(
        "ODFW scientists are planning a research cruise to Cobb Sea Mount in the summer of 2026 to 
      sample age and sex distributions of this rockfish species and others. This will provide 
      additional information about age and sex distribution for {species_name} in areas that 
      are likely not sampled by commercial and recreational fisheries. The final research project 
      being led by ODFW scientists examines the impacts of hypoxia on the catch per unit effort 
      for {species_name}."
      ),
      "\n\n"
    )
    research_text <- glue::glue("{research_text} {text} {text_additional}")
  }

  if (species_name == "widow rockfish") {
    text <- paste0(
      "\n\n",
      glue::glue(
        "The Cooperative Ageing Program lab has aged 1,468 otoliths collected from commercial fisheries in California 
        between 2019-2024 that were not available for the 2025 update assessment. The large majority of these new ages 
        are from trawl gear, with 613 from the midwater trawl fishery and 825 from bottom trawl gear. The California midwater trawl 
        ages were collected from Eureka (n = 613) and the bottom trawl ages were collected from 
        Eureka (n = 199), Fort Bragg (n = 475), and Moss Landing (n = 151). The California ages from midwater trawl gear range 
        5-28 years with a median (i.e., the middle value in an ordered dataset, separating the higher half from the lower half) 
        age of 11. These new ages (1,468 samples) from California would be available for use in a future
        assessment and are included in the summaries below.
        
        For comparison, ages from midwater gear in Oregon and Washington collected during this same period (2019-2024),
        that were included in the last assessment, range from 3-39 years 
        with a median age of 8 in Oregon (2,106 ages) and from 1-37 years with a median age of 7 in Washington (4,117 ages)."
      ),
      "\n\n"
    )
    research_text <- glue::glue("{research_text} {text}")
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

  if (species_name == "yellowtail rockfish south") {
    ej_url <- "https://repository.library.noaa.gov/view/noaa/62673"
    sb_url1 <- "https://link.springer.com/article/10.1007/s10641-014-0238-7"
    sb_url2 <- "https://cdnsciencepub.com/doi/full/10.1139/cjfas-2023-0253"
    text <- paste0(
      "\n\n",
      glue::glue(
        "There are a number of research pojects that could be considered in a 
        future stock asessment of yellowtail rockfish south. Fecundity at size research
        found strong maternal effects on egg production [(Dick et. al, 2017)]({ej_url})
        as well as spatial variation [(Beyers et al, 2014)]({sb_url2}) and temporal
        variation [(Beyers et al, 2024)]({sb_url2}) in fecundity. Available maturity
        samples could also provide additional understanding of maturity-at-length and 
        -age and how that maturity compares to the northern stocks.  Finally, research
        using life history information to estimate stock productivity (e.g., steepness)
        directly from life history parameters and an estimate of early life survey is
        in final review (Beyer et al., in review) which would be available for consideration for a 2027 stock
        assessment."
      ),
      "\n\n"
    )
    research_text <- glue::glue("{research_text} {text}")
  }

  if (species_name == "Pacific spiny dogfish") {
    url <- "https://onlinelibrary.wiley.com/doi/full/10.1111/faf.70028"
    url_ketchen <- "https://cdnsciencepub.com/doi/10.1139/f72-272"
    url_cheng <- "http://www.ceser.in/ceserp/index.php/ijamas/article/view/651"
    text <- paste0(
      "\n\n",
      glue::glue(
        "Research has identified similar declining population trends across regions in the
      North Pacific (i.e., Alaska, Canada, and the U.S. West Coast) for 
      Pacific spiny dogfish [(Davidson et al. 2026)]({url}).
      There has been additional research focusing on understanding movement
      of Pacific spiny dogfish. Since 2024, ODFW in collaboration with 
      Oregon State University, has tagged 76 Pacific spiny dogfish with satellite tags in Washington and Oregon. 
      To date 46 of those tags have been retreived and are providing information. Likelihood profiles 
      for each tags' track are being developed to determine movements of the individuals. 
      The goal is to understand the movement of Pacific spiny dogfish across seasons and depth which 
      will provide improved understanding about availability to summer surveys and 
      bottom trawl gear off the U.S. West Coast."
      ),
      "\n\n",
      glue::glue(
        "The Washington Department of Fish and Wildlife (WDFW) is conducting a study to improve age estimation 
       methods for Pacific Spiny Dogfish using vertebral centra and dorsal spine–based techniques. A key 
       challenge in accurately determining age for this species is accounting for missing growth bands 
       caused by natural wear and spine breakage. Wear is most frequently observed in older individuals and 
       as individuals age, where natural processes and cumulative physical damage to the spines can obscure 
       or completely remove early growth bands, potentially leading to systematic underestimation of age."
      ),
      "\n\n",
      glue::glue(
        "Two methods (i.e., [Ketchen, 1975]({url_ketchen}), and [Cheng, 2012]({url_cheng})) have been developed to account for the worn 
        portion of the spine and to extrapolate missing growth bands. The Ketchen method incorporates 
        an assumed diameter of the spine at birth, whereas the latter does not, allowing for extrapolation 
        without accounting for the birth point. However, the Ketchen method does not account for natural 
        variability in spine growth among individuals, whereas the Cheng method uses a statistical approach 
        that accounts for individual variability. Despite these differences, both approaches introduce biases 
        that can lead to overestimation of age when reconstructing missing growth bands."
      ),
      "\n\n",
      glue::glue(
        "WDFW is currently evaluating age estimates derived from vertebrae and spines through several approaches:
         1) assessing ageing criteria for vertebral centra and spines (e.g., interpretation of banding patterns, 
         including lumping versus splitting of bands); 2) determining whether vertebral centra provide more reliable 
         age estimates and, if so, how they can be used to correct bias on damaged spines; 3) evaluating whether 
         spine measurements (e.g., diameter at the first band, diameter at the second band, diameter at the third 
         band, diameter at the base) can be used to improve reconstruction of missing growth bands when vertebral 
         centra are not suitable; and 4) exploring multiple approaches for indirect age validation, such as eye 
         lens diameter or chemical analysis."
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
    text <- paste0(
      "\n\n",
      glue::glue(
        "ODFW scientists are planning a research cruise to Cobb Sea Mount in the summer of 2026 to 
      sample age and sex distributions of this rockfish species and others."
      )
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
    text <- paste0(
      "\n\n",
      glue::glue(
        "ODFW scientists have conducted research looking at the influences of hypoxia on the catch per unit effort for {species_name}."
      )
    )
    research_text <- glue::glue("{research_text} {text}")
  }
  return(research_text)
}
