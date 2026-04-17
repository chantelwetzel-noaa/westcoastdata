# List of maturity parameter updates
maturity_parameters_list <- function(species_name) {
  maturity_parameters_text <- ""
  if (species_name == "black rockfish") {
    text <- paste0("Maturity parameter estimates would likely not be updated from those used in the previous assessment.")
    maturity_parameters_text <- glue::glue("{maturity_parameters_text} {text}")
  }
  
  if (species_name == "bocaccio") {
    text <- paste0("Maturity parameter estimates may be updated from those used in the previous assessment with additional maturity data and updated modeling methods.")
    maturity_parameters_text <- glue::glue("{maturity_parameters_text} {text}")
  }

  if (species_name == "lingcod north") {
    text <- paste0("Maturity parameter estimates may be updated from those used in the previous coastwide assessment with additional maturity data and updated modeling methods.")
    maturity_parameters_text <- glue::glue("{maturity_parameters_text} {text}")
  }
  
  if (species_name == "lingcod south") {
    text <- paste0("Maturity parameter estimates may be updated from those used in the previous coastwide assessment with additional maturity data and updated modeling methods.")
    maturity_parameters_text <- glue::glue("{maturity_parameters_text} {text}")
  }
  
  if (species_name == "longspine thornyhead") {
    text <- paste0("Maturity parameter estimates may be updated from those used in the previous assessment with additional maturity data and updated modeling methods, but assigning maturity status per sample is time consuming for this species.")
    maturity_parameters_text <- glue::glue("{maturity_parameters_text} {text}")
  }
  
  if (species_name == "Pacific spiny dogfish") {
    text <- paste0("Maturity parameter estimates may be updated from those used in the previous assessment with additional maturity data and updated modeling methods, but assigning maturity status per sample is time consuming for this species.")
    maturity_parameters_text <- glue::glue("{maturity_parameters_text} {text}")
  }
  
  if (species_name == "petrale sole") {
    text <- paste0("Maturity parameter estimates may be updated from those used in the previous coastwide assessment with updated modeling methods.")
    maturity_parameters_text <- glue::glue("{maturity_parameters_text} {text}")
  } 
  
  if (species_name == "redbanded rockfish") {
    text <- paste0("Maturity parameter estimates would be provided using available maturity data and accepted modeling methods.")
    maturity_parameters_text <- glue::glue("{maturity_parameters_text} {text}")
  } 

  if (species_name == "shortspine thornyhead") {
    text <- paste0("Maturity parameter estimates may be updated from those used in the previous assessment with additional maturity data and updated modeling methods.")
    maturity_parameters_text <- glue::glue("{maturity_parameters_text} {text}")
  } 
  
  if (species_name == "widow rockfish") {
    text <- paste0("Maturity parameter estimates may be updated from those used in the previous assessment with additional maturity data and updated modeling methods.")
    maturity_parameters_text <- glue::glue("{maturity_parameters_text} {text}")
  } 

  if (species_name == "yellowtail rockfish south") {
    text <- paste0("Maturity parameter estimates may be updated from those used in the previous coastwide assessment with additional maturity data and updated modeling methods.")
    maturity_parameters_text <- glue::glue("{maturity_parameters_text} {text}")
  }
  
  return(maturity_parameters_text)
}



#Black rockfish: 
#Maturity parameter estimates would likely not be updated from those used in the previous assessment.

#Bocaccio: 
#Maturity parameter estimates may be updated from those used in the previous assessment with additional maturity data and updated modeling methods.

#Lingcod north: 
#Maturity parameter estimates may be updated from those used in the previous coastwide assessment with additional maturity data and updated modeling methods.

#Lingcod south: 
#Maturity parameter estimates may be updated from those used in the previous coastwide assessment with additional maturity data and updated modeling methods.

#Longspine thornyhead: 
#Maturity parameter estimates may be updated from those used in the previous assessment with additional maturity data and updated modeling methods, but assigning maturity status per sample is time consuming for this species.

#Pacific spiny dogfish: 
#Maturity parameter estimates may be updated from those used in the previous assessment with additional maturity data and updated modeling methods, but assigning maturity status per sample is time consuming for this species.

#Petrale sole: 
#Maturity parameter estimates may be updated from those used in the previous coastwide assessment with updated modeling methods.

#Redbanded rockfish: 
#Maturity parameter estimates would be provided using available maturity data and accepted modeling methods.

#Shortspine thornyhead: 
#Maturity parameter estimates may be updated from those used in the previous assessment with additional maturity data and updated modeling methods.

#Widow rockfish: 
#Maturity parameter estimates may be updated from those used in the previous assessment with additional maturity data and updated modeling methods.

#Yellowtail rockfish south: 
#Maturity parameter estimates may be updated from those used in the previous coastwide assessment with additional maturity data and updated modeling methods.
