
multiplot_hkl <- function(species_name){
	
	glue::glue(" \n## {species_name} \n \n") %>% cat()

	sub_data <- data %>%
		filter(common_name == species_name)
	glue::glue("The NWFSC HKL survey has a total of {sum(sub_data$lengthed)} length observations and {sum(sub_data$aged_fish)} of aged fish for {species_name}. \n") %>% cat()		
	
	##find <- grep(species_name, wcgbt$Common_name, ignore.case = TRUE)
	colnames(sub_data)[-1] = c("Year", "Positive Sitex", "Sexed Fish", "Unsexed Fish", "Lengths", "Read Ages", "Otoliths")

	t = table_format(x = sub_data[,-1], 
		caption = "Data collected annually from the NWFSC HKL survey.",
		label = 'tab-label',
		align = 'l')

	print(t)
  
  	cat("  \n  \n")
}