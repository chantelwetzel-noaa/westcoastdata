mat <- read.csv('C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/data-raw/maturity_totals.csv')

maturity_text <- data.frame(
  species = mat$Species,
  collected = mat$Count,
  read = mat$Read,
  text_to_add = rep("", length(mat$Species))
)
