# Always: Load packages
library(sa4ss)

# Always: Specify the directory for the document
setwd(here::here(), "doc_2026")

# Render Call:
if(file.exists("_main.Rmd")){
	file.remove("_main.Rmd")
}
# Render the pdf
bookdown::render_book("00a.Rmd", clean=FALSE, output_dir = getwd())
