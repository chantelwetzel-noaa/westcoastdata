
setwd("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/indices")

load("C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/data-raw/wcgbt_catch_filtered.Rdata")

wcgbt_species <- nwfscSurvey::GetSpp.fn(species = nwfscSurvey::GetSppDefault.fn())
species_to_remove <- c("copper rockfish", "quillback rockfish", "brown rockfish", "squarespot rockfish", "bank rockfish")
wcgbt_species <- wcgbt_species[which(!wcgbt_species$common %in% species_to_remove), ]
wcgbt_species[which(wcgbt_species$common == "rougheye rockfish"), "common"] <- "rougheye and blackspotted rockfish"
species_to_run <- unique(wcgbt_species[which(wcgbt_species[, "common"] %in% catch$Common_name), "common"])

# aurora
# big skate

for (sp in species_to_run[7:8]){

  sp_out <- gsub(" ", "_", sp)
  sp_out <- gsub(" ", "_", sp_out)

  out_file <- file.path(getwd(), paste0(sp_out, "_gamma_index.png"))
  smooth <- FALSE

  data <- read.csv(paste0(
  	"C:/Users/Chantel.Wetzel/Documents/GitHub/data_summary/indices/",
  	sp_out,
  	"/wcgbts/delta_gamma/",
  	sp_out,
  	"/wcgbts/delta_gamma/index/",
  	"est_by_area.csv"))

  data <- data[data$area == "WA", ]
  years =   as.numeric(as.character(data$year))
  sdmtmb_est <- data[,'est']
  hi_sdmtmb <- data[, "upr"]
  lo_sdmtmb <- data[, "lwr"]
  tmp <- data.frame(Year = years, 
                   Median = as.numeric(sdmtmb_est))
  
  grDevices::png(filename = out_file,
    width = 10, height = 7, units = "in", res = 300, pointsize = 12)

  cex.axis = 1.25
  cex.lab = 1.20
  ymax = max(hi_sdmtmb) + 0.10 * max(hi_sdmtmb)
  x <- 0.04

  plot(0, type = "n",
      xlim = range(years),
      ylim = c(0, ymax),
      xlab = "", ylab = "", yaxs = "i",
      main = "", cex.axis = cex.axis)

  graphics::mtext(side = 1, "Year", cex = cex.lab, line = 3)
  graphics::mtext(side = 2, "Relative Index", cex = cex.lab, line = 2.5)

  graphics::arrows(x0 = years + x, y0 = lo_sdmtmb, x1 = years + x, y1 = hi_sdmtmb, 
    angle = 90, code = 3, length = 0.01, col = "blue",
    lty = 2)
  graphics::points(years + x, sdmtmb_est, pch = 16, bg = 1, cex = 1.6, col = 'blue')
  graphics::lines(years + x,  sdmtmb_est, cex = 1, col = 'blue', lty = 2)
  
  if (smooth == TRUE) {
    pred <- stats::loess(Median ~ Year, tmp)
    graphics::lines(years, stats::predict(pred), lty = 2, col = "snow4")
  }

  dev.off()
}