
index = read.csv(paste0(dir, "/txt_files/index.csv"), header = TRUE)
all = sort(unique(index[,'filename']))

index.lab = sum.lab = depth.lab = length.lab = matrix(NA, length(all), 4)

for(a in 1:length(all)){
  find = which(index[,"filename"] == all[a])

  name = gsub("_", " ", firstup(all[a]))
  area = ifelse(index[find[1],"area"]== "coast", "coastwide and by state", 
         ifelse(index[find[1],"area"]== "north_south", "coastwide and by North and South of N. 40 degrees 10' lat. ",
         ifelse(index[find[1],"area"]== "ca", "for California",
         ifelse(index[find[1],"area"]== "wa_or", "across Oregon and Washington and by state",
                "coastwide and North and South of N. 36 degrees lat. "))))

  # Index of abundance
  index.lab[a, 1:2] = c(all[a], name)
  if (sum(index[find,'area'] != 'hkl') == 1){
    text = paste("Index of abundance for",  name, "from the NWFSC WCGBT survey from 2003 - 2019 calculated", area, ". A loess smoother line was fit to the data series and is denoted by the grey dashed line.")
    index.lab[a, 3] = text
  }
  if (sum(index[find,'area'] == 'hkl') == 1){
    text = paste("Index of abundance for",  name, "from the NWFSC Hook and Line survey from 2004 - 2019 calculated", area, ".")
    index.lab[a, 4] = text
  }

  # Depth figure 
  depth.lab[a, 1:2] = c(all[a], name)
  if (sum(index[find,'area'] != 'hkl') == 1){
    text = paste("CPUE and length data by latitude, depth, and sex for",  name, "from the NWFSC WCGBT survey from 2003 - 2019.")
    depth.lab[a, 3] = text
  }
  if (sum(index[find,'area'] == 'hkl') == 1){
    text = paste("CPUE and length data by latitude, depth, and sex for",  name, "from the NWFSC Hook and Line survey from 2004 - 2019.")    
    depth.lab[a, 4] = text
  }

  # Data summary 
  sum.lab[a, 1:2] = c(all[a], name)
  if (sum(index[find,'area'] != 'hkl') == 1){
    text = paste("Summary of collected data for",  name, "from the NWFSC WCGBT survey from 2003 - 2019.")
    sum.lab[a, 3] = text
  }
  if (sum(index[find,'area'] == 'hkl') == 1){
    text = paste("Summary of collected data for",  name, "from the NWFSC Hook and Line survey from 2004 - 2019.")    
    sum.lab[a, 4] = text
  }

  # Length compositions
  length.lab[a, 1:2] = c(all[a], name)
  if (sum(index[find,'area'] != 'hkl') == 1){
    text = paste("Summary of length composition data by year for",  name, "from the NWFSC WCGBT survey from 2003 - 2019.")
    length.lab[a,3] = text
  }
  if (sum(index[find,'area'] != 'hkl') == 1){
    text = paste("Summary of length composition data by year for",  name, "from the NWFSC Hook and Line survey from 2004 - 2019.")    
    length.lab[a, 4] = text
  }

}
