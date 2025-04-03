
rm(list = ls()) # clear out previous data 
dev.off()       # close all previous plots
cat("\014")     # clear console of previous output

library("eeptools")
library("tidyr")
library("dplyr")

#------------------------------------------------------------

setwd("/Users/me/Documents/Doerr Lab/Andy manuscript/R data/Clists")
filelist <- list.files()
filelist

# making a dataframe called datafull because we can't rbind something onto nothing
fl <- filelist[1]
load(fl)
datafull <- data
rm(data)

for (i in 2:length(filelist)){
  fl <- filelist[i]
  cat(fl, "\n")
  load(fl)
  datafull <- rbind(datafull, data)
  rm(data)
}

anyDuplicated(datafull)

data <- datafull
rm(datafull)


#-----------------------------------------------------------
# adding columns for division and age at division
#-----------------------------------------------------------

data$division <- !is.na(data$daughter1)

data$division_age <- data$age
data$division_age[is.na(data$daughter1)] <- NA


#-----------------------------------------------------------
# saving all clists as one .Rdata file
#-----------------------------------------------------------

setwd("/Users/me/Documents/Doerr Lab/Andy manuscript/R data")
save(data, file = "AllClists.RData")

