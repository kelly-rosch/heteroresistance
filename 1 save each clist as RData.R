
rm(list = ls()) # clear out previous data 
dev.off()       # close all previous plots
cat("\014")     # clear console of previous output

library(eeptools)

#------------------------------------------------------------
# opening all files in the Clists folder to form the filelist
setwd("/Users/me/Documents/Doerr Lab/Andy manuscript/MATLAB csv output")
filelist <- list.files()
filelist
           
              
for (fl in filelist) {
	setwd("/Users/me/Documents/Doerr Lab/Andy manuscript/MATLAB csv output")
	cat(fl,"\n")
	alldata <- read.csv(fl)
	
# making a dataset called "data" that only contains the columns we want 
# from the "alldata" dataset
	data <- alldata[, c(1,4,5,6,8,10,11,12,13,14,15,61,62,63,94,95,97)]
	rm(alldata)

# rename columns to something nicer
	colnames(data)[1] <- 'cellid'
	colnames(data)[2] <- 'birthframe'
	colnames(data)[3] <- 'deathframe'
	colnames(data)[4] <- 'age'
	colnames(data)[5] <- 'error'
	colnames(data)[6] <- 'lengthbirth'
	colnames(data)[7] <- 'lengthdeath'
	names(data)[names(data) == 'Short.axis.birth'] <- 'widthbirth'
	names(data)[names(data) == 'Short.axis.death'] <- 'widthdeath'
	names(data)[names(data) == 'Area.birth'] <- 'areabirth'
	names(data)[names(data) == 'Area.death'] <- 'areadeath'
	names(data)[names(data) == 'Long.axis.Short.axis.birth'] <- 'ratiobirth'
	names(data)[names(data) == 'Long.axis.Short.axis.death'] <- 'ratiodeath'
	names(data)[names(data) == 'Maximum.width'] <- 'maxwidth'
	names(data)[names(data) == 'Mother.ID'] <- 'mother'
	names(data)[names(data) == 'Daughter1.ID'] <- 'daughter1'
	names(data)[names(data) == 'Daughter2.ID'] <- 'daughter2'

	
	#printing cell id and width at birth
	cat(sprintf("%5.0f",data$cellid[1]),"  ")	
	cat(sprintf("%5.1f",data$widthbirth[1]),"\n")
	
	
	# checking to make sure each cellid only appears once in each file
	# if it appears multiple times, redo cellid number based on row number
	A = isid(data,vars=c("cellid"))
	if (A == FALSE) {
	  cat("---------------Cellid for the data above is not unique, creating new cellid based on row number---------------- \n")
	  data$cellid = 1:nrow(data)
	} 
	
	
# convert pixels to microns
	data$lengthbirth <- data$lengthbirth*0.065
	data$lengthdeath <- data$lengthdeath*0.065;
	data$widthbirth <- data$widthbirth*0.065;
	data$widthdeath <- data$widthdeath*0.065;
# area = length * width so we multiply twice
	data$areabirth <- data$areabirth*0.065*0.065;
	data$areadeath  <- data$areadeath*0.065*0.065;	

# Identifying errors but not getting rid of them yet
	
	# adding up the number of times there's an error according to MATLAB
	nerror <- sum(!is.na(data$error))
	N <- sum(!is.na(data$cellid))
	cat("Number of MATLAB errors =",nerror, " out of",N,",",100*nerror/N,"%\n")

	# counting times that there was zero LENGTH growth between birth and death
	data$lengthdif <- data$lengthdeath-data$lengthbirth
	nzerolength <- sum(data$lengthdif==0)
	cat("Cells with 0 length growth =",nzerolength, " out of",N,",",100*nzerolength/N,"%\n")
	
	# counting times that there was zero WIDTH growth between birth and death
	data$widthdif <- data$widthdeath-data$widthbirth
	nzerowidth <- sum(data$widthdif==0)
	cat("Cells with 0 width growth =",nzerowidth, " out of",N,",",100*nzerowidth/N,"%\n")
	
	# counting times that there was zero AREA growth between birth and death
	data$areadif <- data$areadeath-data$areabirth
	nzeroarea <- sum(data$areadif==0)
	cat("Cells with 0 area growth =",nzeroarea, " out of",N,",",100*nzeroarea/N,"%\n")
	
	data$error_MATLAB <- (!is.na(data$error))
	data$error_nogrowthL <- (data$lengthdif==0)
	data$error_nogrowthW <- (data$widthdif==0)
	data$error_nogrowthA <- (data$areadif==0)
	
	# It's unlikely that the 30's were born and died in the same frame
	# Many of the cells born in 30 had outrageous values
	# Perhaps we should get rid of the cells born in frame 30 as we've done above
	
	
# Adding columns for strain, position, and drug concentration based on clist filename

	X <- substr(fl,1,2)
	if (X == "d3") {data$strain <- "d3"}
	if (X == "WT") {data$strain <- "WT"}
	
	data$strain <- as.factor(data$strain)
	
	
	Y <- substr(fl,4,5)
	if (Y == "00") {data$drug_conc <- "0"}
	if (Y == "10") {data$drug_conc <- "10"}
	if (Y == "20") {data$drug_conc <- "20"}
	if (Y == "HI") {data$drug_conc <- "200"}
	
	data$drug_conc <- as.factor(data$drug_conc)
	
	
	Z <- substr(fl,7,7)
	if (Z == "1") {data$position <- "1"}
	if (Z == "2") {data$position <- "2"}
	if (Z == "3") {data$position <- "3"}
	
	data$position <- as.numeric(data$position)
	

	# bringing strain, date, position, and medium to the front of the dataset
	data <- data[, c(25,26,27,1,2,3,4,12,13,14,6,7,8,9,10,11,15,16,17,18,19,20,5,21,22,23,24)]

	
# save		
	setwd("/Users/me/Documents/Doerr Lab/Andy manuscript/R data/Clists")
	
	Rf=paste0(substr(fl,1,nchar(fl)-4),".RData")
	save(data, file = Rf);
	cat(Rf,"\n\n")
	rm(data)
}


 