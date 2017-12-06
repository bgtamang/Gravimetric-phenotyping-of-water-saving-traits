

#This part of script is to compute VPD from the weather data loggers. 
#Make sure you use correct file encoding. In this case it is "iso-8859-1". Use terminal and use file -I <filename> to find out the encoding type.

rm(list = ls())
setwd()

#Writing a function for computing VPD
get.vpd <- function (rh, temp){
  vpd <- 0.61375 * exp (17.502 * temp/(240.97 + temp)) * (1- rh/100)
  return (vpd)
}

#Reading multiple WEATHER LOGGER .txt files. (See attached file to see format).
filelist <- list.files (pattern = ".txt")

for (i in 1:length(filelist)) {
  
  filename <- read.table (paste(i, ".txt", sep = ""), sep = ",", header = TRUE, fileEncoding="iso-8859-1", skipNul = TRUE)
  filename1 <- filename[,1:4] #Excluding unnecessary columns and retaining only required ones.
  names(filename1) <- c("S.No", "Date.Time", "Temperature", "RH") # Renaming the column names for ease.
  
  for (j in 1: length(filename1$Temperature)){
    filename1$VPD[j] <- get.vpd (filename1$RH[j],filename1$Temperature[j])
  }
#Writing out the files as .csv.
  filename2 <- paste(i,".csv", sep = "")
  write.table (filename1, file = filename2, sep=",", row.names = FALSE)
  
}

