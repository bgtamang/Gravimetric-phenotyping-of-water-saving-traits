

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

#Run all the loggers in loop
for (i in 1:length(filelist)) {
  
  filename <- read.table (paste(i, ".txt", sep = ""), sep = ",", header = TRUE, fileEncoding="iso-8859-1", skipNul = TRUE)
  filename1 <- filename[,1:4]
  names(filename1) <- c("S.No", "Date.Time", "Temperature", "RH")
  filename1$Date.Time <- as.POSIXct(filename1$Date.Time)
  
  for (j in 1:length(filename1$Temperature)) {
    filename1$VPD[j] <- get.vpd (filename1$RH[j],filename1$Temperature[j])
   
    filename1$Week    <- 1
    filename1$Logger  <- i
    
    if (i == 1 | i == 2 | i == 3) {filename1$Chamber <- 1 }
    if (i == 4 | i == 5 | i == 6) {filename1$Chamber <- 2 }
    if (i == 7 | i == 8 | i == 9) {filename1$Chamber <- 3 }
    
    #Subsetting the data according to days.
    Day1 <- filename1[filename1$Date.Time %within% Day1.interval,]
    Day1$Day <- 1
    
    Day2 <- filename1[filename1$Date.Time %within% Day2.interval,]
    Day2$Day <- 2
    
    Day3 <- filename1[filename1$Date.Time %within% Day3.interval,]
    Day3$Day <- 3
    
    filename2 <- rbind(Day1, Day2, Day3) #combine all relevant data again from above.
    
   # if (filename1$Date.Time[j] %within% Day1.interval == TRUE) {filename1$Day <- 1 }
   # else if (filename1$Date.Time[j] %within% Day2.interval == TRUE) {filename1$Day <- 2}
    #else if (filename1$Date.Time[j] %within% Day3.interval == TRUE) {filename1$Day <- 3}
    #else {filename1$Day <- 0}
 }
  filename3 <- paste(i,".csv", sep = "")
  write.table (filename2, file = filename3, sep=",", row.names = FALSE)
}
