

# Extracting parameters from model fitted in prism-generated files. Transpiration rate to VPD raw data is first uploaded to PRISM and two models
# are fitted (linear vs segmental). Then, the results are exported and following scripts are separately run on two types of files output to extract
# model parameters such as Slope1, Slope2, Y-intercept, Breakpoint, p-value, probability, R-squared etc. Example of raw files are uploaded as folder
# named "AICc method".


# METHOD 1
#####################################################################
#                      Least Square Method                          #
#####################################################################


rm(list = ls())
setwd()

filelist <- list.files (pattern = ".csv")

for (i in 1:length(filelist)) {
  
  filename <-  read.csv (filelist[i], header = TRUE, sep = ",")
  filename$TR <- as.character (filename$TR)
  
  if (filename$TR[4] > 0.05) {
    Model <- "Line"
    Y.intercept <- filename$TR[33]
    Y.intercept.SE <- filename$TR[36]
    Slope.1 <- filename$TR[34]
    Slope.1.SE <- filename$TR[37]
    Slope.2 <- filename$TR[1]
    Slope.2.SE <- filename$TR[1]
    BP <- filename$TR[1]
    BP.SE <- filename$TR[1]
    p.value <-  filename$TR[4]
    R.square <- filename$TR[43]
  }
  
  else if (filename$TR[4] < 0.05) {
    Model <- "Segmental"
    Y.intercept <- filename$TR[11]
    Y.intercept.SE <- filename$TR[16]
    Slope.1 <- filename$TR[12]
    Slope.1.SE <- filename$TR[17]
    Slope.2 <- filename$TR[14]
    Slope.2.SE <- filename$TR[19]
    BP <- filename$TR[13]
    BP.SE <- filename$TR[18]
    p.value <-  filename$TR[4]
    R.square <- filename$TR[27]
  }
  
  result <- data.frame (filelist[i], Model, Y.intercept, Y.intercept.SE,Slope.1,Slope.1.SE,Slope.2,Slope.2.SE, R.square, BP, BP.SE, p.value, row.names = FALSE)
  write.table (result, file = "Model.parameter.extract.LSS.csv", row.names= FALSE, col.names=FALSE, sep=",", append = TRUE)
  
}


# METHOD 2
#####################################################################
#              Akaike Information Criteria Method                   #
#####################################################################


rm(list = ls())
setwd()

filelist <- list.files (pattern = ".csv")

for (i in 1:length(filelist)) {
  
  filename <-  read.csv (filelist[i], header = TRUE, sep = ",")
  filename$TR <- as.character (filename$TR)
  
  if (filename$TR[3] > filename$TR[5]) {
    Model <- "Line"
    Y.intercept <- filename$TR[34]
    Y.intercept.SE <- filename$TR[37]
    Slope.1 <- filename$TR[35]
    Slope.1.SE <- filename$TR[38]
    Slope.2 <- filename$TR[1]
    Slope.2.SE <- filename$TR[1]
    BP <- filename$TR[1]
    BP.SE <- filename$TR[1]
    probability <-  filename$TR[3]
    R.square <- filename$TR[44]
  }
  
  else if (filename$TR[3] < filename$TR[5]) {
    Model <- "Segmental"
    Y.intercept <- filename$TR[12]
    Y.intercept.SE <- filename$TR[17]
    Slope.1 <- filename$TR[15]
    Slope.1.SE <- filename$TR[20]
    Slope.2 <- filename$TR[14]
    Slope.2.SE <- filename$TR[19]
    BP <- filename$TR[14]
    BP.SE <- filename$TR[19]
    probability <-  filename$TR[5]
    R.square <- filename$TR[28]
  }
  
  result <- data.frame (filelist[i], Model, Y.intercept, Y.intercept.SE,Slope.1,Slope.1.SE,Slope.2,Slope.2.SE, R.square, BP, BP.SE, probability, row.names = FALSE)
  write.table (result, file = "Model.parameter.extract.AICc.csv", row.names= FALSE, col.names=FALSE, sep=",", append = TRUE)
  
}

