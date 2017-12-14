
# Extracting parameters from model fitted in prism-generated files.

rm(list = ls())
setwd()

filelist <- list.files (pattern = ".txt")

for (i in 1:length(filelist)) {
  
  filename <-  read.table (filelist[i], header = TRUE, sep = "\t")
  filename$TR.Rate <- as.character (filename$TR.Rate)
  
  if (filename$TR.Rate[4] > 0.05) {
    Model <- "Line"
    Y.intercept <- filename$TR.Rate[33]
    Y.intercept.SE <- filename$TR.Rate[36]
    Slope.1 <- filename$TR.Rate[34]
    Slope.1.SE <- filename$TR.Rate[37]
    Slope.2 <- filename$TR.Rate[1]
    BP <- filename$TR.Rate[1]
    BP.SE <- filename$TR.Rate[1]
    Slope.2.SE <- filename$TR.Rate[1]
    R.square <- filename$TR.Rate[43]
  }
  else if (filename$TR.Rate[4] < 0.05) {
    Model <- "Segmental"
    Y.intercept <- filename$TR.Rate[11]
    Y.intercept.SE <- filename$TR.Rate[16]
    Slope.1 <- filename$TR.Rate[12]
    Slope.1.SE <- filename$TR.Rate[17]
    Slope.2 <- filename$TR.Rate[14]
    BP <- filename$TR.Rate[13]
    BP.SE <- filename$TR.Rate[18]
    Slope.2.SE <- filename$TR.Rate[19]
    R.square <- filename$TR.Rate[27]
  }
  
  result <- data.frame (filelist[i], Model, Y.intercept, Y.intercept.SE,Slope.1,Slope.1.SE,Slope.2,Slope.2.SE, R.square, BP, BP.SE, row.names = FALSE)
  write.table (result, file = "Model.comparison.LSq.csv", row.names= FALSE, col.names=FALSE, sep=",", append = TRUE)

}

