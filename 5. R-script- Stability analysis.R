rm(list=ls()) 
require(lubridate)
#require (parsedate)


setwd(")

weight.metadata  <- read.csv ("weather.metadata.csv", sep = ",", header = TRUE)
weight.metadata$Genotype <- as.character (weight.metadata$Genotype)
####

weight.metadata$step1.start   <- as.POSIXct (weight.metadata$step1.start, format = "%m/%d/%y %H:%M")
weight.metadata$step1.end     <- as.POSIXct (weight.metadata$step1.end, format = "%m/%d/%y %H:%M")
weight.metadata$step2.start   <- as.POSIXct (weight.metadata$step2.start, format = "%m/%d/%y %H:%M")
weight.metadata$step2.end     <- as.POSIXct (weight.metadata$step2.end, format = "%m/%d/%y %H:%M")
weight.metadata$step3.start   <- as.POSIXct (weight.metadata$step3.start, format = "%m/%d/%y %H:%M")
weight.metadata$step3.end     <- as.POSIXct (weight.metadata$step3.end, format = "%m/%d/%y %H:%M")
weight.metadata$step4.start   <- as.POSIXct (weight.metadata$step4.start, format = "%m/%d/%y %H:%M")
weight.metadata$step4.end     <- as.POSIXct (weight.metadata$step4.end, format = "%m/%d/%y %H:%M")
weight.metadata$step5.start   <- as.POSIXct (weight.metadata$step5.start, format = "%m/%d/%y %H:%M")
weight.metadata$step5.end     <- as.POSIXct (weight.metadata$step5.end, format = "%m/%d/%y %H:%M")
weight.metadata$step6.start   <- as.POSIXct (weight.metadata$step6.start, format = "%m/%d/%y %H:%M")
weight.metadata$step6.end     <- as.POSIXct (weight.metadata$step6.end, format = "%m/%d/%y %H:%M")
weight.metadata$step7.start   <- as.POSIXct (weight.metadata$step7.start, format = "%m/%d/%y %H:%M")
weight.metadata$step7.end     <- as.POSIXct (weight.metadata$step7.end, format = "%m/%d/%y %H:%M")

#class(weather.metadata$Date.Time)
####

for (i in 1:length(unique(weight.metadata$Day))){
  weight.metadata_ss  <- subset(weight.metadata, weight.metadata$Day == i)
  weight.metadata_ss  <- weight.metadata_ss[with(weight.metadata_ss, order (Balance)),]
  
  if      (weight.metadata_ss$Day [i] == 1) setwd("/Users/bgtamang/UMN/LAB WORKS/RESEARCH DATA/4. BARLEY combined (Exp23 and Exp24)/R curated/Exp23_Weight")
  else if (weight.metadata_ss$Day [i] == 2) setwd("/Users/bgtamang/UMN/LAB WORKS/RESEARCH DATA/4. BARLEY combined (Exp23 and Exp24)/R curated/Exp23_Weight")
  else if (weight.metadata_ss$Day [i] == 3) setwd("/Users/bgtamang/UMN/LAB WORKS/RESEARCH DATA/4. BARLEY combined (Exp23 and Exp24)/R curated/Exp23_Weight")
  else if (weight.metadata_ss$Day [i] == 4) setwd("/Users/bgtamang/UMN/LAB WORKS/RESEARCH DATA/4. BARLEY combined (Exp23 and Exp24)/R curated/Exp24_Weight")
  else if (weight.metadata_ss$Day [i] == 5) setwd("/Users/bgtamang/UMN/LAB WORKS/RESEARCH DATA/4. BARLEY combined (Exp23 and Exp24)/R curated/Exp24_Weight")
  
  for (k in 1:length(weight.metadata_ss$Balance)) {
    if (file.exists(paste(k,".csv",sep="")) == TRUE){
      balance.name           <-read.csv (paste(k,".csv",sep=""),sep=",", header=FALSE)
      names(balance.name)    <- c("S.No", "Date", "Time", "Stability", "Weight", "Unit")
      drop.columns           <- c("Stability", "Unit") #Creating list of columns to remove
      balance.name           <- balance.name[,!(names(balance.name) %in% drop.columns)] #Removing the unnecessary columns
      balance.name$Date.Time <- paste (balance.name$Date, balance.name$Time)
      balance.name$Date.Time <- as.POSIXct (balance.name$Date.Time, format = "%Y/%m/%d %H:%M:%S")
      
      balance.name$Weight    <- as.character(balance.name$Weight)
      balance.name$Weight    <-suppressWarnings(as.numeric(balance.name$Weight))
      
      balance.name$TR.RATE   <- c(NA)
      LEAF.AREA              <- weight.metadata_ss$Leaf.area[k]
      
      for (l in 2:length(balance.name$Weight)){
        balance.name$TR.RATE[l] <- (((balance.name$Weight[l-1]-balance.name$Weight[l])*1000)/(60))/((LEAF.AREA)/10000)
      }
      
      #Interval defining for TR rate
      
      step1.start <- weight.metadata_ss$step1.start[k]
      step1.end   <- weight.metadata_ss$step1.end[k]
      step2.start <- weight.metadata_ss$step2.start[k]
      step2.end   <- weight.metadata_ss$step2.end[k]
      step3.start <- weight.metadata_ss$step3.start[k]
      step3.end   <- weight.metadata_ss$step3.end[k]
      step4.start <- weight.metadata_ss$step4.start[k]
      step4.end   <- weight.metadata_ss$step4.end[k]
      step5.start <- weight.metadata_ss$step5.start[k]
      step5.end   <- weight.metadata_ss$step5.end[k]
      step6.start <- weight.metadata_ss$step6.start[k]
      step6.end   <- weight.metadata_ss$step6.end[k]
      step7.start <- weight.metadata_ss$step7.start[k]
      step7.end   <- weight.metadata_ss$step7.end[k]
      
      int1        <- interval(step1.start, step1.end)
      int2        <- interval(step2.start, step2.end)
      int3        <- interval(step3.start, step3.end)
      int4        <- interval(step4.start, step4.end)
      int5        <- interval(step5.start, step5.end)
      int6        <- interval(step6.start, step6.end)
      int7        <- interval(step7.start, step7.end)
    
      #############################################################################################################
      #Daytime TR Steps
      #STEP1
      
      STEP1 <- balance.name$TR.RATE[balance.name$Date.Time %within% int1]
      
      STEP1 <- t (STEP1 [c(1:44)])
      
      #############################################################################################################
      
      #STEP2
      
      STEP2 <- balance.name$TR.RATE[balance.name$Date.Time %within% int2]
      STEP2 <- t(STEP2 [c(1:44)])

      #############################################################################################################
      
      #STEP3
      
      STEP3 <- balance.name$TR.RATE[balance.name$Date.Time %within% int3]
      STEP3 <- t (STEP3 [c(1:44)])
    
      #############################################################################################################
      
      #STEP4
      
      STEP4 <- balance.name$TR.RATE[balance.name$Date.Time %within% int4]
      STEP4 <- t (STEP4 [c(1:44)])
      
      #############################################################################################################
      
      #STEP5
      
      STEP5 <- balance.name$TR.RATE[balance.name$Date.Time %within% int5]
      STEP5 <- t (STEP5 [c(1:44)])
    
      #############################################################################################################
      
      #STEP6
      
      STEP6 <- balance.name$TR.RATE[balance.name$Date.Time %within% int6]
      STEP6 <- t (STEP6 [c(1:44)])
      
      #############################################################################################################
      
      #STEP7
      
      STEP7 <- balance.name$TR.RATE[balance.name$Date.Time %within% int7]
      STEP7 <- t (STEP7 [c(1:44)])
     
      ############################################################################################################   

      
      write.table (STEP1, file = "STEP1.csv", sep = ",", row.names= FALSE, col.names = FALSE, append = TRUE)
      write.table (STEP2, file = "STEP2.csv", sep = ",", row.names= FALSE, col.names = FALSE, append = TRUE)
      write.table (STEP3, file = "STEP3.csv", sep = ",", row.names= FALSE, col.names = FALSE, append = TRUE)
      write.table (STEP4, file = "STEP4.csv", sep = ",", row.names= FALSE, col.names = FALSE, append = TRUE)
      write.table (STEP5, file = "STEP5.csv", sep = ",", row.names= FALSE, col.names = FALSE, append = TRUE)
      write.table (STEP6, file = "STEP6.csv", sep = ",", row.names= FALSE, col.names = FALSE, append = TRUE)
      write.table (STEP7, file = "STEP7.csv", sep = ",", row.names= FALSE, col.names = FALSE, append = TRUE)
      
    }
    else if (file.exists(paste(k,".csv",sep=" "))==FALSE) 
    {}
  }
  
}

