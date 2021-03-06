#This part of the R script computes nocturnal transpiration and daytime canopy conductances using all the logger datasets.

require(lubridate)
require (parsedate)
rm(list=ls()) 
setwd()

weight.metadata  <- read.csv ("weight.metadata.csv", sep = ",", header = TRUE)
weather.metadata <- read.csv ("weather.metadata.csv", sep = ",", header = TRUE)

weight.metadata$Genotype <- as.character (weight.metadata$Genotype)


####
weight.metadata$NTR.start     <- as.POSIXct (weight.metadata$NTR.start, format = "%m/%d/%y %H:%M")
weight.metadata$NTR.end       <- as.POSIXct (weight.metadata$NTR.end, format = "%m/%d/%y %H:%M")
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

weather.metadata$Date.Time    <- as.POSIXct (weather.metadata$Date.Time, format = "%m/%d/%y %H:%M")
#class(weather.metadata$Date.Time)
####

for (i in 1:length(unique(weight.metadata$Day))){
  weight.metadata_ss <- subset(weight.metadata, weight.metadata$Day == i)
  weight.metadata_ss <- weight.metadata_ss[with(weight.metadata_ss, order (Balance)),]
  
  weather.metadata_ss <- subset(weather.metadata, weather.metadata$Day == i)
  
  for (j in 1:length(unique(weight.metadata$Day))) {
         if (weight.metadata_ss$Day [j] ==1) setwd()
    else if (weight.metadata_ss$Day [j] ==2) setwd()
    else if (weight.metadata_ss$Day [j] ==3) setwd()
    else if (weight.metadata_ss$Day [j] ==4) setwd()
    else if (weight.metadata_ss$Day [j] ==5) setwd()
    
  }
  
  for (k in 1:length(weight.metadata_ss$Balance)){
    if (file.exists(paste(k,".csv",sep="")) == TRUE){
      balance.name <-read.csv (paste(k,".csv",sep=""),sep=",", header=FALSE)
      names(balance.name) <- c("S.No", "Date", "Time", "Stability", "Weight", "Unit")
      drop.columns <- c("Stability", "Unit") #Creating list of columns to remove
      balance.name <- balance.name[,!(names(balance.name) %in% drop.columns)] #Removing the unnecessary columns
      balance.name$Date.Time <- paste (balance.name$Date, balance.name$Time)
      balance.name$Date.Time <- as.POSIXct (balance.name$Date.Time, format = "%Y/%m/%d %H:%M:%S")
      
      balance.name$Weight <- as.character(balance.name$Weight)
      balance.name$Weight <-suppressWarnings(as.numeric(balance.name$Weight))

      balance.name$TR.RATE <- c(NA)
      LEAF.AREA <- weight.metadata_ss$Leaf.area[k]
       for (l in 2:length(balance.name$Weight)){
         balance.name$TR.RATE[l] <- (((balance.name$Weight[l-1]-balance.name$Weight[l])*1000)/(60))/((LEAF.AREA)/10000)
       }
      #Interval defining for TR rate
      ntr.start <- weight.metadata_ss$NTR.start[k]
      ntr.end <- weight.metadata_ss$NTR.end[k]
      step1.start <- weight.metadata_ss$step1.start[k]
      step1.end <- weight.metadata_ss$step1.end[k]
      step2.start <- weight.metadata_ss$step2.start[k]
      step2.end <- weight.metadata_ss$step2.end[k]
      step3.start <- weight.metadata_ss$step3.start[k]
      step3.end <- weight.metadata_ss$step3.end[k]
      step4.start <- weight.metadata_ss$step4.start[k]
      step4.end <- weight.metadata_ss$step4.end[k]
      step5.start <- weight.metadata_ss$step5.start[k]
      step5.end <- weight.metadata_ss$step5.end[k]
      step6.start <- weight.metadata_ss$step6.start[k]
      step6.end <- weight.metadata_ss$step6.end[k]
      step7.start <- weight.metadata_ss$step7.start[k]
      step7.end <- weight.metadata_ss$step7.end[k]
      
      int.ntr <- interval (ntr.start, ntr.end)
      int1 <- interval(step1.start, step1.end)
      int2 <- interval(step2.start, step2.end)
      int3 <- interval(step3.start, step3.end)
      int4 <- interval(step4.start, step4.end)
      int5 <- interval(step5.start, step5.end)
      int6 <- interval(step6.start, step6.end)
      int7 <- interval(step7.start, step7.end)
      
      #Interval defining for VPD because VPD loggers are 1 hr ahead of TR loggers and they need different intervals.
      vpd.ntr.start <- weight.metadata_ss$NTR.start[k] + 60*60
      vpd.ntr.end <- weight.metadata_ss$NTR.end[k] + 60*60
      vpd.step1.start <- weight.metadata_ss$step1.start[k] + 60*60
      vpd.step1.end <- weight.metadata_ss$step1.end[k] + 60*60
      vpd.step2.start <- weight.metadata_ss$step2.start[k] + 60*60
      vpd.step2.end <- weight.metadata_ss$step2.end[k] + 60*60
      vpd.step3.start <- weight.metadata_ss$step3.start[k] + 60*60
      vpd.step3.end <- weight.metadata_ss$step3.end[k] + 60*60
      vpd.step4.start <- weight.metadata_ss$step4.start[k] + 60*60
      vpd.step4.end <- weight.metadata_ss$step4.end[k] + 60*60
      vpd.step5.start <- weight.metadata_ss$step5.start[k] + 60*60
      vpd.step5.end <- weight.metadata_ss$step5.end[k] + 60*60
      vpd.step6.start <- weight.metadata_ss$step6.start[k] + 60*60
      vpd.step6.end <- weight.metadata_ss$step6.end[k] + 60*60
      vpd.step7.start <- weight.metadata_ss$step7.start[k] + 60*60
      vpd.step7.end <- weight.metadata_ss$step7.end[k] + 60*60
      
      vpd.int.ntr <- interval (vpd.ntr.start, vpd.ntr.end)
      vpd.int1 <- interval(vpd.step1.start, vpd.step1.end)
      vpd.int2 <- interval(vpd.step2.start, vpd.step2.end)
      vpd.int3 <- interval(vpd.step3.start, vpd.step3.end)
      vpd.int4 <- interval(vpd.step4.start, vpd.step4.end)
      vpd.int5 <- interval(vpd.step5.start, vpd.step5.end)
      vpd.int6 <- interval(vpd.step6.start, vpd.step6.end)
      vpd.int7 <- interval(vpd.step7.start, vpd.step7.end)
      
      #############################################################################################################
      # Nighttime Transpiration Rate
      NTR.int <- balance.name$TR.RATE[balance.name$Date.Time %within% vpd.int.ntr]
      mean.NTR <- mean(NTR.int, na.rm = TRUE)
      SE.NTR = sd(NTR.int)/sqrt(length(NTR.int))
      if (weight.metadata_ss$Chamber[k] == 1) {
        NTR.VPD_ss <- subset (weather.metadata_ss, weather.metadata_ss$Chamber == 1)
        NTR.VPD_ss_logger1 <- subset (NTR.VPD_ss, NTR.VPD_ss$Logger == 1)
        NTR.VPD_ss_logger2 <- subset (NTR.VPD_ss, NTR.VPD_ss$Logger == 2)
        NTR.VPD_ss_logger3 <- subset (NTR.VPD_ss, NTR.VPD_ss$Logger == 3)
        
        NTR.VPD.Logger1_int <- NTR.VPD_ss_logger1$VPD[NTR.VPD_ss_logger1$Date.Time %within% vpd.int.ntr]
        NTR.VPD.Logger2_int <- NTR.VPD_ss_logger2$VPD[NTR.VPD_ss_logger2$Date.Time %within% vpd.int.ntr]
        NTR.VPD.Logger3_int <- NTR.VPD_ss_logger3$VPD[NTR.VPD_ss_logger3$Date.Time %within% vpd.int.ntr]
        
        mean.NTR.Logger1 <- mean(NTR.VPD.Logger1_int)
        mean.NTR.Logger2 <- mean(NTR.VPD.Logger2_int)
        mean.NTR.Logger3 <- mean(NTR.VPD.Logger3_int)
        
        av.NTR.VPD <- (mean.NTR.Logger1 + mean.NTR.Logger2 + mean.NTR.Logger3)/3
        
      }
      
      else if (weight.metadata_ss$Chamber[k] == 2) {
        NTR.VPD_ss <- subset (weather.metadata_ss, weather.metadata_ss$Chamber == 2)
        NTR.VPD_ss_logger4 <- subset (NTR.VPD_ss, NTR.VPD_ss$Logger == 4)
        NTR.VPD_ss_logger5 <- subset (NTR.VPD_ss, NTR.VPD_ss$Logger == 5)
        NTR.VPD_ss_logger6 <- subset (NTR.VPD_ss, NTR.VPD_ss$Logger == 6)
        
        NTR.VPD.Logger4_int <- NTR.VPD_ss_logger4$VPD[NTR.VPD_ss_logger4$Date.Time %within% vpd.int.ntr]
        NTR.VPD.Logger5_int <- NTR.VPD_ss_logger5$VPD[NTR.VPD_ss_logger5$Date.Time %within% vpd.int.ntr]
        NTR.VPD.Logger6_int <- NTR.VPD_ss_logger6$VPD[NTR.VPD_ss_logger6$Date.Time %within% vpd.int.ntr]
        
        mean.NTR.Logger4 <- mean(NTR.VPD.Logger4_int)
        mean.NTR.Logger5 <- mean(NTR.VPD.Logger5_int)
        mean.NTR.Logger6 <- mean(NTR.VPD.Logger6_int)
        
        av.NTR.VPD <- (mean.NTR.Logger4 + mean.NTR.Logger5 + mean.NTR.Logger6)/3
      }
      
      else if (weight.metadata_ss$Chamber[k] == 3) {
        NTR.VPD_ss <- subset (weather.metadata_ss, weather.metadata_ss$Chamber == 3)
        NTR.VPD_ss_logger7 <- subset (NTR.VPD_ss, NTR.VPD_ss$Logger == 7)
        NTR.VPD_ss_logger8 <- subset (NTR.VPD_ss, NTR.VPD_ss$Logger == 8)
        NTR.VPD_ss_logger9 <- subset (NTR.VPD_ss, NTR.VPD_ss$Logger == 9)
        
        NTR.VPD.Logger7_int <- NTR.VPD_ss_logger7$VPD[NTR.VPD_ss_logger7$Date.Time %within% vpd.int.ntr]
        NTR.VPD.Logger8_int <- NTR.VPD_ss_logger8$VPD[NTR.VPD_ss_logger8$Date.Time %within% vpd.int.ntr]
        NTR.VPD.Logger9_int <- NTR.VPD_ss_logger9$VPD[NTR.VPD_ss_logger9$Date.Time %within% vpd.int.ntr]
        
        mean.NTR.Logger7 <- mean(NTR.VPD.Logger7_int)
        mean.NTR.Logger8 <- mean(NTR.VPD.Logger8_int)
        mean.NTR.Logger9 <- mean(NTR.VPD.Logger9_int)
        
        av.NTR.VPD <- (mean.NTR.Logger7 + mean.NTR.Logger8 + mean.NTR.Logger9)/3
      }
      
      
      #############################################################################################################
     #Daytime TR Steps
       #STEP1
      
      STEP1 <- balance.name$TR.RATE[balance.name$Date.Time %within% int1]
      mean.TR.step1 <- mean(STEP1, na.rm = TRUE)
      if (weight.metadata_ss$Chamber[k] == 1){ 
        Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 1)
        Day.VPD.Logger1 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 1)
        Day.VPD.Logger2 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 2)
        Day.VPD.Logger3 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 3)
        
        Day.VPD.Logger1.Step1 <- Day.VPD.Logger1$VPD[Day.VPD.Logger1$Date.Time %within% vpd.int1]
        Day.VPD.Logger2.Step1 <- Day.VPD.Logger2$VPD[Day.VPD.Logger2$Date.Time %within% vpd.int1]
        Day.VPD.Logger3.Step1 <- Day.VPD.Logger3$VPD[Day.VPD.Logger3$Date.Time %within% vpd.int1]
        
        mean.Logger1 <- mean (Day.VPD.Logger1.Step1)
        mean.Logger2 <- mean (Day.VPD.Logger2.Step1)
        mean.Logger3 <- mean (Day.VPD.Logger3.Step1)
        
        VPD.step1.mean <- (mean.Logger1 + mean.Logger2 + mean.Logger3) / 3
      }
      
      else if (weight.metadata_ss$Chamber[k] == 2){ 
        Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 2)
        Day.VPD.Logger4 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 4)
        Day.VPD.Logger5 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 5)
        Day.VPD.Logger6 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 6)
        
        Day.VPD.Logger4.Step1 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int1]
        Day.VPD.Logger5.Step1 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int1]
        Day.VPD.Logger6.Step1 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int1]
        
        mean.Logger4 <- mean (Day.VPD.Logger4.Step1)
        mean.Logger5 <- mean (Day.VPD.Logger5.Step1)
        mean.Logger6 <- mean (Day.VPD.Logger6.Step1)
        
        VPD.step1.mean <- (mean.Logger4 + mean.Logger5 + mean.Logger6) / 3
      }
      
      else if (weight.metadata_ss$Chamber[k] == 3){ 
        Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 3)
        Day.VPD.Logger7 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 7)
        Day.VPD.Logger8 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 8)
        Day.VPD.Logger9 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 9)
        
        Day.VPD.Logger7.Step1 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int1]
        Day.VPD.Logger8.Step1 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int1]
        Day.VPD.Logger9.Step1 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int1]
        
        mean.Logger7 <- mean (Day.VPD.Logger7.Step1)
        mean.Logger8 <- mean (Day.VPD.Logger8.Step1)
        mean.Logger9 <- mean (Day.VPD.Logger9.Step1)
        
        VPD.step1.mean <- (mean.Logger7 + mean.Logger8 + mean.Logger9) / 3
      }
        
    
    
    
    #############################################################################################################
    
    #STEP2
    
    STEP2 <- balance.name$TR.RATE[balance.name$Date.Time %within% int2]
    mean.TR.step2 <- mean(STEP2, na.rm = TRUE)
    if (weight.metadata_ss$Chamber[k] == 1){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 1)
      Day.VPD.Logger1 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 1)
      Day.VPD.Logger2 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 2)
      Day.VPD.Logger3 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 3)
      
      Day.VPD.Logger1.Step2 <- Day.VPD.Logger1$VPD[Day.VPD.Logger1$Date.Time %within% vpd.int2]
      Day.VPD.Logger2.Step2 <- Day.VPD.Logger2$VPD[Day.VPD.Logger2$Date.Time %within% vpd.int2]
      Day.VPD.Logger3.Step2 <- Day.VPD.Logger3$VPD[Day.VPD.Logger3$Date.Time %within% vpd.int2]
      
      mean.Logger1 <- mean (Day.VPD.Logger1.Step2)
      mean.Logger2 <- mean (Day.VPD.Logger2.Step2)
      mean.Logger3 <- mean (Day.VPD.Logger3.Step2)
      
      VPD.step2.mean <- (mean.Logger1 + mean.Logger2 + mean.Logger3) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 2){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 2)
      Day.VPD.Logger4 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 4)
      Day.VPD.Logger5 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 5)
      Day.VPD.Logger6 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 6)
      
      Day.VPD.Logger4.Step2 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int2]
      Day.VPD.Logger5.Step2 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int2]
      Day.VPD.Logger6.Step2 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int2]
      
      mean.Logger4 <- mean (Day.VPD.Logger4.Step2)
      mean.Logger5 <- mean (Day.VPD.Logger5.Step2)
      mean.Logger6 <- mean (Day.VPD.Logger6.Step2)
      
      VPD.step2.mean <- (mean.Logger4 + mean.Logger5 + mean.Logger6) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 3){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 3)
      Day.VPD.Logger7 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 7)
      Day.VPD.Logger8 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 8)
      Day.VPD.Logger9 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 9)
      
      Day.VPD.Logger7.Step2 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int2]
      Day.VPD.Logger8.Step2 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int2]
      Day.VPD.Logger9.Step2 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int2]
      
      mean.Logger7 <- mean (Day.VPD.Logger7.Step2)
      mean.Logger8 <- mean (Day.VPD.Logger8.Step2)
      mean.Logger9 <- mean (Day.VPD.Logger9.Step2)
      
      VPD.step2.mean <- (mean.Logger7 + mean.Logger8 + mean.Logger9) / 3
    }
    
    #############################################################################################################
    
    #STEP3
    
    STEP3 <- balance.name$TR.RATE[balance.name$Date.Time %within% int3]
    mean.TR.step3 <- mean(STEP3, na.rm = TRUE)
    if (weight.metadata_ss$Chamber[k] == 1){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 1)
      Day.VPD.Logger1 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 1)
      Day.VPD.Logger2 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 2)
      Day.VPD.Logger3 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 3)
      
      Day.VPD.Logger1.Step3 <- Day.VPD.Logger1$VPD[Day.VPD.Logger1$Date.Time %within% vpd.int3]
      Day.VPD.Logger2.Step3 <- Day.VPD.Logger2$VPD[Day.VPD.Logger2$Date.Time %within% vpd.int3]
      Day.VPD.Logger3.Step3 <- Day.VPD.Logger3$VPD[Day.VPD.Logger3$Date.Time %within% vpd.int3]
      
      mean.Logger1 <- mean (Day.VPD.Logger1.Step3)
      mean.Logger2 <- mean (Day.VPD.Logger2.Step3)
      mean.Logger3 <- mean (Day.VPD.Logger3.Step3)
      
      VPD.step3.mean <- (mean.Logger1 + mean.Logger2 + mean.Logger3) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 2){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 2)
      Day.VPD.Logger4 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 4)
      Day.VPD.Logger5 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 5)
      Day.VPD.Logger6 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 6)
      
      Day.VPD.Logger4.Step3 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int3]
      Day.VPD.Logger5.Step3 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int3]
      Day.VPD.Logger6.Step3 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int3]
      
      mean.Logger4 <- mean (Day.VPD.Logger4.Step3)
      mean.Logger5 <- mean (Day.VPD.Logger5.Step3)
      mean.Logger6 <- mean (Day.VPD.Logger6.Step3)
      
      VPD.step3.mean <- (mean.Logger4 + mean.Logger5 + mean.Logger6) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 3){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 3)
      Day.VPD.Logger7 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 7)
      Day.VPD.Logger8 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 8)
      Day.VPD.Logger9 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 9)
      
      Day.VPD.Logger7.Step3 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int3]
      Day.VPD.Logger8.Step3 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int3]
      Day.VPD.Logger9.Step3 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int3]
      
      mean.Logger7 <- mean (Day.VPD.Logger7.Step3)
      mean.Logger8 <- mean (Day.VPD.Logger8.Step3)
      mean.Logger9 <- mean (Day.VPD.Logger9.Step3)
      
      VPD.step3.mean <- (mean.Logger7 + mean.Logger8 + mean.Logger9) / 3
    }
  
    #############################################################################################################
    
    #STEP4
    
    STEP4 <- balance.name$TR.RATE[balance.name$Date.Time %within% int4]
    mean.TR.step4 <- mean(STEP4, na.rm = TRUE)
    if (weight.metadata_ss$Chamber[k] == 1){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 1)
      Day.VPD.Logger1 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 1)
      Day.VPD.Logger2 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 2)
      Day.VPD.Logger3 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 3)
      
      Day.VPD.Logger1.Step4 <- Day.VPD.Logger1$VPD[Day.VPD.Logger1$Date.Time %within% vpd.int4]
      Day.VPD.Logger2.Step4 <- Day.VPD.Logger2$VPD[Day.VPD.Logger2$Date.Time %within% vpd.int4]
      Day.VPD.Logger3.Step4 <- Day.VPD.Logger3$VPD[Day.VPD.Logger3$Date.Time %within% vpd.int4]
      
      mean.Logger1 <- mean (Day.VPD.Logger1.Step4)
      mean.Logger2 <- mean (Day.VPD.Logger2.Step4)
      mean.Logger3 <- mean (Day.VPD.Logger3.Step4)
      
      VPD.step4.mean <- (mean.Logger1 + mean.Logger2 + mean.Logger3) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 2){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 2)
      Day.VPD.Logger4 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 4)
      Day.VPD.Logger5 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 5)
      Day.VPD.Logger6 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 6)
      
      Day.VPD.Logger4.Step4 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int4]
      Day.VPD.Logger5.Step4 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int4]
      Day.VPD.Logger6.Step4 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int4]
      
      mean.Logger4 <- mean (Day.VPD.Logger4.Step4)
      mean.Logger5 <- mean (Day.VPD.Logger5.Step4)
      mean.Logger6 <- mean (Day.VPD.Logger6.Step4)
      
      VPD.step4.mean <- (mean.Logger4 + mean.Logger5 + mean.Logger6) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 3){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 3)
      Day.VPD.Logger7 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 7)
      Day.VPD.Logger8 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 8)
      Day.VPD.Logger9 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 9)
      
      Day.VPD.Logger7.Step4 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int4]
      Day.VPD.Logger8.Step4 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int4]
      Day.VPD.Logger9.Step4 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int4]
      
      mean.Logger7 <- mean (Day.VPD.Logger7.Step4)
      mean.Logger8 <- mean (Day.VPD.Logger8.Step4)
      mean.Logger9 <- mean (Day.VPD.Logger9.Step4)
      
      VPD.step4.mean <- (mean.Logger7 + mean.Logger8 + mean.Logger9) / 3
    }
    
    #############################################################################################################
    
    #STEP5
    
    STEP5 <- balance.name$TR.RATE[balance.name$Date.Time %within% int5]
    mean.TR.step5 <- mean(STEP5, na.rm = TRUE)
    if (weight.metadata_ss$Chamber[k] == 1){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 1)
      Day.VPD.Logger1 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 1)
      Day.VPD.Logger2 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 2)
      Day.VPD.Logger3 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 3)
      
      Day.VPD.Logger1.Step5 <- Day.VPD.Logger1$VPD[Day.VPD.Logger1$Date.Time %within% vpd.int5]
      Day.VPD.Logger2.Step5 <- Day.VPD.Logger2$VPD[Day.VPD.Logger2$Date.Time %within% vpd.int5]
      Day.VPD.Logger3.Step5 <- Day.VPD.Logger3$VPD[Day.VPD.Logger3$Date.Time %within% vpd.int5]
      
      mean.Logger1 <- mean (Day.VPD.Logger1.Step5)
      mean.Logger2 <- mean (Day.VPD.Logger2.Step5)
      mean.Logger3 <- mean (Day.VPD.Logger3.Step5)
      
      VPD.step5.mean <- (mean.Logger1 + mean.Logger2 + mean.Logger3) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 2){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 2)
      Day.VPD.Logger4 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 4)
      Day.VPD.Logger5 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 5)
      Day.VPD.Logger6 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 6)
      
      Day.VPD.Logger4.Step5 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int5]
      Day.VPD.Logger5.Step5 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int5]
      Day.VPD.Logger6.Step5 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int5]
      
      mean.Logger4 <- mean (Day.VPD.Logger4.Step5)
      mean.Logger5 <- mean (Day.VPD.Logger5.Step5)
      mean.Logger6 <- mean (Day.VPD.Logger6.Step5)
      
      VPD.step5.mean <- (mean.Logger4 + mean.Logger5 + mean.Logger6) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 3){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 3)
      Day.VPD.Logger7 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 7)
      Day.VPD.Logger8 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 8)
      Day.VPD.Logger9 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 9)
      
      Day.VPD.Logger7.Step5 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int5]
      Day.VPD.Logger8.Step5 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int5]
      Day.VPD.Logger9.Step5 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int5]
      
      mean.Logger7 <- mean (Day.VPD.Logger7.Step5)
      mean.Logger8 <- mean (Day.VPD.Logger8.Step5)
      mean.Logger9 <- mean (Day.VPD.Logger9.Step5)
      
      VPD.step5.mean <- (mean.Logger7 + mean.Logger8 + mean.Logger9) / 3
    } 
    
    
    #############################################################################################################
    
    #STEP6
    
    STEP6 <- balance.name$TR.RATE[balance.name$Date.Time %within% int6]
    mean.TR.step6 <- mean(STEP6, na.rm = TRUE)
    if (weight.metadata_ss$Chamber[k] == 1){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 1)
      Day.VPD.Logger1 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 1)
      Day.VPD.Logger2 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 2)
      Day.VPD.Logger3 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 3)
      
      Day.VPD.Logger1.Step6 <- Day.VPD.Logger1$VPD[Day.VPD.Logger1$Date.Time %within% vpd.int6]
      Day.VPD.Logger2.Step6 <- Day.VPD.Logger2$VPD[Day.VPD.Logger2$Date.Time %within% vpd.int6]
      Day.VPD.Logger3.Step6 <- Day.VPD.Logger3$VPD[Day.VPD.Logger3$Date.Time %within% vpd.int6]
      
      mean.Logger1 <- mean (Day.VPD.Logger1.Step6)
      mean.Logger2 <- mean (Day.VPD.Logger2.Step6)
      mean.Logger3 <- mean (Day.VPD.Logger3.Step6)
      
      VPD.step6.mean <- (mean.Logger1 + mean.Logger2 + mean.Logger3) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 2){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 2)
      Day.VPD.Logger4 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 4)
      Day.VPD.Logger5 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 5)
      Day.VPD.Logger6 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 6)
      
      Day.VPD.Logger4.Step6 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int6]
      Day.VPD.Logger5.Step6 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int6]
      Day.VPD.Logger6.Step6 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int6]
      
      mean.Logger4 <- mean (Day.VPD.Logger4.Step6)
      mean.Logger5 <- mean (Day.VPD.Logger5.Step6)
      mean.Logger6 <- mean (Day.VPD.Logger6.Step6)
      
      VPD.step6.mean <- (mean.Logger4 + mean.Logger5 + mean.Logger6) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 3){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 3)
      Day.VPD.Logger7 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 7)
      Day.VPD.Logger8 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 8)
      Day.VPD.Logger9 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 9)
      
      Day.VPD.Logger7.Step6 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int6]
      Day.VPD.Logger8.Step6 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int6]
      Day.VPD.Logger9.Step6 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int6]
      
      mean.Logger7 <- mean (Day.VPD.Logger7.Step6)
      mean.Logger8 <- mean (Day.VPD.Logger8.Step6)
      mean.Logger9 <- mean (Day.VPD.Logger9.Step6)
      
      VPD.step6.mean <- (mean.Logger7 + mean.Logger8 + mean.Logger9) / 3
    }
    
    #############################################################################################################
    
    #STEP7
    
    STEP7 <- balance.name$TR.RATE[balance.name$Date.Time %within% int7]
    mean.TR.step7 <- mean(STEP7, na.rm = TRUE)
    if (weight.metadata_ss$Chamber[k] == 1){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 1)
      Day.VPD.Logger1 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 1)
      Day.VPD.Logger2 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 2)
      Day.VPD.Logger3 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 3)
      
      Day.VPD.Logger1.Step7 <- Day.VPD.Logger1$VPD[Day.VPD.Logger1$Date.Time %within% vpd.int7]
      Day.VPD.Logger2.Step7 <- Day.VPD.Logger2$VPD[Day.VPD.Logger2$Date.Time %within% vpd.int7]
      Day.VPD.Logger3.Step7 <- Day.VPD.Logger3$VPD[Day.VPD.Logger3$Date.Time %within% vpd.int7]
      
      mean.Logger1 <- mean (Day.VPD.Logger1.Step7)
      mean.Logger2 <- mean (Day.VPD.Logger2.Step7)
      mean.Logger3 <- mean (Day.VPD.Logger3.Step7)
      
      VPD.step7.mean <- (mean.Logger1 + mean.Logger2 + mean.Logger3) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 2){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 2)
      Day.VPD.Logger4 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 4)
      Day.VPD.Logger5 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 5)
      Day.VPD.Logger6 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 6)
      
      Day.VPD.Logger4.Step7 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int7]
      Day.VPD.Logger5.Step7 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int7]
      Day.VPD.Logger6.Step7 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int7]
      
      mean.Logger4 <- mean (Day.VPD.Logger4.Step7)
      mean.Logger5 <- mean (Day.VPD.Logger5.Step7)
      mean.Logger6 <- mean (Day.VPD.Logger6.Step7)
      
      VPD.step7.mean <- (mean.Logger4 + mean.Logger5 + mean.Logger6) / 3
    }
    
    else if (weight.metadata_ss$Chamber[k] == 3){ 
      Day.VPD_ss <- subset(weather.metadata_ss, weather.metadata_ss$Chamber == 3)
      Day.VPD.Logger7 <- subset (Day.VPD_ss, Day.VPD_ss$Logger == 7)
      Day.VPD.Logger8 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 8)
      Day.VPD.Logger9 <- subset( Day.VPD_ss, Day.VPD_ss$Logger == 9)
      
      Day.VPD.Logger7.Step7 <- Day.VPD.Logger4$VPD[Day.VPD.Logger4$Date.Time %within% vpd.int7]
      Day.VPD.Logger8.Step7 <- Day.VPD.Logger5$VPD[Day.VPD.Logger5$Date.Time %within% vpd.int7]
      Day.VPD.Logger9.Step7 <- Day.VPD.Logger6$VPD[Day.VPD.Logger6$Date.Time %within% vpd.int7]
      
      mean.Logger7 <- mean (Day.VPD.Logger7.Step7)
      mean.Logger8 <- mean (Day.VPD.Logger8.Step7)
      mean.Logger9 <- mean (Day.VPD.Logger9.Step7)
      
      VPD.step7.mean <- (mean.Logger7 + mean.Logger8 + mean.Logger9) / 3
    }

    
   ############################################################################################################   
      SN <- c(1:7)
      Transpiration<-c(mean.TR.step1, mean.TR.step2, mean.TR.step3, mean.TR.step4, mean.TR.step5, mean.TR.step6, mean.TR.step7)
      VPD <- c(VPD.step1.mean, VPD.step2.mean, VPD.step3.mean, VPD.step4.mean, VPD.step5.mean, VPD.step6.mean, VPD.step7.mean)
      Genotype <- c(rep(weight.metadata_ss$Genotype[k], times = 7))
      Balance <- c(weight.metadata_ss$Balance[k],"","","","","","")
      Replicate<-c(rep(weight.metadata_ss$Replicate[k], times = 7))
      Leaf.area <-c(weight.metadata_ss$Leaf.area[k],"","","","","","")
      Stage<-c(weight.metadata_ss$Stage[k],"","","","","","")
      Dry.weight<-c(weight.metadata_ss$Dry.weight[k],"","","","","","")
      Specific.leaf.area<-c(weight.metadata_ss$Specific.leaf.area[k],"","","","","","")
      Day <-c(rep(weight.metadata_ss$Day[k], times = 7))
      Mean.NTR <- c(mean.NTR,"","","","","","")
      SE.NTR <- c(SE.NTR, "","","","","","")
      VPD.NTR <- c(av.NTR.VPD,"","","","","","")
      #code <- c(weight.metadata_ss$Code[k], "0","0","0","0","0","0")
      
      
      Results<-data.frame(SN, Genotype, Replicate, Balance, Day, Leaf.area,Stage,Dry.weight,Specific.leaf.area,VPD, Transpiration, VPD.NTR, Mean.NTR, SE.NTR)
      write.table(Results, file= <path and filename>, row.names=FALSE,col.names=FALSE, sep=",", append = TRUE)
      
    }
    else if (file.exists(paste(k,".csv",sep=" "))==FALSE) 
    {}
  }

}




################################################################################
