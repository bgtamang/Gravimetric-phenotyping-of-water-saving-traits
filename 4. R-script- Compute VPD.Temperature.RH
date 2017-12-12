# This files associated with this script is folder named "Experimental conditions files" to extract the VPD, temperature and
# relative humidity levels during experiments.


#################################################
#  Script starts
#################################################
rm(list=ls())
require (lubridate)

setwd()

weather.metadata  <- read.csv ("Weather.metadata.csv", sep = ",", header = TRUE)
VPD.metadata <- read.csv ("VPD.metadata.csv", sep = ",", header = TRUE)



####
weather.metadata$NTR.start     <- as.POSIXct (weather.metadata$NTR.start, format = "%m/%d/%y %H:%M")
weather.metadata$NTR.end       <- as.POSIXct (weather.metadata$NTR.end, format = "%m/%d/%y %H:%M")
weather.metadata$step1.start   <- as.POSIXct (weather.metadata$step1.start, format = "%m/%d/%y %H:%M")
weather.metadata$step1.end     <- as.POSIXct (weather.metadata$step1.end, format = "%m/%d/%y %H:%M")
weather.metadata$step2.start   <- as.POSIXct (weather.metadata$step2.start, format = "%m/%d/%y %H:%M")
weather.metadata$step2.end     <- as.POSIXct (weather.metadata$step2.end, format = "%m/%d/%y %H:%M")
weather.metadata$step3.start   <- as.POSIXct (weather.metadata$step3.start, format = "%m/%d/%y %H:%M")
weather.metadata$step3.end     <- as.POSIXct (weather.metadata$step3.end, format = "%m/%d/%y %H:%M")
weather.metadata$step4.start   <- as.POSIXct (weather.metadata$step4.start, format = "%m/%d/%y %H:%M")
weather.metadata$step4.end     <- as.POSIXct (weather.metadata$step4.end, format = "%m/%d/%y %H:%M")
weather.metadata$step5.start   <- as.POSIXct (weather.metadata$step5.start, format = "%m/%d/%y %H:%M")
weather.metadata$step5.end     <- as.POSIXct (weather.metadata$step5.end, format = "%m/%d/%y %H:%M")
weather.metadata$step6.start   <- as.POSIXct (weather.metadata$step6.start, format = "%m/%d/%y %H:%M")
weather.metadata$step6.end     <- as.POSIXct (weather.metadata$step6.end, format = "%m/%d/%y %H:%M")
weather.metadata$step7.start   <- as.POSIXct (weather.metadata$step7.start, format = "%m/%d/%y %H:%M")
weather.metadata$step7.end     <- as.POSIXct (weather.metadata$step7.end, format = "%m/%d/%y %H:%M")

VPD.metadata$Date.Time    <- as.POSIXct (VPD.metadata$Date.Time, format = "%m/%d/%y %H:%M")

####

for (i in 1:length(unique(weather.metadata$Exp))){
  weather.metadata.ss.exp <- subset (weather.metadata, weather.metadata$Exp == i)
  VPD.metadata.ss.exp  <- subset (VPD.metadata, VPD.metadata$Exp == i)
  
  for (j in 1:length(unique(weather.metadata.ss.exp$Day))){
    weather.metadata.ss.exp.day <- subset (weather.metadata.ss.exp, weather.metadata.ss.exp$Day == j)
    VPD.metadata.ss.exp.day  <- subset (VPD.metadata.ss.exp, VPD.metadata.ss.exp$Day == j)
   
     for (k in 1:length(weather.metadata.ss.exp.day$Chamber)) {
       
        #Interval defining for VPD because VPD loggers are 1 hr ahead of TR loggers and they need different intervals.
        vpd.ntr.start   <- weather.metadata.ss.exp.day$NTR.start[k] + 60*60
        vpd.ntr.end     <- weather.metadata.ss.exp.day$NTR.end[k] + 60*60
        vpd.step1.start <- weather.metadata.ss.exp.day$step1.start[k] + 60*60
        vpd.step1.end   <- weather.metadata.ss.exp.day$step1.end[k] + 60*60
        vpd.step2.start <- weather.metadata.ss.exp.day$step2.start[k] + 60*60
        vpd.step2.end   <- weather.metadata.ss.exp.day$step2.end[k] + 60*60
        vpd.step3.start <- weather.metadata.ss.exp.day$step3.start[k] + 60*60
        vpd.step3.end   <- weather.metadata.ss.exp.day$step3.end[k] + 60*60
        vpd.step4.start <- weather.metadata.ss.exp.day$step4.start[k] + 60*60
        vpd.step4.end   <- weather.metadata.ss.exp.day$step4.end[k] + 60*60
        vpd.step5.start <- weather.metadata.ss.exp.day$step5.start[k] + 60*60
        vpd.step5.end   <- weather.metadata.ss.exp.day$step5.end[k] + 60*60
        vpd.step6.start <- weather.metadata.ss.exp.day$step6.start[k] + 60*60
        vpd.step6.end   <- weather.metadata.ss.exp.day$step6.end[k] + 60*60
        vpd.step7.start <- weather.metadata.ss.exp.day$step7.start[k] + 60*60
        vpd.step7.end   <- weather.metadata.ss.exp.day$step7.end[k] + 60*60
        
        vpd.int.ntr     <- interval (vpd.ntr.start, vpd.ntr.end)
        vpd.int1        <- interval(vpd.step1.start, vpd.step1.end)
        vpd.int2        <- interval(vpd.step2.start, vpd.step2.end)
        vpd.int3        <- interval(vpd.step3.start, vpd.step3.end)
        vpd.int4        <- interval(vpd.step4.start, vpd.step4.end)
        vpd.int5        <- interval(vpd.step5.start, vpd.step5.end)
        vpd.int6        <- interval(vpd.step6.start, vpd.step6.end)
        vpd.int7        <- interval(vpd.step7.start, vpd.step7.end)
      
  

  
         if (weather.metadata.ss.exp.day$Chamber[k] == 1) {
              Chamber1 <- subset (VPD.metadata.ss.exp.day, VPD.metadata.ss.exp.day$Chamber == 1)
              Logger1 <- subset (Chamber1, Chamber1$Logger == 1)
              Logger2 <- subset (Chamber1, Chamber1$Logger == 2)
              Logger3 <- subset (Chamber1, Chamber1$Logger == 3)
      
              #Logger1  #########################################################################
      
              #NTR
              NTR.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int.ntr]
              NTR.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int.ntr]
              NTR.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int.ntr]
      
              av.NTR.VPD.l1  <- mean(NTR.VPD.int.l1)
              av.NTR.temp.l1 <- mean(NTR.temp.int.l1)
              av.NTR.RH.l1   <- mean(NTR.RH.int.l1)
      
              se.NTR.VPD.l1  <- sd (NTR.VPD.int.l1)  / sqrt (length(NTR.VPD.int.l1))
              se.NTR.temp.l1 <- sd (NTR.temp.int.l1) / sqrt (length(NTR.temp.int.l1))
              se.NTR.RH.l1   <- sd (NTR.RH.int.l1)   / sqrt (length(NTR.RH.int.l1))
      
              #Step1
              Step1.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int1]
              Step1.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int1]
              Step1.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int1]
      
              av.Step1.VPD.l1  <- mean(Step1.VPD.int.l1)
              av.Step1.temp.l1 <- mean(Step1.temp.int.l1)
              av.Step1.RH.l1   <- mean(Step1.RH.int.l1)
      
              se.Step1.VPD.l1  <- sd (Step1.VPD.int.l1)  / sqrt (length(Step1.VPD.int.l1))
              se.Step1.temp.l1 <- sd (Step1.temp.int.l1) / sqrt (length(Step1.temp.int.l1))
              se.Step1.RH.l1   <- sd (Step1.RH.int.l1)   / sqrt (length(Step1.RH.int.l1))
          
              #Step2
              Step2.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int2]
              Step2.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int2]
              Step2.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int2]
          
              av.Step2.VPD.l1  <- mean(Step2.VPD.int.l1)
              av.Step2.temp.l1 <- mean(Step2.temp.int.l1)
              av.Step2.RH.l1   <- mean(Step2.RH.int.l1)
          
              se.Step2.VPD.l1  <- sd (Step2.VPD.int.l1)  / sqrt (length(Step2.VPD.int.l1))
              se.Step2.temp.l1 <- sd (Step2.temp.int.l1) / sqrt (length(Step2.temp.int.l1))
              se.Step2.RH.l1   <- sd (Step2.RH.int.l1)   / sqrt (length(Step2.RH.int.l1))
          
              #Step3
              Step3.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int3]
              Step3.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int3]
              Step3.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int3]
          
              av.Step3.VPD.l1  <- mean(Step3.VPD.int.l1)
              av.Step3.temp.l1 <- mean(Step3.temp.int.l1)
              av.Step3.RH.l1   <- mean(Step3.RH.int.l1)
          
              se.Step3.VPD.l1  <- sd (Step3.VPD.int.l1)  / sqrt (length(Step3.VPD.int.l1))
              se.Step3.temp.l1 <- sd (Step3.temp.int.l1) / sqrt (length(Step3.temp.int.l1))
              se.Step3.RH.l1   <- sd (Step3.RH.int.l1)   / sqrt (length(Step3.RH.int.l1))
          
              #Step4
              Step4.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int4]
              Step4.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int4]
              Step4.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int4]
          
              av.Step4.VPD.l1  <- mean(Step4.VPD.int.l1)
              av.Step4.temp.l1 <- mean(Step4.temp.int.l1)
              av.Step4.RH.l1   <- mean(Step4.RH.int.l1)
          
              se.Step4.VPD.l1  <- sd (Step4.VPD.int.l1)  / sqrt (length(Step4.VPD.int.l1))
              se.Step4.temp.l1 <- sd (Step4.temp.int.l1) / sqrt (length(Step4.temp.int.l1))
              se.Step4.RH.l1   <- sd (Step4.RH.int.l1)   / sqrt (length(Step4.RH.int.l1))
          
              #Step5
              Step5.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int5]
              Step5.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int5]
              Step5.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int5]
          
              av.Step5.VPD.l1  <- mean(Step5.VPD.int.l1)
              av.Step5.temp.l1 <- mean(Step5.temp.int.l1)
              av.Step5.RH.l1   <- mean(Step5.RH.int.l1)
          
              se.Step5.VPD.l1  <- sd (Step5.VPD.int.l1)  / sqrt (length(Step5.VPD.int.l1))
              se.Step5.temp.l1 <- sd (Step5.temp.int.l1) / sqrt (length(Step5.temp.int.l1))
              se.Step5.RH.l1   <- sd (Step5.RH.int.l1)   / sqrt (length(Step5.RH.int.l1))
          
              #Step6
              Step6.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int6]
              Step6.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int6]
              Step6.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int6]
          
              av.Step6.VPD.l1  <- mean(Step6.VPD.int.l1)
              av.Step6.temp.l1 <- mean(Step6.temp.int.l1)
              av.Step6.RH.l1   <- mean(Step6.RH.int.l1)
          
              se.Step6.VPD.l1  <- sd (Step6.VPD.int.l1)  / sqrt (length(Step6.VPD.int.l1))
              se.Step6.temp.l1 <- sd (Step6.temp.int.l1) / sqrt (length(Step6.temp.int.l1))
              se.Step6.RH.l1   <- sd (Step6.RH.int.l1)   / sqrt (length(Step6.RH.int.l1))
          
              #Step7
              Step7.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int7]
              Step7.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int7]
              Step7.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int7]
          
              av.Step7.VPD.l1  <- mean(Step7.VPD.int.l1)
              av.Step7.temp.l1 <- mean(Step7.temp.int.l1)
              av.Step7.RH.l1   <- mean(Step7.RH.int.l1)
          
              se.Step7.VPD.l1  <- sd (Step7.VPD.int.l1)  / sqrt (length(Step7.VPD.int.l1))
              se.Step7.temp.l1 <- sd (Step7.temp.int.l1) / sqrt (length(Step7.temp.int.l1))
              se.Step7.RH.l1   <- sd (Step7.RH.int.l1)   / sqrt (length(Step7.RH.int.l1))
      
      
      
      #Logger2 #########################################################################
      NTR.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int.ntr]
      NTR.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int.ntr]
      NTR.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int.ntr]
      
      av.NTR.VPD.l2  <- mean(NTR.VPD.int.l2)
      av.NTR.temp.l2 <- mean(NTR.temp.int.l2)
      av.NTR.RH.l2   <- mean(NTR.RH.int.l2)
      
      se.NTR.VPD.l2  <- sd (NTR.VPD.int.l2) / sqrt (length(NTR.VPD.int.l2))
      se.NTR.temp.l2 <- sd (NTR.temp.int.l2) / sqrt (length(NTR.temp.int.l2))
      se.NTR.RH.l2   <- sd (NTR.RH.int.l2) / sqrt (length(NTR.RH.int.l2))
      
      #Step1
      Step1.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int1]
      Step1.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int1]
      Step1.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int1]
      
      av.Step1.VPD.l2  <- mean(Step1.VPD.int.l2)
      av.Step1.temp.l2 <- mean(Step1.temp.int.l2)
      av.Step1.RH.l2   <- mean(Step1.RH.int.l2)
      
      se.Step1.VPD.l2  <- sd (Step1.VPD.int.l2)  / sqrt (length(Step1.VPD.int.l2))
      se.Step1.temp.l2 <- sd (Step1.temp.int.l2) / sqrt (length(Step1.temp.int.l2))
      se.Step1.RH.l2   <- sd (Step1.RH.int.l2)   / sqrt (length(Step1.RH.int.l2))
      
      #Step2
      Step2.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int2]
      Step2.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int2]
      Step2.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int2]
      
      av.Step2.VPD.l2  <- mean(Step2.VPD.int.l2)
      av.Step2.temp.l2 <- mean(Step2.temp.int.l2)
      av.Step2.RH.l2   <- mean(Step2.RH.int.l2)
      
      se.Step2.VPD.l2  <- sd (Step2.VPD.int.l2)  / sqrt (length(Step2.VPD.int.l2))
      se.Step2.temp.l2 <- sd (Step2.temp.int.l2) / sqrt (length(Step2.temp.int.l2))
      se.Step2.RH.l2   <- sd (Step2.RH.int.l2)   / sqrt (length(Step2.RH.int.l2))
      
      #Step3
      Step3.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int3]
      Step3.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int3]
      Step3.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int3]
      
      av.Step3.VPD.l2  <- mean(Step3.VPD.int.l2)
      av.Step3.temp.l2 <- mean(Step3.temp.int.l2)
      av.Step3.RH.l2   <- mean(Step3.RH.int.l2)
      
      se.Step3.VPD.l2  <- sd (Step3.VPD.int.l2)  / sqrt (length(Step3.VPD.int.l2))
      se.Step3.temp.l2 <- sd (Step3.temp.int.l2) / sqrt (length(Step3.temp.int.l2))
      se.Step3.RH.l2   <- sd (Step3.RH.int.l2)   / sqrt (length(Step3.RH.int.l2))
      
      #Step4
      Step4.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int4]
      Step4.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int4]
      Step4.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int4]
      
      av.Step4.VPD.l2  <- mean(Step4.VPD.int.l2)
      av.Step4.temp.l2 <- mean(Step4.temp.int.l2)
      av.Step4.RH.l2   <- mean(Step4.RH.int.l2)
      
      se.Step4.VPD.l2  <- sd (Step4.VPD.int.l2)  / sqrt (length(Step4.VPD.int.l2))
      se.Step4.temp.l2 <- sd (Step4.temp.int.l2) / sqrt (length(Step4.temp.int.l2))
      se.Step4.RH.l2   <- sd (Step4.RH.int.l2)   / sqrt (length(Step4.RH.int.l2))
      
      #Step5
      Step5.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int5]
      Step5.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int5]
      Step5.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int5]
      
      av.Step5.VPD.l2  <- mean(Step5.VPD.int.l2)
      av.Step5.temp.l2 <- mean(Step5.temp.int.l2)
      av.Step5.RH.l2   <- mean(Step5.RH.int.l2)
      
      se.Step5.VPD.l2  <- sd (Step5.VPD.int.l2)  / sqrt (length(Step5.VPD.int.l2))
      se.Step5.temp.l2 <- sd (Step5.temp.int.l2) / sqrt (length(Step5.temp.int.l2))
      se.Step5.RH.l2   <- sd (Step5.RH.int.l2)   / sqrt (length(Step5.RH.int.l2))
      
      #Step6
      Step6.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int6]
      Step6.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int6]
      Step6.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int6]
      
      av.Step6.VPD.l2  <- mean(Step6.VPD.int.l2)
      av.Step6.temp.l2 <- mean(Step6.temp.int.l2)
      av.Step6.RH.l2   <- mean(Step6.RH.int.l2)
      
      se.Step6.VPD.l2  <- sd (Step6.VPD.int.l2)  / sqrt (length(Step6.VPD.int.l2))
      se.Step6.temp.l2 <- sd (Step6.temp.int.l2) / sqrt (length(Step6.temp.int.l2))
      se.Step6.RH.l2   <- sd (Step6.RH.int.l2)   / sqrt (length(Step6.RH.int.l2))
      
      #Step7
      Step7.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int7]
      Step7.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int7]
      Step7.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int7]
      
      av.Step7.VPD.l2  <- mean(Step7.VPD.int.l2)
      av.Step7.temp.l2 <- mean(Step7.temp.int.l2)
      av.Step7.RH.l2   <- mean(Step7.RH.int.l2)
      
      se.Step7.VPD.l2  <- sd (Step7.VPD.int.l2)  / sqrt (length(Step7.VPD.int.l2))
      se.Step7.temp.l2 <- sd (Step7.temp.int.l2) / sqrt (length(Step7.temp.int.l2))
      se.Step7.RH.l2   <- sd (Step7.RH.int.l2)   / sqrt (length(Step7.RH.int.l2))
      
      #Logger3 #########################################################################
      
      #NTR
      NTR.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int.ntr]
      NTR.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int.ntr]
      NTR.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int.ntr]
      
      av.NTR.VPD.l3  <- mean(NTR.VPD.int.l3)
      av.NTR.temp.l3 <- mean(NTR.temp.int.l3)
      av.NTR.RH.l3   <- mean(NTR.RH.int.l3)
      
      se.NTR.VPD.l3  <- sd (NTR.VPD.int.l3) / sqrt (length(NTR.VPD.int.l3))
      se.NTR.temp.l3 <- sd (NTR.temp.int.l3) / sqrt (length(NTR.temp.int.l3))
      se.NTR.RH.l3   <- sd (NTR.RH.int.l3) / sqrt (length(NTR.RH.int.l3))
      
      #Step1
      Step1.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int1]
      Step1.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int1]
      Step1.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int1]
      
      av.Step1.VPD.l3  <- mean(Step1.VPD.int.l3)
      av.Step1.temp.l3 <- mean(Step1.temp.int.l3)
      av.Step1.RH.l3   <- mean(Step1.RH.int.l3)
      
      se.Step1.VPD.l3  <- sd (Step1.VPD.int.l3)  / sqrt (length(Step1.VPD.int.l3))
      se.Step1.temp.l3 <- sd (Step1.temp.int.l3) / sqrt (length(Step1.temp.int.l3))
      se.Step1.RH.l3   <- sd (Step1.RH.int.l3)   / sqrt (length(Step1.RH.int.l3))
      
      #Step2
      Step2.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int2]
      Step2.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int2]
      Step2.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int2]
      
      av.Step2.VPD.l3  <- mean(Step2.VPD.int.l3)
      av.Step2.temp.l3 <- mean(Step2.temp.int.l3)
      av.Step2.RH.l3   <- mean(Step2.RH.int.l3)
      
      se.Step2.VPD.l3  <- sd (Step2.VPD.int.l3)  / sqrt (length(Step2.VPD.int.l3))
      se.Step2.temp.l3 <- sd (Step2.temp.int.l3) / sqrt (length(Step2.temp.int.l3))
      se.Step2.RH.l3   <- sd (Step2.RH.int.l3)   / sqrt (length(Step2.RH.int.l3))
      
      #Step3
      Step3.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int3]
      Step3.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int3]
      Step3.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int3]
      
      av.Step3.VPD.l3  <- mean(Step3.VPD.int.l3)
      av.Step3.temp.l3 <- mean(Step3.temp.int.l3)
      av.Step3.RH.l3   <- mean(Step3.RH.int.l3)
      
      se.Step3.VPD.l3  <- sd (Step3.VPD.int.l3)  / sqrt (length(Step3.VPD.int.l3))
      se.Step3.temp.l3 <- sd (Step3.temp.int.l3) / sqrt (length(Step3.temp.int.l3))
      se.Step3.RH.l3   <- sd (Step3.RH.int.l3)   / sqrt (length(Step3.RH.int.l3))
      
      #Step4
      Step4.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int4]
      Step4.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int4]
      Step4.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int4]
      
      av.Step4.VPD.l3  <- mean(Step4.VPD.int.l3)
      av.Step4.temp.l3 <- mean(Step4.temp.int.l3)
      av.Step4.RH.l3   <- mean(Step4.RH.int.l3)
      
      se.Step4.VPD.l3  <- sd (Step4.VPD.int.l3)  / sqrt (length(Step4.VPD.int.l3))
      se.Step4.temp.l3 <- sd (Step4.temp.int.l3) / sqrt (length(Step4.temp.int.l3))
      se.Step4.RH.l3   <- sd (Step4.RH.int.l3)   / sqrt (length(Step4.RH.int.l3))
      
      #Step5
      Step5.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int5]
      Step5.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int5]
      Step5.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int5]
      
      av.Step5.VPD.l3  <- mean(Step5.VPD.int.l3)
      av.Step5.temp.l3 <- mean(Step5.temp.int.l3)
      av.Step5.RH.l3   <- mean(Step5.RH.int.l3)
      
      se.Step5.VPD.l3  <- sd (Step5.VPD.int.l3)  / sqrt (length(Step5.VPD.int.l3))
      se.Step5.temp.l3 <- sd (Step5.temp.int.l3) / sqrt (length(Step5.temp.int.l3))
      se.Step5.RH.l3   <- sd (Step5.RH.int.l3)   / sqrt (length(Step5.RH.int.l3))
      
      #Step6
      Step6.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int6]
      Step6.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int6]
      Step6.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int6]
      
      av.Step6.VPD.l3  <- mean(Step6.VPD.int.l3)
      av.Step6.temp.l3 <- mean(Step6.temp.int.l3)
      av.Step6.RH.l3   <- mean(Step6.RH.int.l3)
      
      se.Step6.VPD.l3  <- sd (Step6.VPD.int.l3)  / sqrt (length(Step6.VPD.int.l3))
      se.Step6.temp.l3 <- sd (Step6.temp.int.l3) / sqrt (length(Step6.temp.int.l3))
      se.Step6.RH.l3   <- sd (Step6.RH.int.l3)   / sqrt (length(Step6.RH.int.l3))
      
      #Step7
      Step7.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int7]
      Step7.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int7]
      Step7.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int7]
      
      av.Step7.VPD.l3  <- mean(Step7.VPD.int.l3)
      av.Step7.temp.l3 <- mean(Step7.temp.int.l3)
      av.Step7.RH.l3   <- mean(Step7.RH.int.l3)
      
      se.Step7.VPD.l3  <- sd (Step7.VPD.int.l3)  / sqrt (length(Step7.VPD.int.l3))
      se.Step7.temp.l3 <- sd (Step7.temp.int.l3) / sqrt (length(Step7.temp.int.l3))
      se.Step7.RH.l3   <- sd (Step7.RH.int.l3)   / sqrt (length(Step7.RH.int.l3))
      
         }
        
    else if (weather.metadata.ss.exp.day$Chamber[k] == 2) {
          Chamber2 <- subset (VPD.metadata.ss.exp.day, VPD.metadata.ss.exp.day$Chamber == 2)
          Logger1 <- subset (Chamber2, Chamber2$Logger == 4)
          Logger2 <- subset (Chamber2, Chamber2$Logger == 5)
          Logger3 <- subset (Chamber2, Chamber2$Logger == 6)
          
          #Logger1 #########################################################################
          
          #NTR
          NTR.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int.ntr]
          NTR.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int.ntr]
          NTR.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int.ntr]
          
          av.NTR.VPD.l1  <- mean(NTR.VPD.int.l1)
          av.NTR.temp.l1 <- mean(NTR.temp.int.l1)
          av.NTR.RH.l1   <- mean(NTR.RH.int.l1)
          
          se.NTR.VPD.l1  <- sd (NTR.VPD.int.l1)  / sqrt (length(NTR.VPD.int.l1))
          se.NTR.temp.l1 <- sd (NTR.temp.int.l1) / sqrt (length(NTR.temp.int.l1))
          se.NTR.RH.l1   <- sd (NTR.RH.int.l1)   / sqrt (length(NTR.RH.int.l1))
          
          #Step1
          Step1.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int1]
          Step1.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int1]
          Step1.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int1]
          
          av.Step1.VPD.l1  <- mean(Step1.VPD.int.l1)
          av.Step1.temp.l1 <- mean(Step1.temp.int.l1)
          av.Step1.RH.l1   <- mean(Step1.RH.int.l1)
          
          se.Step1.VPD.l1  <- sd (Step1.VPD.int.l1)  / sqrt (length(Step1.VPD.int.l1))
          se.Step1.temp.l1 <- sd (Step1.temp.int.l1) / sqrt (length(Step1.temp.int.l1))
          se.Step1.RH.l1   <- sd (Step1.RH.int.l1)   / sqrt (length(Step1.RH.int.l1))
          
          #Step2
          Step2.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int2]
          Step2.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int2]
          Step2.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int2]
          
          av.Step2.VPD.l1  <- mean(Step2.VPD.int.l1)
          av.Step2.temp.l1 <- mean(Step2.temp.int.l1)
          av.Step2.RH.l1   <- mean(Step2.RH.int.l1)
          
          se.Step2.VPD.l1  <- sd (Step2.VPD.int.l1)  / sqrt (length(Step2.VPD.int.l1))
          se.Step2.temp.l1 <- sd (Step2.temp.int.l1) / sqrt (length(Step2.temp.int.l1))
          se.Step2.RH.l1   <- sd (Step2.RH.int.l1)   / sqrt (length(Step2.RH.int.l1))
          
          #Step3
          Step3.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int3]
          Step3.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int3]
          Step3.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int3]
          
          av.Step3.VPD.l1  <- mean(Step3.VPD.int.l1)
          av.Step3.temp.l1 <- mean(Step3.temp.int.l1)
          av.Step3.RH.l1   <- mean(Step3.RH.int.l1)
          
          se.Step3.VPD.l1  <- sd (Step3.VPD.int.l1)  / sqrt (length(Step3.VPD.int.l1))
          se.Step3.temp.l1 <- sd (Step3.temp.int.l1) / sqrt (length(Step3.temp.int.l1))
          se.Step3.RH.l1   <- sd (Step3.RH.int.l1)   / sqrt (length(Step3.RH.int.l1))
          
          #Step4
          Step4.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int4]
          Step4.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int4]
          Step4.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int4]
          
          av.Step4.VPD.l1  <- mean(Step4.VPD.int.l1)
          av.Step4.temp.l1 <- mean(Step4.temp.int.l1)
          av.Step4.RH.l1   <- mean(Step4.RH.int.l1)
          
          se.Step4.VPD.l1  <- sd (Step4.VPD.int.l1)  / sqrt (length(Step4.VPD.int.l1))
          se.Step4.temp.l1 <- sd (Step4.temp.int.l1) / sqrt (length(Step4.temp.int.l1))
          se.Step4.RH.l1   <- sd (Step4.RH.int.l1)   / sqrt (length(Step4.RH.int.l1))
          
          #Step5
          Step5.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int5]
          Step5.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int5]
          Step5.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int5]
          
          av.Step5.VPD.l1  <- mean(Step5.VPD.int.l1)
          av.Step5.temp.l1 <- mean(Step5.temp.int.l1)
          av.Step5.RH.l1   <- mean(Step5.RH.int.l1)
          
          se.Step5.VPD.l1  <- sd (Step5.VPD.int.l1)  / sqrt (length(Step5.VPD.int.l1))
          se.Step5.temp.l1 <- sd (Step5.temp.int.l1) / sqrt (length(Step5.temp.int.l1))
          se.Step5.RH.l1   <- sd (Step5.RH.int.l1)   / sqrt (length(Step5.RH.int.l1))
          
          #Step6
          Step6.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int6]
          Step6.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int6]
          Step6.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int6]
          
          av.Step6.VPD.l1  <- mean(Step6.VPD.int.l1)
          av.Step6.temp.l1 <- mean(Step6.temp.int.l1)
          av.Step6.RH.l1   <- mean(Step6.RH.int.l1)
          
          se.Step6.VPD.l1  <- sd (Step6.VPD.int.l1)  / sqrt (length(Step6.VPD.int.l1))
          se.Step6.temp.l1 <- sd (Step6.temp.int.l1) / sqrt (length(Step6.temp.int.l1))
          se.Step6.RH.l1   <- sd (Step6.RH.int.l1)   / sqrt (length(Step6.RH.int.l1))
          
          #Step7
          Step7.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int7]
          Step7.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int7]
          Step7.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int7]
          
          av.Step7.VPD.l1  <- mean(Step7.VPD.int.l1)
          av.Step7.temp.l1 <- mean(Step7.temp.int.l1)
          av.Step7.RH.l1   <- mean(Step7.RH.int.l1)
          
          se.Step7.VPD.l1  <- sd (Step7.VPD.int.l1)  / sqrt (length(Step7.VPD.int.l1))
          se.Step7.temp.l1 <- sd (Step7.temp.int.l1) / sqrt (length(Step7.temp.int.l1))
          se.Step7.RH.l1   <- sd (Step7.RH.int.l1)   / sqrt (length(Step7.RH.int.l1))
          
          
          
          #Logger2  #########################################################################
          
          NTR.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int.ntr]
          NTR.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int.ntr]
          NTR.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int.ntr]
          
          av.NTR.VPD.l2  <- mean(NTR.VPD.int.l2)
          av.NTR.temp.l2 <- mean(NTR.temp.int.l2)
          av.NTR.RH.l2   <- mean(NTR.RH.int.l2)
          
          se.NTR.VPD.l2  <- sd (NTR.VPD.int.l2) / sqrt (length(NTR.VPD.int.l2))
          se.NTR.temp.l2 <- sd (NTR.temp.int.l2) / sqrt (length(NTR.temp.int.l2))
          se.NTR.RH.l2   <- sd (NTR.RH.int.l2) / sqrt (length(NTR.RH.int.l2))
          
          #Step1
          Step1.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int1]
          Step1.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int1]
          Step1.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int1]
          
          av.Step1.VPD.l2  <- mean(Step1.VPD.int.l2)
          av.Step1.temp.l2 <- mean(Step1.temp.int.l2)
          av.Step1.RH.l2   <- mean(Step1.RH.int.l2)
          
          se.Step1.VPD.l2  <- sd (Step1.VPD.int.l2)  / sqrt (length(Step1.VPD.int.l2))
          se.Step1.temp.l2 <- sd (Step1.temp.int.l2) / sqrt (length(Step1.temp.int.l2))
          se.Step1.RH.l2   <- sd (Step1.RH.int.l2)   / sqrt (length(Step1.RH.int.l2))
          
          #Step2
          Step2.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int2]
          Step2.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int2]
          Step2.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int2]
          
          av.Step2.VPD.l2  <- mean(Step2.VPD.int.l2)
          av.Step2.temp.l2 <- mean(Step2.temp.int.l2)
          av.Step2.RH.l2   <- mean(Step2.RH.int.l2)
          
          se.Step2.VPD.l2  <- sd (Step2.VPD.int.l2)  / sqrt (length(Step2.VPD.int.l2))
          se.Step2.temp.l2 <- sd (Step2.temp.int.l2) / sqrt (length(Step2.temp.int.l2))
          se.Step2.RH.l2   <- sd (Step2.RH.int.l2)   / sqrt (length(Step2.RH.int.l2))
          
          #Step3
          Step3.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int3]
          Step3.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int3]
          Step3.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int3]
          
          av.Step3.VPD.l2  <- mean(Step3.VPD.int.l2)
          av.Step3.temp.l2 <- mean(Step3.temp.int.l2)
          av.Step3.RH.l2   <- mean(Step3.RH.int.l2)
          
          se.Step3.VPD.l2  <- sd (Step3.VPD.int.l2)  / sqrt (length(Step3.VPD.int.l2))
          se.Step3.temp.l2 <- sd (Step3.temp.int.l2) / sqrt (length(Step3.temp.int.l2))
          se.Step3.RH.l2   <- sd (Step3.RH.int.l2)   / sqrt (length(Step3.RH.int.l2))
          
          #Step4
          Step4.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int4]
          Step4.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int4]
          Step4.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int4]
          
          av.Step4.VPD.l2  <- mean(Step4.VPD.int.l2)
          av.Step4.temp.l2 <- mean(Step4.temp.int.l2)
          av.Step4.RH.l2   <- mean(Step4.RH.int.l2)
          
          se.Step4.VPD.l2  <- sd (Step4.VPD.int.l2)  / sqrt (length(Step4.VPD.int.l2))
          se.Step4.temp.l2 <- sd (Step4.temp.int.l2) / sqrt (length(Step4.temp.int.l2))
          se.Step4.RH.l2   <- sd (Step4.RH.int.l2)   / sqrt (length(Step4.RH.int.l2))
          
          #Step5
          Step5.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int5]
          Step5.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int5]
          Step5.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int5]
          
          av.Step5.VPD.l2  <- mean(Step5.VPD.int.l2)
          av.Step5.temp.l2 <- mean(Step5.temp.int.l2)
          av.Step5.RH.l2   <- mean(Step5.RH.int.l2)
          
          se.Step5.VPD.l2  <- sd (Step5.VPD.int.l2)  / sqrt (length(Step5.VPD.int.l2))
          se.Step5.temp.l2 <- sd (Step5.temp.int.l2) / sqrt (length(Step5.temp.int.l2))
          se.Step5.RH.l2   <- sd (Step5.RH.int.l2)   / sqrt (length(Step5.RH.int.l2))
          
          #Step6
          Step6.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int6]
          Step6.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int6]
          Step6.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int6]
          
          av.Step6.VPD.l2  <- mean(Step6.VPD.int.l2)
          av.Step6.temp.l2 <- mean(Step6.temp.int.l2)
          av.Step6.RH.l2   <- mean(Step6.RH.int.l2)
          
          se.Step6.VPD.l2  <- sd (Step6.VPD.int.l2)  / sqrt (length(Step6.VPD.int.l2))
          se.Step6.temp.l2 <- sd (Step6.temp.int.l2) / sqrt (length(Step6.temp.int.l2))
          se.Step6.RH.l2   <- sd (Step6.RH.int.l2)   / sqrt (length(Step6.RH.int.l2))
          
          #Step7
          Step7.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int7]
          Step7.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int7]
          Step7.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int7]
          
          av.Step7.VPD.l2  <- mean(Step7.VPD.int.l2)
          av.Step7.temp.l2 <- mean(Step7.temp.int.l2)
          av.Step7.RH.l2   <- mean(Step7.RH.int.l2)
          
          se.Step7.VPD.l2  <- sd (Step7.VPD.int.l2)  / sqrt (length(Step7.VPD.int.l2))
          se.Step7.temp.l2 <- sd (Step7.temp.int.l2) / sqrt (length(Step7.temp.int.l2))
          se.Step7.RH.l2   <- sd (Step7.RH.int.l2)   / sqrt (length(Step7.RH.int.l2))
          
          #Logger3 #########################################################################
          
          #NTR
          NTR.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int.ntr]
          NTR.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int.ntr]
          NTR.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int.ntr]
          
          av.NTR.VPD.l3  <- mean(NTR.VPD.int.l3)
          av.NTR.temp.l3 <- mean(NTR.temp.int.l3)
          av.NTR.RH.l3   <- mean(NTR.RH.int.l3)
          
          se.NTR.VPD.l3  <- sd (NTR.VPD.int.l3) / sqrt (length(NTR.VPD.int.l3))
          se.NTR.temp.l3 <- sd (NTR.temp.int.l3) / sqrt (length(NTR.temp.int.l3))
          se.NTR.RH.l3   <- sd (NTR.RH.int.l3) / sqrt (length(NTR.RH.int.l3))
          
          #Step1
          Step1.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int1]
          Step1.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int1]
          Step1.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int1]
          
          av.Step1.VPD.l3  <- mean(Step1.VPD.int.l3)
          av.Step1.temp.l3 <- mean(Step1.temp.int.l3)
          av.Step1.RH.l3   <- mean(Step1.RH.int.l3)
          
          se.Step1.VPD.l3  <- sd (Step1.VPD.int.l3)  / sqrt (length(Step1.VPD.int.l3))
          se.Step1.temp.l3 <- sd (Step1.temp.int.l3) / sqrt (length(Step1.temp.int.l3))
          se.Step1.RH.l3   <- sd (Step1.RH.int.l3)   / sqrt (length(Step1.RH.int.l3))
          
          #Step2
          Step2.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int2]
          Step2.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int2]
          Step2.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int2]
          
          av.Step2.VPD.l3  <- mean(Step2.VPD.int.l3)
          av.Step2.temp.l3 <- mean(Step2.temp.int.l3)
          av.Step2.RH.l3   <- mean(Step2.RH.int.l3)
          
          se.Step2.VPD.l3  <- sd (Step2.VPD.int.l3)  / sqrt (length(Step2.VPD.int.l3))
          se.Step2.temp.l3 <- sd (Step2.temp.int.l3) / sqrt (length(Step2.temp.int.l3))
          se.Step2.RH.l3   <- sd (Step2.RH.int.l3)   / sqrt (length(Step2.RH.int.l3))
          
          #Step3
          Step3.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int3]
          Step3.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int3]
          Step3.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int3]
          
          av.Step3.VPD.l3  <- mean(Step3.VPD.int.l3)
          av.Step3.temp.l3 <- mean(Step3.temp.int.l3)
          av.Step3.RH.l3   <- mean(Step3.RH.int.l3)
          
          se.Step3.VPD.l3  <- sd (Step3.VPD.int.l3)  / sqrt (length(Step3.VPD.int.l3))
          se.Step3.temp.l3 <- sd (Step3.temp.int.l3) / sqrt (length(Step3.temp.int.l3))
          se.Step3.RH.l3   <- sd (Step3.RH.int.l3)   / sqrt (length(Step3.RH.int.l3))
          
          #Step4
          Step4.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int4]
          Step4.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int4]
          Step4.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int4]
          
          av.Step4.VPD.l3  <- mean(Step4.VPD.int.l3)
          av.Step4.temp.l3 <- mean(Step4.temp.int.l3)
          av.Step4.RH.l3   <- mean(Step4.RH.int.l3)
          
          se.Step4.VPD.l3  <- sd (Step4.VPD.int.l3)  / sqrt (length(Step4.VPD.int.l3))
          se.Step4.temp.l3 <- sd (Step4.temp.int.l3) / sqrt (length(Step4.temp.int.l3))
          se.Step4.RH.l3   <- sd (Step4.RH.int.l3)   / sqrt (length(Step4.RH.int.l3))
          
          #Step5
          Step5.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int5]
          Step5.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int5]
          Step5.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int5]
          
          av.Step5.VPD.l3  <- mean(Step5.VPD.int.l3)
          av.Step5.temp.l3 <- mean(Step5.temp.int.l3)
          av.Step5.RH.l3   <- mean(Step5.RH.int.l3)
          
          se.Step5.VPD.l3  <- sd (Step5.VPD.int.l3)  / sqrt (length(Step5.VPD.int.l3))
          se.Step5.temp.l3 <- sd (Step5.temp.int.l3) / sqrt (length(Step5.temp.int.l3))
          se.Step5.RH.l3   <- sd (Step5.RH.int.l3)   / sqrt (length(Step5.RH.int.l3))
          
          #Step6
          Step6.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int6]
          Step6.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int6]
          Step6.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int6]
          
          av.Step6.VPD.l3  <- mean(Step6.VPD.int.l3)
          av.Step6.temp.l3 <- mean(Step6.temp.int.l3)
          av.Step6.RH.l3   <- mean(Step6.RH.int.l3)
          
          se.Step6.VPD.l3  <- sd (Step6.VPD.int.l3)  / sqrt (length(Step6.VPD.int.l3))
          se.Step6.temp.l3 <- sd (Step6.temp.int.l3) / sqrt (length(Step6.temp.int.l3))
          se.Step6.RH.l3   <- sd (Step6.RH.int.l3)   / sqrt (length(Step6.RH.int.l3))
          
          #Step7
          Step7.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int7]
          Step7.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int7]
          Step7.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int7]
          
          av.Step7.VPD.l3  <- mean(Step7.VPD.int.l3)
          av.Step7.temp.l3 <- mean(Step7.temp.int.l3)
          av.Step7.RH.l3   <- mean(Step7.RH.int.l3)
          
          se.Step7.VPD.l3  <- sd (Step7.VPD.int.l3)  / sqrt (length(Step7.VPD.int.l3))
          se.Step7.temp.l3 <- sd (Step7.temp.int.l3) / sqrt (length(Step7.temp.int.l3))
          se.Step7.RH.l3   <- sd (Step7.RH.int.l3)   / sqrt (length(Step7.RH.int.l3))
    }
        
    else if (weather.metadata.ss.exp.day$Chamber[k] == 3) {
          Chamber3 <- subset (VPD.metadata.ss.exp.day, VPD.metadata.ss.exp.day$Chamber == 3)
          Logger1 <- subset (Chamber3, Chamber3$Logger == 7)
          Logger2 <- subset (Chamber3, Chamber3$Logger == 8)
          Logger3 <- subset (Chamber3, Chamber3$Logger == 9)
          
          #Logger1 #########################################################################
          
          #NTR
          NTR.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int.ntr]
          NTR.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int.ntr]
          NTR.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int.ntr]
          
          av.NTR.VPD.l1  <- mean(NTR.VPD.int.l1)
          av.NTR.temp.l1 <- mean(NTR.temp.int.l1)
          av.NTR.RH.l1   <- mean(NTR.RH.int.l1)
          
          se.NTR.VPD.l1  <- sd (NTR.VPD.int.l1)  / sqrt (length(NTR.VPD.int.l1))
          se.NTR.temp.l1 <- sd (NTR.temp.int.l1) / sqrt (length(NTR.temp.int.l1))
          se.NTR.RH.l1   <- sd (NTR.RH.int.l1)   / sqrt (length(NTR.RH.int.l1))
          
          #Step1
          Step1.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int1]
          Step1.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int1]
          Step1.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int1]
          
          av.Step1.VPD.l1  <- mean(Step1.VPD.int.l1)
          av.Step1.temp.l1 <- mean(Step1.temp.int.l1)
          av.Step1.RH.l1   <- mean(Step1.RH.int.l1)
          
          se.Step1.VPD.l1  <- sd (Step1.VPD.int.l1)  / sqrt (length(Step1.VPD.int.l1))
          se.Step1.temp.l1 <- sd (Step1.temp.int.l1) / sqrt (length(Step1.temp.int.l1))
          se.Step1.RH.l1   <- sd (Step1.RH.int.l1)   / sqrt (length(Step1.RH.int.l1))
          
          #Step2
          Step2.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int2]
          Step2.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int2]
          Step2.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int2]
          
          av.Step2.VPD.l1  <- mean(Step2.VPD.int.l1)
          av.Step2.temp.l1 <- mean(Step2.temp.int.l1)
          av.Step2.RH.l1   <- mean(Step2.RH.int.l1)
          
          se.Step2.VPD.l1  <- sd (Step2.VPD.int.l1)  / sqrt (length(Step2.VPD.int.l1))
          se.Step2.temp.l1 <- sd (Step2.temp.int.l1) / sqrt (length(Step2.temp.int.l1))
          se.Step2.RH.l1   <- sd (Step2.RH.int.l1)   / sqrt (length(Step2.RH.int.l1))
          
          #Step3
          Step3.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int3]
          Step3.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int3]
          Step3.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int3]
          
          av.Step3.VPD.l1  <- mean(Step3.VPD.int.l1)
          av.Step3.temp.l1 <- mean(Step3.temp.int.l1)
          av.Step3.RH.l1   <- mean(Step3.RH.int.l1)
          
          se.Step3.VPD.l1  <- sd (Step3.VPD.int.l1)  / sqrt (length(Step3.VPD.int.l1))
          se.Step3.temp.l1 <- sd (Step3.temp.int.l1) / sqrt (length(Step3.temp.int.l1))
          se.Step3.RH.l1   <- sd (Step3.RH.int.l1)   / sqrt (length(Step3.RH.int.l1))
          
          #Step4
          Step4.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int4]
          Step4.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int4]
          Step4.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int4]
          
          av.Step4.VPD.l1  <- mean(Step4.VPD.int.l1)
          av.Step4.temp.l1 <- mean(Step4.temp.int.l1)
          av.Step4.RH.l1   <- mean(Step4.RH.int.l1)
          
          se.Step4.VPD.l1  <- sd (Step4.VPD.int.l1)  / sqrt (length(Step4.VPD.int.l1))
          se.Step4.temp.l1 <- sd (Step4.temp.int.l1) / sqrt (length(Step4.temp.int.l1))
          se.Step4.RH.l1   <- sd (Step4.RH.int.l1)   / sqrt (length(Step4.RH.int.l1))
          
          #Step5
          Step5.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int5]
          Step5.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int5]
          Step5.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int5]
          
          av.Step5.VPD.l1  <- mean(Step5.VPD.int.l1)
          av.Step5.temp.l1 <- mean(Step5.temp.int.l1)
          av.Step5.RH.l1   <- mean(Step5.RH.int.l1)
          
          se.Step5.VPD.l1  <- sd (Step5.VPD.int.l1)  / sqrt (length(Step5.VPD.int.l1))
          se.Step5.temp.l1 <- sd (Step5.temp.int.l1) / sqrt (length(Step5.temp.int.l1))
          se.Step5.RH.l1   <- sd (Step5.RH.int.l1)   / sqrt (length(Step5.RH.int.l1))
          
          #Step6
          Step6.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int6]
          Step6.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int6]
          Step6.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int6]
          
          av.Step6.VPD.l1  <- mean(Step6.VPD.int.l1)
          av.Step6.temp.l1 <- mean(Step6.temp.int.l1)
          av.Step6.RH.l1   <- mean(Step6.RH.int.l1)
          
          se.Step6.VPD.l1  <- sd (Step6.VPD.int.l1)  / sqrt (length(Step6.VPD.int.l1))
          se.Step6.temp.l1 <- sd (Step6.temp.int.l1) / sqrt (length(Step6.temp.int.l1))
          se.Step6.RH.l1   <- sd (Step6.RH.int.l1)   / sqrt (length(Step6.RH.int.l1))
          
          #Step7
          Step7.VPD.int.l1  <- Logger1$VPD [Logger1$Date.Time %within% vpd.int7]
          Step7.temp.int.l1 <- Logger1$Temperature [Logger1$Date.Time %within% vpd.int7]
          Step7.RH.int.l1   <- Logger1$RH [Logger1$Date.Time %within% vpd.int7]
          
          av.Step7.VPD.l1  <- mean(Step7.VPD.int.l1)
          av.Step7.temp.l1 <- mean(Step7.temp.int.l1)
          av.Step7.RH.l1   <- mean(Step7.RH.int.l1)
          
          se.Step7.VPD.l1  <- sd (Step7.VPD.int.l1)  / sqrt (length(Step7.VPD.int.l1))
          se.Step7.temp.l1 <- sd (Step7.temp.int.l1) / sqrt (length(Step7.temp.int.l1))
          se.Step7.RH.l1   <- sd (Step7.RH.int.l1)   / sqrt (length(Step7.RH.int.l1))
          
          
          
          #Logger2 #########################################################################
          
          NTR.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int.ntr]
          NTR.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int.ntr]
          NTR.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int.ntr]
          
          av.NTR.VPD.l2  <- mean(NTR.VPD.int.l2)
          av.NTR.temp.l2 <- mean(NTR.temp.int.l2)
          av.NTR.RH.l2   <- mean(NTR.RH.int.l2)
          
          se.NTR.VPD.l2  <- sd (NTR.VPD.int.l2) / sqrt (length(NTR.VPD.int.l2))
          se.NTR.temp.l2 <- sd (NTR.temp.int.l2) / sqrt (length(NTR.temp.int.l2))
          se.NTR.RH.l2   <- sd (NTR.RH.int.l2) / sqrt (length(NTR.RH.int.l2))
          
          #Step1
          Step1.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int1]
          Step1.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int1]
          Step1.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int1]
          
          av.Step1.VPD.l2  <- mean(Step1.VPD.int.l2)
          av.Step1.temp.l2 <- mean(Step1.temp.int.l2)
          av.Step1.RH.l2   <- mean(Step1.RH.int.l2)
          
          se.Step1.VPD.l2  <- sd (Step1.VPD.int.l2)  / sqrt (length(Step1.VPD.int.l2))
          se.Step1.temp.l2 <- sd (Step1.temp.int.l2) / sqrt (length(Step1.temp.int.l2))
          se.Step1.RH.l2   <- sd (Step1.RH.int.l2)   / sqrt (length(Step1.RH.int.l2))
          
          #Step2
          Step2.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int2]
          Step2.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int2]
          Step2.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int2]
          
          av.Step2.VPD.l2  <- mean(Step2.VPD.int.l2)
          av.Step2.temp.l2 <- mean(Step2.temp.int.l2)
          av.Step2.RH.l2   <- mean(Step2.RH.int.l2)
          
          se.Step2.VPD.l2  <- sd (Step2.VPD.int.l2)  / sqrt (length(Step2.VPD.int.l2))
          se.Step2.temp.l2 <- sd (Step2.temp.int.l2) / sqrt (length(Step2.temp.int.l2))
          se.Step2.RH.l2   <- sd (Step2.RH.int.l2)   / sqrt (length(Step2.RH.int.l2))
          
          #Step3
          Step3.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int3]
          Step3.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int3]
          Step3.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int3]
          
          av.Step3.VPD.l2  <- mean(Step3.VPD.int.l2)
          av.Step3.temp.l2 <- mean(Step3.temp.int.l2)
          av.Step3.RH.l2   <- mean(Step3.RH.int.l2)
          
          se.Step3.VPD.l2  <- sd (Step3.VPD.int.l2)  / sqrt (length(Step3.VPD.int.l2))
          se.Step3.temp.l2 <- sd (Step3.temp.int.l2) / sqrt (length(Step3.temp.int.l2))
          se.Step3.RH.l2   <- sd (Step3.RH.int.l2)   / sqrt (length(Step3.RH.int.l2))
          
          #Step4
          Step4.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int4]
          Step4.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int4]
          Step4.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int4]
          
          av.Step4.VPD.l2  <- mean(Step4.VPD.int.l2)
          av.Step4.temp.l2 <- mean(Step4.temp.int.l2)
          av.Step4.RH.l2   <- mean(Step4.RH.int.l2)
          
          se.Step4.VPD.l2  <- sd (Step4.VPD.int.l2)  / sqrt (length(Step4.VPD.int.l2))
          se.Step4.temp.l2 <- sd (Step4.temp.int.l2) / sqrt (length(Step4.temp.int.l2))
          se.Step4.RH.l2   <- sd (Step4.RH.int.l2)   / sqrt (length(Step4.RH.int.l2))
          
          #Step5
          Step5.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int5]
          Step5.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int5]
          Step5.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int5]
          
          av.Step5.VPD.l2  <- mean(Step5.VPD.int.l2)
          av.Step5.temp.l2 <- mean(Step5.temp.int.l2)
          av.Step5.RH.l2   <- mean(Step5.RH.int.l2)
          
          se.Step5.VPD.l2  <- sd (Step5.VPD.int.l2)  / sqrt (length(Step5.VPD.int.l2))
          se.Step5.temp.l2 <- sd (Step5.temp.int.l2) / sqrt (length(Step5.temp.int.l2))
          se.Step5.RH.l2   <- sd (Step5.RH.int.l2)   / sqrt (length(Step5.RH.int.l2))
          
          #Step6
          Step6.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int6]
          Step6.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int6]
          Step6.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int6]
          
          av.Step6.VPD.l2  <- mean(Step6.VPD.int.l2)
          av.Step6.temp.l2 <- mean(Step6.temp.int.l2)
          av.Step6.RH.l2   <- mean(Step6.RH.int.l2)
          
          se.Step6.VPD.l2  <- sd (Step6.VPD.int.l2)  / sqrt (length(Step6.VPD.int.l2))
          se.Step6.temp.l2 <- sd (Step6.temp.int.l2) / sqrt (length(Step6.temp.int.l2))
          se.Step6.RH.l2   <- sd (Step6.RH.int.l2)   / sqrt (length(Step6.RH.int.l2))
          
          #Step7
          Step7.VPD.int.l2  <- Logger2$VPD [Logger2$Date.Time %within% vpd.int7]
          Step7.temp.int.l2 <- Logger2$Temperature [Logger2$Date.Time %within% vpd.int7]
          Step7.RH.int.l2   <- Logger2$RH [Logger2$Date.Time %within% vpd.int7]
          
          av.Step7.VPD.l2  <- mean(Step7.VPD.int.l2)
          av.Step7.temp.l2 <- mean(Step7.temp.int.l2)
          av.Step7.RH.l2   <- mean(Step7.RH.int.l2)
          
          se.Step7.VPD.l2  <- sd (Step7.VPD.int.l2)  / sqrt (length(Step7.VPD.int.l2))
          se.Step7.temp.l2 <- sd (Step7.temp.int.l2) / sqrt (length(Step7.temp.int.l2))
          se.Step7.RH.l2   <- sd (Step7.RH.int.l2)   / sqrt (length(Step7.RH.int.l2))
          
          #Logger3 #########################################################################
          
          #NTR
          NTR.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int.ntr]
          NTR.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int.ntr]
          NTR.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int.ntr]
          
          av.NTR.VPD.l3  <- mean(NTR.VPD.int.l3)
          av.NTR.temp.l3 <- mean(NTR.temp.int.l3)
          av.NTR.RH.l3   <- mean(NTR.RH.int.l3)
          
          se.NTR.VPD.l3  <- sd (NTR.VPD.int.l3) / sqrt (length(NTR.VPD.int.l3))
          se.NTR.temp.l3 <- sd (NTR.temp.int.l3) / sqrt (length(NTR.temp.int.l3))
          se.NTR.RH.l3   <- sd (NTR.RH.int.l3) / sqrt (length(NTR.RH.int.l3))
          
          #Step1
          Step1.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int1]
          Step1.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int1]
          Step1.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int1]
          
          av.Step1.VPD.l3  <- mean(Step1.VPD.int.l3)
          av.Step1.temp.l3 <- mean(Step1.temp.int.l3)
          av.Step1.RH.l3   <- mean(Step1.RH.int.l3)
          
          se.Step1.VPD.l3  <- sd (Step1.VPD.int.l3)  / sqrt (length(Step1.VPD.int.l3))
          se.Step1.temp.l3 <- sd (Step1.temp.int.l3) / sqrt (length(Step1.temp.int.l3))
          se.Step1.RH.l3   <- sd (Step1.RH.int.l3)   / sqrt (length(Step1.RH.int.l3))
          
          #Step2
          Step2.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int2]
          Step2.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int2]
          Step2.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int2]
          
          av.Step2.VPD.l3  <- mean(Step2.VPD.int.l3)
          av.Step2.temp.l3 <- mean(Step2.temp.int.l3)
          av.Step2.RH.l3   <- mean(Step2.RH.int.l3)
          
          se.Step2.VPD.l3  <- sd (Step2.VPD.int.l3)  / sqrt (length(Step2.VPD.int.l3))
          se.Step2.temp.l3 <- sd (Step2.temp.int.l3) / sqrt (length(Step2.temp.int.l3))
          se.Step2.RH.l3   <- sd (Step2.RH.int.l3)   / sqrt (length(Step2.RH.int.l3))
          
          #Step3
          Step3.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int3]
          Step3.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int3]
          Step3.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int3]
          
          av.Step3.VPD.l3  <- mean(Step3.VPD.int.l3)
          av.Step3.temp.l3 <- mean(Step3.temp.int.l3)
          av.Step3.RH.l3   <- mean(Step3.RH.int.l3)
          
          se.Step3.VPD.l3  <- sd (Step3.VPD.int.l3)  / sqrt (length(Step3.VPD.int.l3))
          se.Step3.temp.l3 <- sd (Step3.temp.int.l3) / sqrt (length(Step3.temp.int.l3))
          se.Step3.RH.l3   <- sd (Step3.RH.int.l3)   / sqrt (length(Step3.RH.int.l3))
          
          #Step4
          Step4.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int4]
          Step4.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int4]
          Step4.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int4]
          
          av.Step4.VPD.l3  <- mean(Step4.VPD.int.l3)
          av.Step4.temp.l3 <- mean(Step4.temp.int.l3)
          av.Step4.RH.l3   <- mean(Step4.RH.int.l3)
          
          se.Step4.VPD.l3  <- sd (Step4.VPD.int.l3)  / sqrt (length(Step4.VPD.int.l3))
          se.Step4.temp.l3 <- sd (Step4.temp.int.l3) / sqrt (length(Step4.temp.int.l3))
          se.Step4.RH.l3   <- sd (Step4.RH.int.l3)   / sqrt (length(Step4.RH.int.l3))
          
          #Step5
          Step5.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int5]
          Step5.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int5]
          Step5.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int5]
          
          av.Step5.VPD.l3  <- mean(Step5.VPD.int.l3)
          av.Step5.temp.l3 <- mean(Step5.temp.int.l3)
          av.Step5.RH.l3   <- mean(Step5.RH.int.l3)
          
          se.Step5.VPD.l3  <- sd (Step5.VPD.int.l3)  / sqrt (length(Step5.VPD.int.l3))
          se.Step5.temp.l3 <- sd (Step5.temp.int.l3) / sqrt (length(Step5.temp.int.l3))
          se.Step5.RH.l3   <- sd (Step5.RH.int.l3)   / sqrt (length(Step5.RH.int.l3))
          
          #Step6
          Step6.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int6]
          Step6.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int6]
          Step6.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int6]
          
          av.Step6.VPD.l3  <- mean(Step6.VPD.int.l3)
          av.Step6.temp.l3 <- mean(Step6.temp.int.l3)
          av.Step6.RH.l3   <- mean(Step6.RH.int.l3)
          
          se.Step6.VPD.l3  <- sd (Step6.VPD.int.l3)  / sqrt (length(Step6.VPD.int.l3))
          se.Step6.temp.l3 <- sd (Step6.temp.int.l3) / sqrt (length(Step6.temp.int.l3))
          se.Step6.RH.l3   <- sd (Step6.RH.int.l3)   / sqrt (length(Step6.RH.int.l3))
          
          #Step7
          Step7.VPD.int.l3  <- Logger3$VPD [Logger3$Date.Time %within% vpd.int7]
          Step7.temp.int.l3 <- Logger3$Temperature [Logger3$Date.Time %within% vpd.int7]
          Step7.RH.int.l3   <- Logger3$RH [Logger3$Date.Time %within% vpd.int7]
          
          av.Step7.VPD.l3  <- mean(Step7.VPD.int.l3)
          av.Step7.temp.l3 <- mean(Step7.temp.int.l3)
          av.Step7.RH.l3   <- mean(Step7.RH.int.l3)
          
          se.Step7.VPD.l3  <- sd (Step7.VPD.int.l3)  / sqrt (length(Step7.VPD.int.l3))
          se.Step7.temp.l3 <- sd (Step7.temp.int.l3) / sqrt (length(Step7.temp.int.l3))
          se.Step7.RH.l3   <- sd (Step7.RH.int.l3)   / sqrt (length(Step7.RH.int.l3))
        }
        ###################################################################################
        #VPD Values
        
        av.NTR.VPD <- (av.NTR.VPD.l1 + av.NTR.VPD.l2 + av.NTR.VPD.l3) / 3
        se.NTR.VPD <- (se.NTR.VPD.l1 + se.NTR.VPD.l2 + se.NTR.VPD.l3) / 3
        
        av.Step1.VPD <- (av.Step1.VPD.l1 + av.Step1.VPD.l2 + av.Step1.VPD.l3) / 3
        se.Step1.VPD <- (se.Step1.VPD.l1 + se.Step1.VPD.l2 + se.Step1.VPD.l3) / 3
        
        av.Step2.VPD <- (av.Step2.VPD.l1 + av.Step2.VPD.l2 + av.Step2.VPD.l3) / 3
        se.Step2.VPD <- (se.Step2.VPD.l1 + se.Step2.VPD.l2 + se.Step2.VPD.l3) / 3
        
        av.Step3.VPD <- (av.Step3.VPD.l1 + av.Step3.VPD.l2 + av.Step3.VPD.l3) / 3
        se.Step3.VPD <- (se.Step3.VPD.l1 + se.Step3.VPD.l2 + se.Step3.VPD.l3) / 3
        
        av.Step4.VPD <- (av.Step4.VPD.l1 + av.Step4.VPD.l2 + av.Step4.VPD.l3) / 3
        se.Step4.VPD <- (se.Step4.VPD.l1 + se.Step4.VPD.l2 + se.Step4.VPD.l3) / 3
        
        av.Step5.VPD <- (av.Step5.VPD.l1 + av.Step5.VPD.l2 + av.Step5.VPD.l3) / 3
        se.Step5.VPD <- (se.Step5.VPD.l1 + se.Step5.VPD.l2 + se.Step5.VPD.l3) / 3
        
        av.Step6.VPD <- (av.Step6.VPD.l1 + av.Step6.VPD.l2 + av.Step6.VPD.l3) / 3
        se.Step6.VPD <- (se.Step6.VPD.l1 + se.Step6.VPD.l2 + se.Step6.VPD.l3) / 3
        
        av.Step7.VPD <- (av.Step7.VPD.l1 + av.Step7.VPD.l2 + av.Step7.VPD.l3) / 3
        se.Step7.VPD <- (se.Step7.VPD.l1 + se.Step7.VPD.l2 + se.Step7.VPD.l3) / 3
        
        #Temperature values
        
        av.NTR.temp <- (av.NTR.temp.l1 + av.NTR.temp.l2 + av.NTR.temp.l3) / 3
        se.NTR.temp <- (se.NTR.temp.l1 + se.NTR.temp.l2 + se.NTR.temp.l3) / 3
        
        av.Step1.temp <- (av.Step1.temp.l1 + av.Step1.temp.l2 + av.Step1.temp.l3) / 3
        se.Step1.temp <- (se.Step1.temp.l1 + se.Step1.temp.l2 + se.Step1.temp.l3) / 3
        
        av.Step2.temp <- (av.Step2.temp.l1 + av.Step2.temp.l2 + av.Step2.temp.l3) / 3
        se.Step2.temp <- (se.Step2.temp.l1 + se.Step2.temp.l2 + se.Step2.temp.l3) / 3
        
        av.Step3.temp <- (av.Step3.temp.l1 + av.Step3.temp.l2 + av.Step3.temp.l3) / 3
        se.Step3.temp <- (se.Step3.temp.l1 + se.Step3.temp.l2 + se.Step3.temp.l3) / 3
        
        av.Step4.temp <- (av.Step4.temp.l1 + av.Step4.temp.l2 + av.Step4.temp.l3) / 3
        se.Step4.temp <- (se.Step4.temp.l1 + se.Step4.temp.l2 + se.Step4.temp.l3) / 3
        
        av.Step5.temp <- (av.Step5.temp.l1 + av.Step5.temp.l2 + av.Step5.temp.l3) / 3
        se.Step5.temp <- (se.Step5.temp.l1 + se.Step5.temp.l2 + se.Step5.temp.l3) / 3
        
        av.Step6.temp <- (av.Step6.temp.l1 + av.Step6.temp.l2 + av.Step6.temp.l3) / 3
        se.Step6.temp <- (se.Step6.temp.l1 + se.Step6.temp.l2 + se.Step6.temp.l3) / 3
        
        av.Step7.temp <- (av.Step7.temp.l1 + av.Step7.temp.l2 + av.Step7.temp.l3) / 3
        se.Step7.temp <- (se.Step7.temp.l1 + se.Step7.temp.l2 + se.Step7.temp.l3) / 3
        
        #Relative Humidity values
        
        av.NTR.RH <- (av.NTR.RH.l1 + av.NTR.RH.l2 + av.NTR.RH.l3) / 3
        se.NTR.RH <- (se.NTR.RH.l1 + se.NTR.RH.l2 + se.NTR.RH.l3) / 3
        
        av.Step1.RH <- (av.Step1.RH.l1 + av.Step1.RH.l2 + av.Step1.RH.l3) / 3
        se.Step1.RH <- (se.Step1.RH.l1 + se.Step1.RH.l2 + se.Step1.RH.l3) / 3
        
        av.Step2.RH <- (av.Step2.RH.l1 + av.Step2.RH.l2 + av.Step2.RH.l3) / 3
        se.Step2.RH <- (se.Step2.RH.l1 + se.Step2.RH.l2 + se.Step2.RH.l3) / 3
        
        av.Step3.RH <- (av.Step3.RH.l1 + av.Step3.RH.l2 + av.Step3.RH.l3) / 3
        se.Step3.RH <- (se.Step3.RH.l1 + se.Step3.RH.l2 + se.Step3.RH.l3) / 3
        
        av.Step4.RH <- (av.Step4.RH.l1 + av.Step4.RH.l2 + av.Step4.RH.l3) / 3
        se.Step4.RH <- (se.Step4.RH.l1 + se.Step4.RH.l2 + se.Step4.RH.l3) / 3
        
        av.Step5.RH <- (av.Step5.RH.l1 + av.Step5.RH.l2 + av.Step5.RH.l3) / 3
        se.Step5.RH <- (se.Step5.RH.l1 + se.Step5.RH.l2 + se.Step5.RH.l3) / 3
        
        av.Step6.RH <- (av.Step6.RH.l1 + av.Step6.RH.l2 + av.Step6.RH.l3) / 3
        se.Step6.RH <- (se.Step6.RH.l1 + se.Step6.RH.l2 + se.Step6.RH.l3) / 3
        
        av.Step7.RH <- (av.Step7.RH.l1 + av.Step7.RH.l2 + av.Step7.RH.l3) / 3
        se.Step7.RH <- (se.Step7.RH.l1 + se.Step7.RH.l2 + se.Step7.RH.l3) / 3
        
        ############################################################################
        
       SN <- c("NTR", "Step1", "Step2", "Step3", "Step4", "Step5", "Step6", "Step7")
       Exp <- c(weather.metadata.ss.exp.day$Exp[k],"","","","","","","")
       Day <- c(weather.metadata.ss.exp.day$Day[k],"","","","","","","")
       Chamber <- c(weather.metadata.ss.exp.day$Chamber[k],"","","","","","","") 
       VPD.av <- c(av.NTR.VPD, av.Step1.VPD, av.Step2.VPD, av.Step3.VPD, av.Step4.VPD, av.Step5.VPD, av.Step6.VPD, av.Step7.VPD)
       VPD.se <- c(se.NTR.VPD, se.Step1.VPD, se.Step2.VPD, se.Step3.VPD, se.Step4.VPD, se.Step5.VPD, se.Step6.VPD, se.Step7.VPD)
       temp.av <- c(av.NTR.temp, av.Step1.temp, av.Step2.temp, av.Step3.temp, av.Step4.temp, av.Step5.temp, av.Step6.temp, av.Step7.temp)
       temp.se <- c(se.NTR.temp, se.Step1.temp, se.Step2.temp, se.Step3.temp, se.Step4.temp, se.Step5.temp, se.Step6.temp, se.Step7.temp)
       RH.av <- c(av.NTR.RH, av.Step1.RH, av.Step2.RH, av.Step3.RH, av.Step4.RH, av.Step5.RH, av.Step6.RH, av.Step7.RH)
       RH.se <- c(se.NTR.RH, se.Step1.RH, se.Step2.RH, se.Step3.RH, se.Step4.RH, se.Step5.RH, se.Step6.RH, se.Step7.RH)
       
       results <- data.frame (SN, Exp, Day, Chamber, VPD.av, VPD.se, temp.av, temp.se, RH.av, RH.se)
       write.table (results, file= VPD.temp.RH.csv", row.names=FALSE,col.names= FALSE, sep=",", append = TRUE)
       
     }
   
     
    }
  }
#################################################
#  Script ends
#################################################

##########################################################################################################################################################################################################



