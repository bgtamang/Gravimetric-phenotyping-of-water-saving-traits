#Several statistical analysis such as correlation, regression, t-test, ANOVA etc. assume that data follow a normal distribution
#or Gaussian distribution. Such tests are known as parametric tests as their validity is dependent on the distribution of the data used.

#Install and load required packages "dplyr" and "ggpubr)

install.packages ("dplyr")
install.packages ("ggpubr")

library (dplyr)
library (ggpubr)

#Import data file
my_data <- read.delim ("Combined families.txt", header = TRUE, sep = "\t") #This file contains nine variables
dim(my_data)

dplyr::sample_n(my_data,10) #Checking 10 rows

#Assessing the normality
# 1.First lets plot a density plot- to visually inspect if distribution is bell shaped or not.

ggdensity (my_data$Slope.T, main = "Density plot of slope.T", xlab = "Slope values")
ggdensity (my_data$Y.intercept.T, main = "Density plot of slope.T", xlab = "Y-intercept values")
ggdensity (my_data$Slope.L , main = "Density plot of slope.T", xlab = "Slope.L values")
ggdensity (my_data$Y.intercept.L , main = "Density plot of slope.T", xlab = "Y-intercept.L values")
ggdensity (my_data$Slope.H , main = "Density plot of slope.T", xlab = "Slope.H values")
ggdensity (my_data$Y.intercept.H , main = "Density plot of slope.T", xlab = "Y-intercept.H values")
ggdensity (my_data$LA , main = "Density plot of slope.T", xlab = "LA values")
ggdensity (my_data$DW , main = "Density plot of slope.T", xlab = "DW values")
ggdensity (my_data$SLA , main = "Density plot of slope.T", xlab = "SLA values")

# 2.Checking using Q-Q plot (quantile-quantile plot)
ggqqplot (my_data$Slope.T)
ggqqplot (my_data$Y.intercept.T)
ggqqplot (my_data$Slope.L)
ggqqplot (my_data$Y.intercept.L)
ggqqplot (my_data$Slope.H)
ggqqplot (my_data$Y.intercept.H)
ggqqplot (my_data$LA)
ggqqplot (my_data$DW)
ggqqplot (my_data$SLA)

# 3.Normality test
# Several methods such as Kolmogorov-Smirnov (K-S) normality test and Shapiro-Wilk's test are used. S-W provides better power than K-S and it is based on correlation between the data and the corresponding normal scores.
# Remember, normality tests are sensitive to sample size, particularly when sample size is small, it shows normality.

shapiro.test(my_data$Slope.T)
shapiro.test(my_data$Y.intercept.T)
shapiro.test (my_data$Slope.L)
shapiro.test (my_data$Y.intercept.L)
shapiro.test (my_data$Slope.H)
shapiro.test (my_data$Y.intercept.H)
shapiro.test (my_data$LA)
shapiro.test (my_data$DW)
shapiro.test (my_data$SLA)

#If the p value is greater than 0.05, that means distribution of the data are not significantly different from normal distribution and one can assume normality.
# If this is not the case, we might have to transform our data (log10, root square etc)

#log10 transformation 
my_data$log10.slopeT <- log10(my_data$Slope.T)
my_data$log10.Y.intercept.T <- log10(my_data$Y.intercept.T+67)
my_data$sqrt.Y.intercept.T <- sqrt(my_data$Y.intercept.T+67)
my_data$log10.slopeL <- log10(my_data$Slope.L)
my_data$log10.Y.intercept.L <- log10(my_data$Y.intercept.L+85)
my_data$log10.LA <- log10(my_data$LA)
my_data$log10.SLA <- log10(my_data$SLA)
my_data$log10.DW <- log10(my_data$DW)



#Test normality one by one.
ggdensity (my_data$log10.slopeT, main = "Density plot of slope.T", xlab = "Slope values")
ggqqplot (my_data$log10.slopeT)
shapiro.test(my_data$log10.slopeT)

ggdensity (my_data$log10.Y.intercept.T, main = "Density plot of slope.T", xlab = "Y.intercept values")
ggqqplot (my_data$log10.Y.intercept.T)
shapiro.test(my_data$log10.Y.intercept.T)

ggdensity (my_data$log10.slopeL, main = "Density plot of slope.T", xlab = "Slope.L values")
ggqqplot (my_data$log10.slopeL)
shapiro.test(my_data$log10.slopeL)

ggdensity (my_data$log10.Y.intercept.L, main = "Density plot of slope.T", xlab = "Y.intercept.L values")
ggqqplot (my_data$log10.Y.intercept.L)
shapiro.test(my_data$log10.Y.intercept.L)

ggdensity (my_data$log10.LA, main = "Density plot of slope.T", xlab = "LA values")
ggqqplot (my_data$log10.LA)
shapiro.test(my_data$log10.LA)

ggdensity (my_data$log10.SLA, main = "Density plot of slope.T", xlab = "SLA values")
ggqqplot (my_data$log10.SLA)
shapiro.test(my_data$log10.SLA)

ggdensity (my_data$log10.SLA, main = "Density plot of slope.T", xlab = "SLA values")
ggqqplot (my_data$log10.SLA)
shapiro.test(my_data$log10.SLA)

ggdensity (my_data$log10.DW, main = "Density plot of slope.T", xlab = "DW values")
ggqqplot (my_data$log10.DW)
shapiro.test(my_data$log10.DW)

write.table (my_data, file = "Combined.families.cleaned.txt", sep = "\t")


