#A study of plant recovery in Burned vs. Unburned areas involved repeated measurements of the same plots over a 15 month period.   The floral count data, in stacked format, is in the .csv file ‘flora.csv’ found in the GitHub repo and is downloaded and prepared for analysis using the following code.

#Load relevant R packages
library(ggplot2)
library(nlme)
library(multcomp)
library(MuMIn)

#Download data from GitHub
if(!file.exists("./flora.csv")){download.file(url = "https://raw.githubusercontent.com/cdo03c/ANOVA-Coursework/master/flora.csv", destfile = "./flora.csv")}

#Load flora.csv data into a data frame while removing NULL columns
df <- read.csv("./flora.csv")[,1:4]

#Turning plot and time into categorical variables
df <- within(df, {
  plot   <- factor(plot)
  time <- factor(time)
})
  
##Plot the data, as response vs. time for each treatment level.
g <- ggplot(df, aes(time, resp))
g <- g + geom_point(aes(shape = trt, color = trt))
g <- g + xlab("Time in Months")
g <- g + ylab("Response")
g

##Run a repeated measures ANOVA, and determine which covariance structure to use (you can base your decision on AICC values).  Consider the Unstructured, Compound Symmetry, and Spatial Power covariance structures.   Show the process of your decision making and the Type 3 fixed effects for your final model.

#Fit an ANOVA model with Variance Components
fit.vc <- gls(resp ~ trt*time-1, data = df)

#Fit an ANOVA model with Unstructured Symmetry
fit.us <- gls(resp ~ trt*time-1, data = df,
              corr=corSymm(form=~1|plot))

#Fit an ANOVA model with Compound Symmetry covariance matrix
fit.cs <- gls(resp ~ trt * time-1, data = df,
              corr = corCompSymm(form= ~ 1 | plot))

#Fit an ANOVA model with Spatial Power or Continuous Autoregressive Process
#covariance matrix
fit.sp <- gls(resp ~ trt * time -1, data = df,
              corr = corCAR1(form= ~ 1 | plot))

#AICC comparison
AICc(fit.vc,fit.us,fit.cs,fit.sp)

#Summary for the spatial power model with the lowest AICc
anova(fit.sp)


#Fitting the final model variance component vs. spatial power comparison
fit.vc2 <- gls(resp ~ trt * time, data = df)
anova(fit.vc2)
fit.sp2 <- gls(resp ~ trt * time, data = df,
              corr = corCAR1(form= ~ 1 | plot))
anova(fit.sp2)
