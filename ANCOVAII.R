#A study is conducted to compare two teaching methods ( A  and B) and five training times (1, 3, 5, 7, or 9 hours) in a crossed design.  High school students are assigned completely at random to receive the one of the method x training  treatment combinations.  There were 3 students randomly assigned to each method x training combination, and at the end of the training session mastery scores were recorded. 

#The data are in the GitHub repo as Schools.csv.

#Load relevant R packages
library(ggplot2)
library(gplots)
library(agricolae)
library(lattice)
library(multcomp)

#Download data from GitHub
if(!file.exists("./Schools.csv")){download.file(url = "https://raw.githubusercontent.com/cdo03c/ANOVA-Coursework/master/Schools.csv", destfile = "./Schools.csv")}

#Load flora.csv data into a data frame
df <- read.csv("./Schools.csv")
str(df)

#Turn hours into categorical variables
df$hours <- factor(df$hours)

##1)	Plot the data to show the response variable  (master_score) vs. hours training for each method (on one graph).
g <- ggplot(df, aes(hours, mastery))
g <- g + geom_point(aes(shape = Method, color = Method))
g <- g + xlab("Hours of Training")
g <- g + ylab("Mastery Score")
g

##2)	Run an ANOVA to compare the two methods at each level of training with the output ANOVA table and appropriate mean comparisons.
df.anova <- aov(mastery ~ hours*Method, df)
anova(df.anova)
TukeyHSD(df.anova)

##Use ANCOVA to characterize the effect of increasing number of training hours on mastery scores for the two teaching methods.   Fit the response as a polynomial (order 2) function of training hours.  Show the output and indicate what factors are significant.

#Step 1: Are all regression slopes = 0.
#Method A
df$hours <- as.integer(df$hours)
df$hours2 <- df$hours^2
dfA.fit <- lm(mastery ~ hours + hours2 + hours*hours2, df[df$Method == 'A',])
summary.lm(dfA.fit)

#Method B
dfB.fit <- lm(mastery ~ hours + hours2 + hours*hours2, df[df$Method == 'B',])
summary.lm(dfB.fit)

#In both cases, the simple linear regressions are significant, so the slopes are not = 0.

#Step 2: Are the slopes equal?

df.ancova <- aov(mastery ~ hours + hours2 + Method + hours*Method + hours2*Method, df)
anova(df.ancova)

#The interaction terms for hours and hours2 are significant so the slopes are not equal.

#Step 3: Fit Unequal Slopes Models

dfA.fit2 <- lm(mastery ~ hours + hours2-1, df[df$Method == 'A',])
print(summary(dfA.fit2))

dfB.fit2 <- lm(mastery ~ hours + hours2-1, df[df$Method == 'B',])
print(summary(dfB.fit2))

#Method B produced greater (p><0.05) mastery skills compared to Method A in all but the lowest level of training.  Methods differed significantly in the linear trend with hours, and we found a greater (p<0.05) linear  increase in Method B.  Method B also had greater degree of positive quadratic  curvature (p<0.05) compared to Method A. 