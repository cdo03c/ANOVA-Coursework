#A study is conducted to compare two teaching methods ( A  and B) and five training times (1, 3, 5, 7, or 9 hours) in a crossed design.  High school students are assigned completely at random to receive the one of the method x training  treatment combinations.  There were 3 students randomly assigned to each method x training combination, and at the end of the training session mastery scores were recorded. 

#The data are in the GitHub repo as Schools.csv.


setwd("~/Dropbox/Cuyler School/Applied Stats/STAT502/Week 12")
library(ggplot2)
library(lattice)
library(multcomp)
df <- read.csv("./HW11_Schools.csv")
df$hours <- factor(df$hours)

##Question 1
g <- ggplot(df, aes(hours, mastery))
g <- g + geom_point(aes(shape = Method, color = Method))
g <- g + xlab("Hours of Training")
g <- g + ylab("Mastery Score")
g

##Question 2
df.anova <- aov(mastery ~ hours + Method + hours*Method, df)
anova(df.anova)
posthoc <- glht(df.anova, linfct = mcp(Method = "Tukey"))
summary(posthoc)

##Question 3

#Step 1: Are all regression slopes = 0.
#Method A
df$hours2 <- df$hours^2
dfA <- df[df$Method == 'A',]
dfA.fit <- lm(mastery ~ hours + hours2 + hours*hours2, dfA)
print(summary.lm(dfA.fit))

#Method B
dfB <- df[df$Method == 'B',]
dfB.fit <- lm(mastery ~ hours + hours2 + hours*hours2, dfB)
print(summary.lm(dfB.fit))

#Step 2: Are the slopes equal?
df.ancova <- aov(mastery ~ hours + hours2 + Method + hours*Method + hours2*Method, df)
anova(df.ancova)

#Step 3: Fit Unequal Slopes Models

dfA.fit2 <- lm(mastery ~ hours + hours2-1, dfA)
print(summary(dfA.fit2))

dfB.fit2 <- lm(mastery ~ hours + hours2-1, dfB)
print(summary(dfB.fit2))

posthoc2

plot(posthoc2)

fit4 <- lm(mastery ~ hours + Method + hours*Method - 1, df)
summary(fit4)
lm(fit4)
