getwd()
setwd("C:/Sagar Drive/")
getwd()

#Importing and Installing required libraries

library(dplyr)
library(psych)
library(MASS)
library(png)
library(UsingR)
library(tidyr)
library(plyr)
library(tidyverse)  
library(ggplot2)
library(gmodels)
library(knitr)
library(kableExtra)
library(psych)
library(stargazer)
library(ggpubr)
library(plotly)

#Part1

cats

write.csv(cats,"cats.csv")

str(cats)

#Creating Subsets for male and female

male_Cats <- subset(cats, subset=(cats$Sex=="M"))
female_Cats <- subset(cats, subset=(cats$Sex=="F"))

male_Cats
write.csv(male_Cats,"cats.csv")
str(male_Cats)

female_Cats
write.csv(female_Cats,"cats.csv")
str(female_Cats)


Hwt_Violin <- ggplot(cats, aes(x=Sex, y=Hwt, fill= Sex)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  labs(title ="Figure 1 :Cat's HWT Violin plot")

Hwt_Violin

Bwt_Violin <- ggplot(cats, aes(x=Sex, y=Bwt, fill= Sex)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  labs(title ="Figure 2 :Cat's BWT Violin plot")

Bwt_Violin

male_Cats %>%
  summarise(Mean = mean(Bwt),SD   = sd(Bwt),Num  = length(Bwt)) 

female_Cats %>%
  summarise(Mean = mean(Bwt),SD   = sd(Bwt),Num  = length(Bwt))

# Perform Shapiro test to check Normality of Data
with(male_Cats, shapiro.test(Bwt))

with(female_Cats, shapiro.test(Bwt))

#Perform test to check the Variance

var.test(Bwt ~ Sex, data = cats)

#Perform the T-test on cat's BWT

# Hypothesis
# Null Hypothesis H0:       Bodyweight of Male and Female cats are same.
# Alternative Hypothesis H1:Bodyweight of Male and Female cats are not same.

ans <- t.test(male_Cats$Bwt, female_Cats$Bwt, var.equal = FALSE)

Ttest <- broom::tidy(ans)

Ttest

write.csv(Ttest, "T-testMod4.csv")

#Part2

# create the Table for data

Sleep_Quality <- data.frame(matrix(ncol = 2, nrow = 10))
colnames(Sleep_Quality) <- c("Before", "After")
Sleep_Quality$Before <- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
Sleep_Quality$After  <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)

write.csv(Sleep_Quality, "T-testMod4.csv")

# Visualization

ggarrange(
  ggdensity(Sleep_Quality$Before, fill = "light blue", main="Before Workshop")
  ,ggdensity(Sleep_Quality$After, fill = "light blue", main="After Workshop")
  ,nrow = 1, ncol = 2)


#T-test using 0.05 and 0.01 conf. Level

# Hypothesis

# Null Hypothesis H0: Sleep quality before is less than the after workshop. 
# Alternative Hypothesis H1: Sleep quality before is not less than the after workshop.


SleepTtest1 <- t.test(Sleep_Quality$Before,
                      Sleep_Quality$After, 
                      paired = TRUE,
                      conf.level = 0.05, 
                      alternative = "less")

SleepTtest1 <- broom::tidy(SleepTtest1)

SleepTtest1

SleepTtest2 <- t.test(Sleep_Quality$Before,
                      Sleep_Quality$After, 
                      paired = TRUE,
                      conf.level = 0.1, 
                      alternative = "less")

SleepTtest2 <- broom::tidy(SleepTtest2)

SleepTtest2

write.csv(rbind(SleepTtest1,SleepTtest2),"SleepTtest.csv")

