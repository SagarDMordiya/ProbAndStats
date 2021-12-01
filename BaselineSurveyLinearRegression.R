getwd()
setwd("C:/Sagar Drive/")
getwd()

# install.packages("dplyr")
# install.packages("plyr")
# install.packages("tidyr")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("rmarkdown")
# install.packages("gmodels")
# install.packages("plotly")
# install.packages("FSA")

library("dplyr")
library("plyr")
library("tidyr")
library("tidyverse")
library("ggplot2")
library("ggpubr")
library("rmarkdown")
library("gmodels")
library("plotly")
library("FSA")
library(broom)

#Importing and displaying CSV file
Baseline_survey <- read.csv("New_baseline_survey.csv")
str(Baseline_survey)
summary(Baseline_survey)

Cor_Baseline_survey <- Baseline_survey[,c("Class","Programming_R","Central_Tendency","Measures_of_Variability","Bivariate_Concepts")]
Cor_Baseline_survey <- na.omit(Cor_Baseline_survey)
str(Cor_Baseline_survey)

#Correlation table
cor_table <- cor(Cor_Baseline_survey)
cor_table

#correlation chart
corrplot::corrplot(cor_table,
                   type = "lower", 
                   lab_size = 1, 
                   method = "circle"
)

#Correlation test
cor.test(Baseline_survey$Programming_R, Baseline_survey$Statistics_or_Analytics_Experience, method = c("pearson"))


reg_tab <- lm(Class ~ Bivariate_Concepts, data = Baseline_survey)
regTable <- tidy(reg_tab)
write.csv(regTable, "RegTable.csv")
str(reg_tab)
summary(reg_tab)
new<- summary(reg_tab)$coefficient
new

reg_tab02 <- lm(Measures_of_Variability ~ Probabilities_and_distributions, data = Baseline_survey)
regTable02 <- tidy(reg_tab02)
write.csv(regTable02, "regTable02.csv")
str(reg_tab02)
summary(reg_tab02)

