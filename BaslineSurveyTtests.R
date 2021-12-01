getwd()
setwd("C:/Sagar Drive/")
getwd()

#Importing and Installing required libraries

install.packages("dplyr")
install.packages("plyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("rmarkdown")
install.packages("gmodels")
install.packages("plotly")
install.packages("FSA")

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
Baseline_survey <- read.csv("Baseline survey.csv")
headtail(Baseline_survey,5)
Baseline_survey


#Coversion to Data frame.
baseline_df<-data.frame(Baseline_survey)
headtail(Baseline_survey,5)


#checking for null values
is.na(baseline_df)

#Dropping NA values record
baseline_df<-na.omit(baseline_df)

#Removing the Unanswered data
baseline_df <- baseline_df[!(baseline_df$Answer2 == "<Unanswered>"),]
baseline_df <- baseline_df[!(baseline_df$Answer3 == "<Unanswered>"),]
baseline_df <- baseline_df[!(baseline_df$Answer4 == "<Unanswered>"),]
baseline_df <- baseline_df[!(baseline_df$Answer5 == "<Unanswered>"),]
headtail(baseline_df)

#filtering values
filteredBaseline_data = filter(baseline_df, between(Answer5,1,5),
                               between(Answer6,1,5),
                               between(Answer7,1,5),
                               between(Answer8,1,5),
                               between(Answer9,1,5))
headtail(filteredBaseline_data)

#checking columns data type
str(filteredBaseline_data)

#converting data types
filteredBaseline_data$Answer2 = as.numeric(filteredBaseline_data$Answer2)
filteredBaseline_data$Answer3 = as.numeric(filteredBaseline_data$Answer3)
filteredBaseline_data$Answer4 = as.numeric(filteredBaseline_data$Answer4)
filteredBaseline_data$Answer5 = as.numeric(filteredBaseline_data$Answer5)
filteredBaseline_data$Answer6 = as.numeric(filteredBaseline_data$Answer6)
filteredBaseline_data$Answer7 = as.numeric(filteredBaseline_data$Answer7)
filteredBaseline_data$Answer8 = as.numeric(filteredBaseline_data$Answer8)
filteredBaseline_data$Answer9 = as.numeric(filteredBaseline_data$Answer9)
str(filteredBaseline_data)

#Rename column names
baselineQuest <- c(filteredBaseline_data$Question1[1],filteredBaseline_data$Question2[1],filteredBaseline_data$Question3[1],filteredBaseline_data$Question4[1],filteredBaseline_data$Question5[1],filteredBaseline_data$Question6[1],filteredBaseline_data$Question7[1],filteredBaseline_data$Question8[1],filteredBaseline_data$Question9[1])
colnames(filteredBaseline_data)[colnames(filteredBaseline_data) %in% 
                                  c("Answer1","Answer2","Answer3","Answer4","Answer5","Answer6","Answer7","Answer8","Answer9") ] = baselineQuest

colnames(filteredBaseline_data)

#Removing Unwanted column

final_data <-dplyr::select(filteredBaseline_data,-c(X,Question1,Question2,Question3,Question4,Question5,Question6,Question7,Question8,Question9))

#Rename column name with short name
colnames(final_data)<-c("Term", "Class", "Id", "Major", "Age", "Work_Experience",
                        "Statistics_or_Analytics_Experience","Programming_R", "Central_Tendency",
                        "Measures_of_Variability", "Bivariate_Concepts", "Probabilities_and_distributions")

headtail(final_data)
getwd()
write.csv(final_data,"Final_data.csv")
#Find out the mean of Age to perfrom the T-test()
mean(final_data$Age)

# Conduct a one-sample t-tests for mean using an appropriate variable(s) from the data set using a hypothesized value of your choice (usually 0). 
# Hypothesis
# Null Hypothesis H0:       Mean of age is equal to 24.48
# Alternative Hypothesis H1:Mean of age is not equal to 24.48

result <- t.test(final_data$Age, mu =24.48, var.equal = TRUE, conf.level = 0.95 )

Ttest_Age <- broom::tidy(result)

Ttest_Age

write.csv(Ttest_Age, "T-test_Age.csv")

# Hypothesis
# Null Hypothesis H0:       Mean of age is greater than 20
# Alternative Hypothesis H1:Mean of age is less than 20

result <- t.test(final_data$Age, mu =20, alternative = "less", var.equal = TRUE, conf.level = 0.95 )

Ttest_Age2 <- broom::tidy(result)

Ttest_Age2

write.csv(tidy_t_test, "T-test_Age2.csv")

# Conduct multiple one-sample t-tests for mean by group and present the result in the same table.

#Filter the data based on Major to perform T test for each group

as.data.frame(unique(final_data$Major))

ComputerIT_data = subset(final_data, final_data$Major=="Computer/IT")
Engineering_data = subset(final_data, final_data$Major=="Engineering")
Mathematics_data = subset(final_data, final_data$Major=="Mathematics")
Telecommunication_data = subset(final_data, final_data$Major=="Telecommunication")
Other_data = subset(final_data, final_data$Major=="Other")
Economics_data = subset(final_data, final_data$Major=="Economics")
Finance_data = subset(final_data, final_data$Major=="Finance")
Business_data = subset(final_data, final_data$Major=="Business")
Statistics_data = subset(final_data, final_data$Major=="Statistics")
Accounting_data = subset(final_data, final_data$Major=="Accounting")
GeologyGIS_data = subset(final_data, final_data$Major=="Geology/GIS")
ChemistryBiology_data = subset(final_data, final_data$Major=="Chemistry/Biology")

write.csv(Engineering_data,"Engineering_data.csv")

#Perform T-test for each group

result <- t.test(ComputerIT_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_comp <- broom::tidy(result)
Ttest_comp$Major <- "Computer/IT"

result <- t.test(Engineering_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_Eng <- broom::tidy(result)
Ttest_Eng$Major <- "Engineering"

result <- t.test(Mathematics_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_Math <- broom::tidy(result)
Ttest_Math$Major <- "Mathematics"

result <- t.test(Telecommunication_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_Tele <- broom::tidy(result)
Ttest_Tele$Major <- "Telecommunication"

result <- t.test(Other_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_Other <- broom::tidy(result)
Ttest_Other$Major <- "Other"


result <- t.test(Economics_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_Eco <- broom::tidy(result)
Ttest_Eco$Major <- "Economics"

result <- t.test(Finance_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_Fin <- broom::tidy(result)
Ttest_Fin$Major <- "Finance"

result <- t.test(Business_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_Busi <- broom::tidy(result)
Ttest_Busi$Major <- "Business"

result <- t.test(Statistics_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_Stat <- broom::tidy(result)
Ttest_Stat$Major <- "Statistics"

result <- t.test(Accounting_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_Acc <- broom::tidy(result)
Ttest_Acc$Major <- "Accounting"

result <- t.test(GeologyGIS_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_Geo <- broom::tidy(result)
Ttest_Geo$Major <- "Geology/GIS"

result <- t.test(ChemistryBiology_data$Work_Experience, mu =1, var.equal = TRUE)
Ttest_Chem <- broom::tidy(result)
Ttest_Chem$Major <- "Chemistry/Biology"

write.csv(rbind(Ttest_Acc,Ttest_Busi,
                Ttest_Chem,Ttest_comp,
                Ttest_Eco,Ttest_Eng,
                Ttest_Fin,Ttest_Geo,
                Ttest_Math,Ttest_Other,
                Ttest_Stat,Ttest_Tele),"TtestQ2Result.csv")


