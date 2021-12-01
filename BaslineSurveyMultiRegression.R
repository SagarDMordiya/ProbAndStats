getwd()
getwd()

#Importing and Installing required libraries

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
library("stargazer")


#Importing and displaying CSV file
Baseline_survey <- read.csv("New_baseline_survey.csv")
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



#Run regression using the entire sample (i.e. all observation)

multi_regression_01 <- lm(Work_Experience ~ Statistics_or_Analytics_Experience  + Programming_R	+ Central_Tendency	 + Measures_of_Variability	+ 
                            Bivariate_Concepts + Probabilities_and_distributions
 + Age + Class , data = final_data  )

summary(multi_regression_01)
regression_01_table <- tidy(multi_regression_01)
write.csv(regression_01_table, "Table01.csv")

#Task 2: Re-run regression using the entire sample and dummy variables (constructed from a categorical variable) as predictors.

#Term and Major: Categorical variable

dummy_variable <- final_data[c("Major", "Term")]
dumm_var <- fastDummies::dummy_cols(dummy_variable$Term)
final_data <- cbind(final_data, dumm_var)
write.csv(final_data, "Dummy_New_Baseline_survey.csv") 

multi_regression_02 <- lm(Work_Experience ~ Major + Statistics_or_Analytics_Experience + Programming_R	+ Central_Tendency	 + Measures_of_Variability	+ 
                            Bivariate_Concepts + Probabilities_and_distributions + Term, data = final_data)

summary(multi_regression_02)
regression_02_table <- tidy(multi_regression_02)
write.csv(regression_02_table , "Table02.csv")

#Task 3: Use the categorical variable to create subsets. 
#How subsets did you create? How many lines are there? 
#Run the regression for subset. 
#How does this impact your understanding of the impact of the categorical variable on the regression?

data1 <- filter(final_data, "2019 Fall" == 1 )
data2 <- filter(final_data, "2020 Spring" == 2 )
data3 <- filter(final_data, "2020 Winter" == 3)

Mul_reg_subset01 <- summary(lm(Statistics_or_Analytics_Experience ~ Central_Tendency + Programming_R, 
                               data = subset(final_data, Term =="2019 Fall"))) 
Mul_reg_subset01
table01_subset <- tidy(Mul_reg_subset01)
write.csv(subset_table01 , "table01_subset.csv")

Mul_reg_subset02 <- summary(lm(Statistics_or_Analytics_Experience ~ Central_Tendency + Programming_R, 
                               data = subset(final_data, Term =="2020 Spring")))
Mul_reg_subset02
table02_subset <- tidy(Mul_reg_subset02)
write.csv(table02_subset , "table02_subset.csv")

Mul_reg_subset03 <- summary(lm(Statistics_or_Analytics_Experience ~ Central_Tendency + Programming_R, 
                               data = subset(final_data, Term =="2020 Winter")))
Mul_reg_subset03
table03_subset <- tidy(Mul_reg_subset03)
write.csv(table03_subset , "table03_subset.csv")

#Task 4: Summarize the findings in a table or two (you may use stargzer( ) or bloom( ) or by hand or function of your choice).

stargazer(as.data.frame(regression_01_table),as.data.frame(regression_02_table),as.data.frame(table01_subset),as.data.frame(table02_subset),as.data.frame(table03_subset), align=TRUE, type = "html", title = c("Regression Analysis Question 1","Question 2","Question 3 :Subset 1","Question 3 :Subset 2","Question 3 :Subset 3"),out = "fiNAL.HTML",  no.space=TRUE)

