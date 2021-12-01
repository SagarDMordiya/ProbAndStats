getwd()
setwd("C:/Sagar Drive/")
getwd()
#Importing and Installing required libraries
install.packages("dplyr")
install.packages("gmodels")
install.packages("readr")
install.packages("epiDisplay")
install.packages("plyr")
install.packages("FSA")

library("dplyr")
library("gmodels")
library("readr")
library("epiDisplay")
library("plyr")
library("FSA")

#Importing and displaying CSV file
Baseline_survey <- read_csv("Baseline survey.csv")
headtail(Baseline_survey,5)



#Prepare data.frame for analysis.
baseline_df<-data.frame(Baseline_survey)
headtail(Baseline_survey,5)


#check for null values
is.na(baseline_df)

#Dropped NA values
baseline_df<-na.omit(baseline_df)

#Replace the Unanswered data
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
filteredBaseline_data
headtail(filteredBaseline_data)

#checking columns data type
str(filteredBaseline_data)

#converting data types

filteredBaseline_data$Answer3 = as.numeric(filteredBaseline_data$Answer3)
filteredBaseline_data$Answer4 = as.numeric(filteredBaseline_data$Answer4)
filteredBaseline_data$Answer5 = as.numeric(filteredBaseline_data$Answer5)
filteredBaseline_data$Answer6 = as.numeric(filteredBaseline_data$Answer6)
filteredBaseline_data$Answer7 = as.numeric(filteredBaseline_data$Answer7)
filteredBaseline_data$Answer8 = as.numeric(filteredBaseline_data$Answer8)
filteredBaseline_data$Answer9 = as.numeric(filteredBaseline_data$Answer9)
filteredBaseline_data

#Rename column names
baselineQuest <- c(filteredBaseline_data$Question1[1],filteredBaseline_data$Question2[1],filteredBaseline_data$Question3[1],filteredBaseline_data$Question4[1],filteredBaseline_data$Question5[1],filteredBaseline_data$Question6[1],filteredBaseline_data$Question7[1],filteredBaseline_data$Question8[1],filteredBaseline_data$Question9[1])
colnames(filteredBaseline_data)[colnames(filteredBaseline_data) %in% 
  c("Answer1","Answer2","Answer3","Answer4","Answer5","Answer6","Answer7","Answer8","Answer9") ] = baselineQuest

colnames(filteredBaseline_data)

#Removing Unwanted column

final_data <- dplyr::selselect(filteredBaseline_data,-c(X1,Question1,Question2,Question3,Question4,Question5,Question6,Question7,Question8,Question9))

#Rename column name with short name
colnames(final_data)<- c("Term", "Class", "Id", "Major", "Age", "Work_Experience", "Statistics_or_Analytics_Experience","Programming_R", "Central_Tendency",
                         "Measures_of_Variability", "Bivariate_Concepts", "Probabilities_and_distributions")
headtail(final_data)


#Frequency table

Intakes_table = table(final_data$Term)
Intakes_table

Major_table = table(final_data$Major)
Major_table

class_table = table(final_data$Class)
class_table


#cross-tabulations

CrossTable(final_data$Term, final_data$Class, prob.t = TRUE)

CrossTable(final_data$Major,final_data$Class, prob.t = TRUE)

CrossTable(final_data$Statistics_or_Analytics_Experience, prob.t = TRUE)

CrossTable(final_data$Age, final_data$Work_Experience, prob.t = TRUE)

#Histogram
hist(final_data$Work_Experience, freq = TRUE,
     ylim = c(0,80),
     main = "Work Experinece",
     xlab = "Experience Level",
     las = 1,
     col = "Light Blue"
)


hist(final_data$Programming_R,
     breaks = 10,
     col = terrain.colors(16),
     xlab = "Ratings from 1 to 5",
     las = 1,
     main = " R Programming Expertise Level",
     xlim = c(0, 5)
)


hist(final_data$Probabilities_and_distributions,
     col = c("Light Green"),
     border = "Black",
     xlab =  "Expertise Level",
     main = "Probability And Concept Expertise",
     las = 1)

colMeans(subset(final_data, select = c("Work_Experience", "Statistics_or_Analytics_Experience","Programming_R", "Central_Tendency",
                                       "Measures_of_Variability", "Bivariate_Concepts", "Probabilities_and_distributions" )), na.rm = TRUE)

apply(baseline_df, 2, final_data)


