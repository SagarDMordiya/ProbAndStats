getwd()
setwd("C:/Sagar Drive/")
getwd()

#Importing and Installing required libraries

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
library(readr)
library(ggplot2)
library(dplyr)
library(ggcorrplot)

Milestone_dataset <- as.data.frame(read.csv(file = "NLSY1979_1996.csv"))

str(Milestone_dataset)

glimpse(Milestone_dataset)

cat("Dimensions of dataset = ",dim(Milestone_dataset)[1]," X ", dim(Milestone_dataset)[2], "\nMissing Values in Dataset = ", sum(complete.cases(Milestone_dataset)))
is.na(Milestone_dataset)

#Replacing Negative values with NA
Dataset <- Milestone_dataset
Dataset[Dataset < 0] <- NA
Milestone_dataset <- Dataset

str(Milestone_dataset)

glimpse(Milestone_dataset)

summary(Milestone_dataset)

Milestone_dataset$YEAR_OF_BIRTH <- paste(19,Milestone_dataset$YEAR_OF_BIRTH, sep="")
Milestone_dataset$AGE_1STCHILD <- replace(Milestone_dataset$AGE_1STCHILD,
                                          which(Milestone_dataset$AGE_1STCHILD == 'No children yet'),
                                          0) #Replacing No children yet to 0
Milestone_dataset$C1DOB_Y <- replace(Milestone_dataset$C1DOB_Y,
                                     which(Milestone_dataset$C1DOB_Y == 'No children yet'),
                                     0) #Replacing No children yet to 0
Milestone_dataset$COUNTRY_OF_BIRTH <- ifelse(Milestone_dataset$COUNTRY_OF_BIRTH== 'IN THE US',
                                             "United States","Other") #Replacing No children yet to 0
Milestone_dataset$EVER_EDU_LOAN<-ifelse(Milestone_dataset$EVER_EDU_LOAN=="Yes",1,0)
Milestone_dataset$EVER_DIVORCED_<-ifelse(Milestone_dataset$EVER_DIVORCED_=="Yes",1,0)
Milestone_dataset$EVER_UNEMPLOYED_<-ifelse(Milestone_dataset$EVER_UNEMPLOYED_=="Yes",1,0)
Milestone_dataset$EVER_IN_POVERTY <-ifelse(Milestone_dataset$EVER_IN_POVERTY=="Yes",1,0)

#Converting columns into appropriate format
Milestone_dataset$EVER_EDU_LOAN   <- as.numeric(Milestone_dataset$EVER_EDU_LOAN)
Milestone_dataset$EVER_DIVORCED_  <- as.numeric(Milestone_dataset$EVER_DIVORCED_)
Milestone_dataset$EVER_UNEMPLOYED_<- as.numeric(Milestone_dataset$EVER_UNEMPLOYED_)
Milestone_dataset$YEAR_OF_BIRTH   <- as.numeric(Milestone_dataset$YEAR_OF_BIRTH)
Milestone_dataset$AGE_1STCHILD    <- as.numeric(Milestone_dataset$AGE_1STCHILD)
Milestone_dataset$C1DOB_Y         <- as.numeric(Milestone_dataset$C1DOB_Y)
Milestone_dataset$EVER_IN_POVERTY <- as.numeric(Milestone_dataset$EVER_IN_POVERTY)

#Removed the Unused columns
Clean_dataset <- dplyr::select(Milestone_dataset,
                               -YEAR,
                               -ID,
                               -X,
                               -EDU_LOAN_)

summary(Clean_dataset)

str(Clean_dataset)

Milestone_dataset <- Clean_dataset

#Structure of the Dataset

str(Milestone_dataset)

glimpse(Milestone_dataset)

summary(Milestone_dataset)

#Q1: T-test to check difference in Max income of Male and Female

# Filtering the dataset

Male_Dataset <-   subset(Milestone_dataset, Milestone_dataset$SAMPLE_SEX == "MALE" )

Female_Dataset <- subset(Milestone_dataset, Milestone_dataset$SAMPLE_SEX == "FEMALE" )

# inferential statistics 

Male_Dataset %>%
  summarise(Mean = mean(INCOME_MAX),SD   = sd(INCOME_MAX),Num  = length(INCOME_MAX))
write.csv(Male_Dataset, "Male_Dataset1.csv")
Female_Dataset %>%
  summarise(Mean = mean(INCOME_MAX),SD   = sd(INCOME_MAX),Num  = length(INCOME_MAX)) 

#Density plot for Male Income Max
ggdensity(Male_Dataset$INCOME_MAX, fill = "blue", main="Male max income")

#Density plot for Female Income Max
ggdensity(Female_Dataset$INCOME_MAX, fill = "pink", main="Female max income")

# check for Normality

with(Male_Dataset, shapiro.test(INCOME_MAX))

with(Female_Dataset, shapiro.test(INCOME_MAX))

#check the Variance

var.test(INCOME_MAX ~ SAMPLE_SEX, data = Milestone_dataset)

# Perform the t-test

#Hypothesis

# Null Hypothesis H0: Average max income of Male and Female are Same
# Alternative Hypothesis H1:  Average max income of Male and Female are not same

result <- t.test(Male_Dataset$INCOME_MAX,
                 Female_Dataset$INCOME_MAX, 
                 var.equal = FALSE,
                 conf.level = 0.05)

Ttest <- broom::tidy(result)

Ttest

write.csv(Ttest, "T-testMilestone.csv")

#Reject the Null Hypothesis as P-value is less than 0.05



#2 Means hours per week is equal to 50

# inferential statistics 

Milestone_dataset %>%
  summarise(Mean = mean(HOURS_WORKED_PER_WEEK_),SD   = sd(HOURS_WORKED_PER_WEEK_),Num  = length(HOURS_WORKED_PER_WEEK_))

#Visulization

plot_ly(ggplot2::diamonds,
        y=Milestone_dataset$HOURS_WORKED_PER_WEEK_,
        type = "box") %>% 
  layout(title = "Boxplot for Hours worked per Week")


#Hypothesis

# Null Hypothesis H0: Mean Hours 1worked per week is 50
# Alternative Hypothesis H1:  Mean Hours worked per week is not 50

#T-test

result <- t.test(Milestone_dataset$HOURS_WORKED_PER_WEEK_, mu= 50, conf.level = 0.95)

Ttest <- broom::tidy(result)

Ttest

write.csv(Ttest, "T-testMilestone1.csv")

#Reject the Null Hypothesis as P-value is less than 0.05

#3 Net worth of USA citizen is greater than the other citizen or not ?

#Subsetting data based on Birth place

USADataset <- subset(Milestone_dataset, Milestone_dataset$COUNTRY_OF_BIRTH == "United States" )

OtherDataset <- subset(Milestone_dataset, Milestone_dataset$COUNTRY_OF_BIRTH == "Other" )

# inferential statistics 

USADataset %>%
  summarise(Mean = mean(NET_WORTH_),SD   = sd(NET_WORTH_),Num  = length(NET_WORTH_))

OtherDataset %>%
  summarise(Mean = mean(NET_WORTH_),SD   = sd(NET_WORTH_),Num  = length(NET_WORTH_)) 

#Visualization

ggplot(data=Milestone_dataset, aes(x=COUNTRY_OF_BIRTH, y=NET_WORTH_)) +
  geom_bar(stat="identity",fill = "light blue")

#T-test
# Hypothesis
#Null Hypothesis H0: Net worth of USA citizen is same than the other citizen
#ALternative Hypothesis H1: Net worth of USA citizen is greater than the other citizen

result <- t.test(USA_dataset$NET_WORTH_,
                 OtherDataset$NET_WORTH_, 
                 var.equal = FALSE,
                 alternative = "greater")

Milestone3Q <- broom::tidy(result)

Milestone3Q

write.csv(Milestone3Q, "T-Milestone3Q.csv")

#P-Value is less than 0.05 hence greater the difference

# We have  few more questiosn which we are going to Analyze by Regression Analysis

# What is the Max income and Number of jobs relation ? 

lm (INCOME_MAX ~ JOBSNUM_, data = Milestone_dataset)

ggplot(Milestone_dataset, aes(x = JOBSNUM_, y = INCOME_MAX)) +
  geom_point(colour=
               "#CC0000") +
  stat_smooth(method = lm) +ggtitle("Hourse Per Week vs Number of Jobs")

# Regression 
# What is the relation of net worth based on that Family Size ?

lm(NET_WORTH_ ~ FAMSIZE_ , data = Milestone_dataset)

ggplot(Milestone_dataset, aes(x = FAMSIZE_, y = NET_WORTH_)) +
  geom_point(colour=
              "#CC0000") +
  stat_smooth(method = lm) +ggtitle("Hourse Per Week vs Number of Jobs")

# Regression 
# What is the Number of Jobs relationship with Hourse worked per week 

lm (HOURS_WORKED_PER_WEEK_ ~ JOBSNUM_, data = Milestone_dataset)


ggplot(Milestone_dataset, aes(x = JOBSNUM_, y = HOURS_WORKED_PER_WEEK_)) +
  geom_point(colour=
               "#CC0000") +
  stat_smooth(method = lm) +ggtitle("Hourse Per Week vs Number of Jobs")

