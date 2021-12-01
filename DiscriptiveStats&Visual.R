getwd()
setwd("C:/Sagar Drive/")
# Installed and Loaded all the libraries

#install.packages("dplyr")
#install.packages("plyr")
#install.packages("tidyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("rmarkdown")
#install.packages("gmodels")
#install.packages("imager")
#install.packages("png") 
library(tidyverse)
library(gapminder)
library(plotly)
library(gmodels)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rmarkdown)
library(data.table)
library(png)
library(grid)
library(imager)
library(kableExtra)
library(patchwork)   
library(png) 

#Import the dataset and store it into Baseline_survey
Baseline_survey <- as.data.frame(read.csv(
  file = "New_baseline_survey.csv"))

kbl(Baseline_survey, caption = "Baseline Survey Dataset") %>%
  kable_material_dark(c("striped", "hover"))%>%
  kable_classic_2(full_width = F,position = "left")   %>%
  row_spec(0, bold = T, color = "white", background = "black") %>%
  scroll_box(width = "100%",height = "500px")

summary(Baseline_survey)

# created psych::describe() by using Baseline_survey entire dataset
describe = psych::describe(Baseline_survey)

kbl(describe, caption = " Descriptive statistics table for Baseline Survey") %>%
  kable_material_dark(c("striped", "hover","responsive"))%>%
  kable_classic_2(full_width = F,position = "left")  %>%
  row_spec(0, bold = T, color = "white", background = "black") %>%
  scroll_box(width = "100%",height = "500px")

 

class_data <- psych::describeBy(Baseline_survey, Baseline_survey$Class)
class_data

kbl(class_data$`1`, caption = "Descriptive statistics table for Class 1") %>%
  kable_material_dark(c("striped", "hover","responsive"))%>%
  kable_classic_2(full_width = F,position = "left")  %>%
  row_spec(0, bold = T, color = "white", background = "black") %>%
  scroll_box(width = "100%",height = "500px")



kbl(class_data$`2`, caption = "Descriptive statistics table for Class 2") %>%
  kable_material_dark(c("striped", "hover","responsive"))%>%
  kable_classic_2(full_width = F,position = "left") %>%
  row_spec(0, bold = T, color = "white", background = "black") %>%
  scroll_box(width = "100%",height = "500px")


kbl(class_data$`3`, caption = "Descriptive statistics table for Class 3") %>%
  kable_material_dark(c("striped", "hover","responsive"))%>%
  kable_classic_2(full_width = F,position = "left") %>%
  row_spec(0, bold = T, color = "white", background = "black") %>%
  scroll_box(width = "100%",height = "500px")


kbl(class_data$`4`, caption = "Descriptive statistics table for Class 4") %>%
  kable_material_dark(c("striped", "hover","responsive"))%>%
  kable_classic_2(full_width = F,position = "left") %>%
  row_spec(0, bold = T, color = "white", background = "black") %>%
  scroll_box(width = "100%", height = "500px")



kbl(class_data$`5`, 
  caption = "Descriptive statistics table for Class 5") %>%
  kable_material_dark(c("striped", "hover","responsive"))%>%
  kable_classic_2(full_width = F,position = "left")  %>%
  row_spec(0, bold = T, color = "white", background = "black") %>%
  scroll_box(width = "100%", height = "500px")

 
kbl(class_data$`6`, caption = "Descriptive statistics table for Class 6") %>%
  kable_material_dark(c("striped", "hover","responsive"))%>%
  kable_classic_2(full_width = F,position = "left") %>%
  row_spec(0, bold = T, color = "white", background = "black") %>%
  scroll_box(width = "100%",height = "500px")



kbl(describe, 
    caption = "Three Line table format of Dataset") %>%
  kable_minimal()%>%
  kable_classic(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = T, 
                font_size = 20, 
                position = "center")%>%
  add_header_above(c("Statistical data for Dataset" = 14))%>%
  scroll_box(width = "100%",height = "500px")

par(mfrow=c(1,1))
boxplot(Baseline_survey$Work_Experience,
        Baseline_survey$Statistics_or_Analytics_Experience,
        names = c("Work Experience","Statistics or Analytics Experience"), 
        xlab="No. of Years",
        main="Figure 1: Boxplot of Work and Academic Experience")

par(mfrow=c(1,2))
boxplot(Baseline_survey$Age ~ Baseline_survey$Class,
        xlab="Age",
        main="Figure 2: Boxplot of Age in class")
boxplot(Baseline_survey$Class ~ Baseline_survey$Major,
        las=2,
        xlab = "",
        main="Figure 3: Boxplot of Class per Major")

img <- readPNG("C:/Sagar Drive/ALY6010/ALY6010_R/Week 1/c.png")
imageLogo <- readPNG("C:/Sagar Drive/ALY6010/ALY6010_R/Week 1/s.png")
images<- c(imageLogo,imageLogo)
ggplot(Baseline_survey, aes(x = Term, y = Class)) +
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
geom_point( shape=8, color="white")  +
labs(title= "Figure 4: Scatterplot for Class as per Term")


reg<-lm(Class ~ Term, data = Baseline_survey) 
coeff = coefficients(reg)

scatplot <- ggplot(Baseline_survey,
              aes(x = Term, y = Class)) +
  geom_point( shape=10, color="blue") + 
  labs(title= "Figure 5: Scatterplot of Class per Term")

jitplot <- ggplot(Baseline_survey,
              aes(Term, Class)) +
  geom_jitter(position=position_jitter(0.1),shape = 10,color="blue") +
  geom_abline(intercept = reg$coefficients) +
  labs(title= "Figure 6: JitterPlot of Class per Term")

multiple <- ggarrange(scatplot,
                   jitplot, 
                   nrow = 1,
                   ncol = 2)

multiple


