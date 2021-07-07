setwd("/Users/akanksha/Documents/GitHub/Analysis")

# Libraries ####
library(readxl)
library(utils)
library(tibble)
#
# Data Import #### 
Dating_Apps_Respondent_Data_CORE <- read_excel("Dating Apps_Respondent_Data_CORE.xlsx")
Dont_be_Fooled_Respondent_Data_CORE <- read_excel("Dont be Fooled_Respondent_Data_CORE.xlsx")
Fealessness_Respondent_Data_CORE <- read_excel("Fealessness_Respondent_Data_CORE.xlsx")
Read_my_lips_Respondent_Data_CORE <- read_excel("Read my lips_Respondent_Data_CORE.xlsx")
She_is_Valuable_Respondent_Data_CORE <- read_excel("She is Valuable_Respondent_Data_CORE.xlsx")
Merged <- read_excel("Merged.xlsx")
#
# summary ####
view(Merged)
str(Merged)
summary(Merged)
# cleaning the dataset ####


