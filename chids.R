getwd()
setwd("~/Desktop/CHIDS")

# Libraries ####
library(readxl)
#
# Data Import #### 
Dating_Apps_Respondent_Data_CORE <- read_excel("Dating Apps_Respondent_Data_CORE.xlsx")
Dont_be_Fooled_Respondent_Data_CORE <- read_excel("Dont be Fooled_Respondent_Data_CORE.xlsx")
Fealessness_Respondent_Data_CORE <- read_excel("Fealessness_Respondent_Data_CORE.xlsx")
Read_my_lips_Respondent_Data_CORE <- read_excel("Read my lips_Respondent_Data_CORE.xlsx")
She_is_Valuable_Respondent_Data_CORE <- read_excel("She is Valuable_Respondent_Data_CORE.xlsx")
#
# Summary ####
summary(Dating_Apps_Respondent_Data_CORE)

