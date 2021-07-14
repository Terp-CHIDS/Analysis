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
pca.data <- read.csv("data_pca_clean.csv", header = T, sep = ',')
#
# summary ####
view(Merged)
str(Merged)
summary(Merged)
# cleaning the dataset ####



# PCA ####
pca.data.sc <- pca.data
pca.data.sc [,] <- data.frame(scale(pca.data[,]))
#aggregate mean ratings by brand
pca.data.mean <- aggregate(.~ , data = pca.data.sc, mean)
#housekeeping -- 
rownames(pca.data.mean) <- pca.data.mean[, 1]
pca.data.mean <- pca.data.mean[, ]
#visualizing pca
pca.data.pc <- prcomp(pca.data.sc[,])
#scree or elbow plot
plot(pca.data.pc, type= "l")
#mean data Q2 
pca.data.mean.pc <- prcomp(pca.data.mean, scale = TRUE)
biplot(pca.data.mean.pc, main ="pca")
biplot(pca.data.mean.pc, choices = 2:3)
