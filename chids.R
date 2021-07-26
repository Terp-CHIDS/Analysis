setwd("/Users/akanksha/Documents/GitHub/Analysis")

# Libraries ####
library(readxl)
library(utils)
library(tibble)
library(ellipsis)
install.packages("factoextra")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(factoextra)

# Data Import #### 
Dating_Apps_Respondent_Data_CORE <- read_excel("Dating Apps_Respondent_Data_CORE.xlsx")
Dont_be_Fooled_Respondent_Data_CORE <- read_excel("Dont be Fooled_Respondent_Data_CORE.xlsx")
Fealessness_Respondent_Data_CORE <- read_excel("Fealessness_Respondent_Data_CORE.xlsx")
Read_my_lips_Respondent_Data_CORE <- read_excel("Read my lips_Respondent_Data_CORE.xlsx")
She_is_Valuable_Respondent_Data_CORE <- read_excel("She is Valuable_Respondent_Data_CORE.xlsx")
merged <- read.csv("MergedDataset.csv", header = T, sep = ',')
#
# summary ####
view(merged)
str(merged)
summary(merged)
view(pca)
str(pca)
summary(pca)
# cleaning the dataset ####
merged <- subset(merged, select = -c(1))
merged <- subset(merged, select = -c(4,5))
pca <- subset(merged, select = -c(4,14,15))
pca <- subset(pca, select = -c(Mood_tone))
# convert: 1 ON WHAT THE AD IS TRYING TO ENCOURAGE --> 1
#pca[is.na(pca)] = 0
pca <- replace(pca, is.na(pca), 0)

# PCA on the entire dataset ####
pca.sc <- pca
pca.sc [,3:87] <- data.frame(scale(pca[,3:87]))
pca.pc <- prcomp(pca.sc[,3:87], scale = T, center = T)
summary(pca.pc)
fviz_eig(pca.pc)
#visualizing PCA
plot(pca.pc, type = "l")
# Graph of individuals. Individuals with a similar profile are grouped together. 

fviz_pca_ind(pca.pc,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T    #  Avoid text overlapping
)

# Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.

fviz_pca_var(pca.pc,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Biplot of individuals and variables

fviz_pca_biplot(pca.pc, 
                repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

#Access to the PCA results

# Eigenvalues
eig.val <- get_eigenvalue(pca.pc)
eig.val

# Results for Variables
res.var <- get_pca_var(pca.pc)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(pca.pc)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

# PCA on predictor variables only ####

# independent variables : 
# Age | ACTIVEINVOLVEMENT_1 | ACTIVEINVOLVEMENT_2 | ACTIVEINVOLVEMENT_3 | AD_DISTINCTIVENESS | UNDERSTANDING 
# RELEVANCE | CREDIBILITY | WATCHSKIP | STOPLOOK | NEWINFORMATION | 
# Motherhood..:.Interest | Parenting..:.Interest | Reading..:.Interest | Word.Games..:.Interest | 
# Charities.And.Causes..:.Interest | Vacations..:.Interest | Live.Events..:.Interest | Adventure.Travel..:.Interest
# Climate.Change..:.ISSUE | Drug.Usage.Amongst.Young.People..:.ISSUE | Having.A.Healthier.Lifestyle..:.ISSUE | 
# Making.Your.Neighbourhood.A.Safer.Place..:.ISSUE | Reducing.Road.Accidents..:.ISSUE | The.Amount.Of.Alcohol.Consumed.By.People..:.ISSUE
# The.Effects.Of.Smoking.And.Passive.Smoking..:.ISSUE | Crime.Prevention..:.ISSUE | Unemployment.Or.Job.Security..:.ISSUE |
# Gender-based.Violence..:.ISSUE | HIV.Prevention..:.ISSUE

pca.predictor.var <- c("Ads", "Age", "ACTIVEINVOLVEMENT_1", "ACTIVEINVOLVEMENT_2", "ACTIVEINVOLVEMENT_3", "AD_DISTINCTIVENESS", "UNDERSTANDING",
                                        "RELEVANCE", "CREDIBILITY", "WATCHSKIP", "STOPLOOK", "NEWINFORMATION", "Motherhood....Interest" ,
                                        "Parenting....Interest", "Reading....Interest", "Word.Games....Interest", 
                                        "Charities.And.Causes....Interest", "Vacations....Interest", "Live.Events....Interest",
                                        "Adventure.Travel....Interest", "Climate.Change....ISSUE", 
                                        "Drug.Usage.Amongst.Young.People....ISSUE", "Having.A.Healthier.Lifestyle....ISSUE", 
                                        "Making.Your.Neighbourhood.A.Safer.Place....ISSUE",
                                        "Reducing.Road.Accidents....ISSUE", "The.Amount.Of.Alcohol.Consumed.By.People....ISSUE", 
                                        "The.Effects.Of.Smoking.And.Passive.Smoking....ISSUE", "Crime.Prevention....ISSUE",
                                        "Unemployment.Or.Job.Security....ISSUE", "Gender.based.Violence....ISSUE", "HIV.Prevention....ISSUE")
pca.predictor <- pca[pca.predictor.var]
pca.predictor [,1:31] <- data.frame(scale(pca.predictor[,1:31]))
pca.predictor <- prcomp(pca.predictor[,1:31], scale = T, center = T)
summary(pca.predictor)
fviz_eig(pca.predictor)

fviz_pca_ind(pca.predictor,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T    #  Avoid text overlapping
)

# Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.

fviz_pca_var(pca.predictor,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Biplot of individuals and variables

fviz_pca_biplot(pca.predictor, 
                repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

#Access to the PCA results

# Eigenvalues
eig.val <- get_eigenvalue(pca.predictor)
eig.val

# Results for Variables
res.var <- get_pca_var(pca.predictor)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(pca.predictor)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

# PCA on Indicator variables only ####

# dependent variables :
# Branding | HIV Prevention Persuasion 2
