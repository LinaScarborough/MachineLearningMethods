library(dplyr) # Data manipulation, cleaning.
# library(e1071)  # library for SVMs
library(ggplot2) # Data visualization, plotting
library(MASS) # Statistical methods, data analysis.
library(caret) # Machine learning, model training.

# Caution! You might need to change the path to open the csv on your device.
df.dogs <- read.csv("Datasets/kul100od1001.csv")
head(df.dogs)

df.dogs <- df.dogs %>%
  mutate(DogSize = case_when(
    RassentypLang == "Kleinwüchsig" ~ "small",
    RassentypLang == "Rassentypenliste I" ~ "large",
    RassentypLang == "Rassentypenliste II" ~ "banned",
    TRUE ~ NA_character_
  ))

write.csv(df.dogs, "Dog population Zurich_EN.csv", row.names = FALSE)

# Define a translation dictionary for column names
translation_dict <- c("StichtagDatJahr" = "ReferenceYear",
                      "DatenstandCd" = "DataStatusCoded",
                      "HalterId" = "OwnerId",
                      "AlterV10Cd" = "AgeV10Coded",
                      "AlterV10Lang" = "AgeV10Text",
                      "AlterV10Sort" = "AgeV10Sort",
                      "SexCd" = "OwnerSexCoded",
                      "SexLang" = "OwnerSexText",
                      "SexSort" = "SexSort",
                      "KreisCd" = "DistrictCoded",
                      "KreisLang" = "DistrictText",
                      "KreisSort" = "DistrictSort",
                      "QuarCd" = "QuarterCoded",
                      "QuarLang" = "QuarterText",
                      "QuarSort" = "QuarterSort",
                      "Rasse1Text" = "Breed1Text",
                      "Rasse2Text" = "Breed2Text",
                      "RasseMischlingCd" = "MixedBreedCoded",
                      "RasseMischlingLang" = "MixedBreedText",
                      "RasseMischlingSort" = "MixedBreedSort",
                      "RassentypCd" = "BreedTypeCode",
                      "RassentypLang" = "BreedTypeLong",
                      "RassentypSort" = "BreedTypeSort",
                      "GebDatHundJahr" = "DogBirthYear",
                      "AlterVHundCd" = "DogAgeCoded",
                      "AlterVHundLang" = "DogAgeText",
                      "AlterVHundSort" = "DogAgeSort",
                      "SexHundCd" = "DogSexCoded",
                      "SexHundLang" = "DogSexText",
                      "SexHundSort" = "DogSexSort",
                      "HundefarbeText" = "DogColorText",
                      "AnzHunde" = "NumberOfDogs")


# Rename columns using translation dictionary
names(df.dogs) <- sapply(names(df.dogs), function(x) {
  if (x %in% names(translation_dict)) {
    return(translation_dict[x])
  } else {
    return(x)
  }
})

# Write the modified data frame
write.csv(df.dogs, "Dog population Zurich_EN.csv", row.names = FALSE)
head(df.dogs)
colnames(df.dogs)
#########################################################################

### SVM model – started 01.05 by LS. Handed to RG on 26.05
#############################################################

### LS added SVM
# Which districts are more likely to have which dog breeds?
#This a classification problem, predicting the dog breed based on district.
#SVMs are suitable for this. We train an SVM classifier with features like DistrictText, OwnerSexText, DogAgeText,  ReferenceYear.
#After training, we interpret the model for features which are most influential in predicting dog breeds for each district.
"""
library(e1071)  # library for SVMs

# Selecting data
data <- read.csv("kul100od1001_EN_avg.csv")
selected_data <- data[, c("DistrictText", "OwnerSexText", "DogAgeText", "ReferenceYear", "Breed1Text")]

# Encoding categorical variables
selected_data$DistrictText <- as.factor(selected_data$DistrictText)
selected_data$OwnerSexText <- as.factor(selected_data$OwnerSexText)
selected_data$DogAgeText <- as.factor(selected_data$DogAgeText)
selected_data$Breed1Text <- as.factor(selected_data$Breed1Text)

# training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(selected_data), 0.8*nrow(selected_data))
train_data <- selected_data[train_index, ]
test_data <- selected_data[-train_index, ]

# to be inserted: Exploratory Data Analysis (EDA)

# Modeling, training the SVM classifier
svm_model <- svm(Breed1Text ~ ., data = train_data, kernel = "radial")

# Interpretation
# Get the most influential features
summary(svm_model)
"""

