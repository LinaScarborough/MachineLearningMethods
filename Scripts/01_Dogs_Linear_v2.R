library(dplyr) # Data manipulation, cleaning.
# library(e1071)  # library for SVMs
library(ggplot2) # Data visualization, plotting
library(MASS) # Statistical methods, data analysis.
library(caret) # Machine learning, model training.
# library(forecast) # needs debugging. Search cntrl+F

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

# 1.1. Model 1: Linear Model

# Encode DogSize variable to numerical values
df.dogs$DogSize <- factor(df.dogs$DogSize, levels = c("small", "large", "banned"), ordered = TRUE)
df.dogs$DogSize <- as.numeric(df.dogs$DogSize)

# Check for missing values in DistrictCode and DogSize columns
missing_district <- sum(is.na(df.dogs$DistrictCode))
missing_dogsize <- sum(is.na(df.dogs$DogSize))

# Fit linear regression model and provide summary
lm_model <- lm(DogSize ~ DistrictCoded, data = df.dogs)
summary(lm_model)

"""
1.2 Linear Model Interpretation
The linear regression model summary shows the relationship between dog sizes and the district codes.
The coefficient for DistrictCoded is significant, indicating a relationship between the district
and the size of the dogs.

However, the low R-squared value suggests that the model explains very little of the variance in dog sizes.
"""
