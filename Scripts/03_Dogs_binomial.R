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

# 3 Model 3: Binomial GLM
# We constructed a binomial generalized linear model to estimate the likelihood of dog sex
# (coded as 1 for male and 2 for female) based on the predictor DogAgeCoded. The goal was to identify
# if there is a significant relationship between dog age and the likelihood of being male or female.

# Create a boxplot to visualize the distribution of dog age by sex
ggplot(df.dogs, aes(x = DogSexText, y = DogAgeCoded)) +
  geom_boxplot() +
  labs(title = "Distribution of Dog Age by Sex",
       x = "Sex",
       y = "Dog Age")


# binomials work on binary data only, so sex must be 0 or 1. Change from 1 and 2.
df.dogs$DogSexCoded[df.dogs$DogSexCoded == 1] <- 0
df.dogs$DogSexCoded[df.dogs$DogSexCoded == 2] <- 1

# We fit the model
glm_dog_sex_age <- glm(DogSexCoded ~ DogAgeCoded, family = binomial, data = df.dogs)
summary(glm_dog_sex_age)
exp(coef(glm_dog_sex_age))

"""
The coefficient for DogAgeCoded is 0.0035 with a p-value of 0.0104, indicating statistical significance.
For every year increase in dog age, the probability of being male increase by 0.35%, holding other variables constant.
While the statistical significance of the coefficient for a dog's age suggests that there is evidence to support the relationship between dog age and the probability of being male,
the practical significance of a 0.35% increase in odds may not be substantial enough to warrant immediate business decisions based solely on this finding.

Further analysis may be needed here. Although the hypothesis was to offer products tailored to the dog's age and gender,
and While age appears to influence sex likelihood, additional factors such as breed, size, and general health complaints would provide a more sound business decision making.
So more investigation into factors would refine predictions.
"""
# DEBUG, MORE LEVELS THAN REFERENCE
# Model accuracy
predicted_values <- predict(glm_dog_sex_age, type = "response")
threshold <- 0.5
binary_predictions <- ifelse(predicted_values > threshold, 1, 0)

# Create confusion matrix
conf_matrix <- confusionMatrix(as.factor(binary_predictions), as.factor(df.dogs$SexCoded))

# Display confusion matrix and statistics
print(conf_matrix)



"""
Identifying Popular Breeds:

    The model can accurately predict the popularity of different dog breeds based on age and sex.
Pet Paradise can use the model predictions to optimize inventory management by stocking up on products that are likely to be in higher demand based on the popularity of different breeds.
For example, if certain breeds are more popular among senior dog owners, Pet Paradise can introduce specialized products such as senior dog food or joint supplements targeted towards that demographic.
"""

# Select my variables. Updated with Owner Age.
df <- df.dogs[, c("Breed1Text", "DogAgeCoded", "DogSexCoded")]

# clean NAs
missing_values <- colSums(is.na(df))
print(missing_values) # no missing values :)

summary(df$DogAgeCoded)
# output shows max value 999 -> nonsensical. It refers to meaning "age unknown". So we replace it with the average age.
mean_dog_age <- mean(df$DogAgeCoded[df$DogAgeCoded != 999], na.rm = TRUE)
df <- df %>%
  mutate(DogAgeCoded = ifelse(DogAgeCoded == 999, mean_dog_age, DogAgeCoded))
summary(df$DogAgeCoded) # now output makes much more sense.
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#0.000   2.000   5.000   5.887   9.000  23.000

# Distributions
table(df$DogSexCoded)
# The distribution of dog sexes shows there are 35,409 female dogs (coded 0) and 35,558 male dogs (coded 1).
# There's no significant skew towards one sex in the dataset.

# Favourite 5 breeds
breed_counts <- table(df$Breed1Text)
sorted_breeds <- sort(breed_counts, decreasing = TRUE)
top_5_breeds <- head(sorted_breeds, 5)
top_5_breeds

"""
Top breeds:
          Unbekannt           Chihuahua  Labrador Retriever   Yorkshire Terrier Jack Russel Terrier
               9095                4828                4198                2709                2579
So Pet Paradise must offer mixed breed foods and products. Not only purebred product offerings.
"""

# Fit the binomial logistic regression model
binomial_model <- glm(DogSexCoded ~ DogAgeCoded, family = binomial, data = df.dogs)
summary(binomial_model)
# odds ratios
exp_coef <- exp(coef(binomial_model))
print(exp_coef)
percentage_change <- (exp_coef - 1) * 100

# Interpretation of the coefficients
print("Interpretation of Coefficients:")
print(paste("For every year increase in dog age, the odds of being female increase by",
            round(percentage_change["DogAgeCoded"], 2), "%"))

"""
Older dogs are more likely to be female, if all other variables are equal. Pet Paradise could tailor it's pet care,
such as tailoring products or services to the specific needs of aging female dogs. But, the increase is negligible, at 0.35%.
"""

"""
So instead, another sales approach. Let's say, Pet Paradise is trying to target specific age groups for marketing or sales purposes.
Pet Paradise wants to predict the likelihood of a pet owner in their 40s owning a top 5 breed (e.g., the Chihuahua)
compared to owning an unknown breed. For this, a binomial logistic regression makes most sense to use.
This is because the response variable is binary: either a pet owner owns a Chihuahua (let say, coded as 1)
or they own an unknown breed (we will code this as 0).
"""

# I first subset the dataset. This is to include owners in their 40s and the 2 analysed breeds.
subset_df <- df[df$AgeV10Coded == 40 & (df$Breed1Text == "Chihuahua" | df$Breed1Text == "Unbekannt"), ]
names(df) # Debugging; Age10Coded wasn't in dataframe.
df <- df.dogs[, c("Breed1Text", "DogAgeCoded", "DogSexCoded", "AgeV10Coded", "AgeV10Text")]
names(df) # Debugging; Age10Coded wasn't in dataframe.
head(df)

# Create a binary variable for Chihuahua ownership
subset_df$ChihuahuaOwned <- ifelse(subset_df$Breed1Text == "Chihuahua", 1, 0)

# Binomial logistic regression
binomial_model <- glm(ChihuahuaOwned ~ AgeV10Coded, family = binomial, data = subset_df)

# Summary of the model
summary(binomial_model)

# Predict the probability of owning a Chihuahua vs. owning an unknown breed for individuals in their 40s
age_40 <- data.frame(AgeV10Coded = 40)
predicted_prob <- predict(binomial_model, newdata = age_40, type = "response")

# Print the predicted probability
print(paste("Probability of owning a Chihuahua for individuals in their 40s:", predicted_prob, "%"))







# Evaluate Model Performance
# Predict breed probabilities
predicted_probabilities <- predict(binomial_model, type = "response")

# Convert probabilities to predicted classes (0 or 1)
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(predicted_classes == df$Breed1Text)

# Calculate precision
precision <- sum(predicted_classes == 1 & df$Breed1Text == 1) / sum(predicted_classes == 1)

# Calculate recall
recall <- sum(predicted_classes == 1 & df$Breed1Text == 1) / sum(df$Breed1Text == 1)

# Calculate F1-score
f1_score <- 2 * precision * recall / (precision + recall)

# Print performance metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1_score, "\n")

# Plot ROC curve
library(pROC)
roc_curve <- roc(df$Breed1Text, predicted_probabilities)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
auc <- auc(roc_curve)
cat("AUC:", auc, "\n")

# Assess Predictor Significance
summary(binomial_model)
# Evaluate Model Performance
# Predict breed probabilities
predicted_probabilities <- predict(binomial_model, type = "response")

# Convert probabilities to predicted classes (0 or 1)
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(predicted_classes == df$Breed1Text)

# Calculate precision
precision <- sum(predicted_classes == 1 & df$Breed1Text == 1) / sum(predicted_classes == 1)

# Calculate recall
recall <- sum(predicted_classes == 1 & df$Breed1Text == 1) / sum(df$Breed1Text == 1)

# Calculate F1-score
f1_score <- 2 * precision * recall / (precision + recall)

# Print performance metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1_score, "\n")

# Plot ROC curve
library(pROC)
roc_curve <- roc(df$Breed1Text, predicted_probabilities)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
auc <- auc(roc_curve)
cat("AUC:", auc, "\n")

# Assess Predictor Significance
summary(binomial_model)



