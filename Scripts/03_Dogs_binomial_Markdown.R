library(dplyr) # Data manipulation, cleaning.
# library(e1071)  # library for SVMs
library(plotly)
library(gplots) # for fancy confusion matrices
library(ggplot2) # Data visualization, plotting
library(reshape2)
library(MASS) # Statistical methods, data analysis.
library(caret) # Machine learning, model training.

# Caution! You might need to change the path to open the csv on your device.
df_EN <- read.csv("Datasets/df_EN.csv")
head(df_EN)
colnames(df_EN)
#########################################################################
"""
Disclaimer:

The analysis in this report is conducted purely for educational purposes, focusing solely on the statistical modeling and client recomendations. This means that within the confines of this project, sex values for an animal such as dogs within the dataset are interpretet as binary. The findings presented here are not intended to reflect or endorse any particular social reflections related to gender identity.
We acknowledge that discussions surrounding gender identity are multifaceted for individuals and communities.
"""

# 3 Model 3: Binomial GLM
# We constructed a binomial generalized linear model to estimate the likelihood of dog sex
# (coded as 1 for male and 2 for female) based on the predictor DogAgeGroupCd. The goal was to identify
# if there is a significant relationship between dog age and the likelihood of being male or female.

# binomials work on binary data only, so dog sex for this dataset must be 0 or 1. Ie Change from 1 and 2.
df_EN$DogSexCd[df_EN$DogSexCd == 1] <- 0
df_EN$DogSexCd[df_EN$DogSexCd == 2] <- 1

# We fit the model
glm_dog_sex_age <- glm(DogSexCd ~ DogAgeGroupCd, family = binomial, data = df_EN)
summary(glm_dog_sex_age)
exp_coef <- exp(coef(glm_dog_sex_age))
# odds ratios
print(exp_coef)
percentage_change <- (exp_coef - 1) * 100

# Interpretation of the coefficients
print("Interpretation of Coefficients:")
print(paste("For every year increase in dog age, the odds of being female increase by",
            round(percentage_change["DogAgeGroupCd"], 2), "%"))

"""
The coefficient for DogAgeGroupCd is 0.0035 with a p-value of 0.0104, indicating statistical significance.
For every year increase in dog age, the probability of being female increase by 0.35%, holding other variables constant.
While the statistical significance of the coefficient for a dog's age suggests that there is evidence to support the relationship between dog age and the probability of being female,
the practical significance of a 0.35% increase in odds may not be substantial enough to warrant immediate business decisions based solely on this finding.

Further analysis may be needed here. Although the hypothesis was to offer products tailored to the dog's age and gender,
and while age appears to influence sex likelihood, additional factors such as breed and size would provide a more sound business decision making.
So more investigation into factors would refine predictions. We look at dog breeds.
"""
# Select my variables. Updated with Owner Age.
df <- df_EN[, c("PrimaryBreed", "DogAgeGroupCd", "DogSexCd", "OwnerAgeGroupCd")]

# clean NAs
missing_values <- colSums(is.na(df))
print(missing_values) # no missing values

summary(df$DogAgeGroupCd)
# output shows max value 999 -> nonsensical. It refers to meaning "age unknown". So we replace it with the average age.
mean_dog_age <- mean(df$DogAgeGroupCd[df$DogAgeGroupCd != 999], na.rm = TRUE)
df <- df %>%
  mutate(DogAgeGroupCd = ifelse(DogAgeGroupCd == 999, mean_dog_age, DogAgeGroupCd))
summary(df$DogAgeGroupCd) # now output makes much more sense.
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#0.000   2.000   5.000   5.887   9.000  23.000

# Distributions
table(df$DogSexCd)
# The distribution of dog sexes shows there are 35,409 female dogs (coded 0) and 35,558 male dogs (coded 1).
# There's no significant skew towards one sex in the dataset.

# Favourite 5 breeds
breed_counts <- table(df$PrimaryBreed)
sorted_breeds <- sort(breed_counts, decreasing = TRUE)
top_5_breeds <- head(sorted_breeds, 5)
top_5_breeds

"""
Top breeds:
          Unbekannt           Chihuahua  Labrador Retriever   Yorkshire Terrier Jack Russel Terrier
               9095                4828                4198                2709                2579
The most popular dog is a unknown ie. mixed dog breed. So Pet Paradise must offer mixed breed foods and products. Not only purebred product offerings.

So instead, another sales approach. Let's say, Pet Paradise is trying to target specific dog or owner age groups for marketing or sales purposes.
Pet Paradise wants to predict the likelihood of a pet owner in their 40s owning a top 5 breed (e.g., the Chihuahua)
compared to owning an unknown breed. For this, a binomial logistic regression makes most sense to use.
This is because the response variable is binary: either a pet owner owns a Chihuahua (let say, coded as 1)
or they own an unknown breed (we will code this as 0).

In terms of identifying popular breeds based on age and sex, Pet Paradise wants to use a model's predictions to optimize inventory management by stocking up on products that are likely to be in higher demand based on the popularity of a given pedigree breed.
"""


# Now that the top 5 breeds are identified, a new subset should be created for owners in their 40s and for the top 5 breeds
owner_40s_df <- df[df$OwnerAgeGroupCd == 40 & df$PrimaryBreed %in% names(top_5_breeds), ]

# Create a binary variable for Chihuahua ownership
owner_40s_df$ChihuahuaOwned <- ifelse(owner_40s_df$PrimaryBreed == "Chihuahua", 1, 0)

# Check if there are any 0s in the ChihuahuaOwned column
head(owner_40s_df)
any(owner_40s_df$ChihuahuaOwned == 0)  # Yes, we get TRUE because there are other breeds


# Check if there are any 0s in the ChihuahuaOwned column
any(owner_40s_df$ChihuahuaOwned == 0)  # Should be TRUE if there are other breeds

# Fit binomial logistic regression model
chihuahua_binomial <- glm(ChihuahuaOwned ~ 1, family = binomial, data = owner_40s_df)
summary(chihuahua_binomial)

# Calculate odds ratio
chihuahua_exp_coef <- exp(coef(chihuahua_binomial))
print(chihuahua_exp_coef)
chihuahua_percentage_change <- (chihuahua_exp_coef - 1) * 100
print(chihuahua_percentage_change)

"""
We see that the log-odds of a 40-year-old owning a Chihuahua are -1.29, which is statistically significant.
The exponentiated coefficient is 0.27, indicating that the odds of a 40-year-old owning a Chihuahua are 27%.
If Pet Paradise targets typically well-earning professionals, i.e., adults in their 40s, and since the likelihood
of a dog owner in their 40s owning a Chihuahua is relatively low, Pet Paradise diversify marketing efforts away
from pedigree focus and rather highlight a broader range of mixed-breed foods, fur-shampoos and other products.
This strategy will help attract well-earning customers who may own mixed or different breeds.
"""

"""
To check a last potential business case, we can check the reverse; predict which owner age group is more likely to own a Chihuahua,
Because we have a binary variable (ChihuahuaOwned: 1 for ownership and 0 for no ownership) and the remaining predictor variables (PrimaryBreed, DogAgeGroupCd, DogSexCd, and OwnerAgeGroupCd).
We will try o predict which owner age group is more likely to own a Chihuahua.
"""

chihuahua_age_bracket <- glm(formula = ChihuahuaOwned ~ PrimaryBreed + DogAgeGroupCd + DogSexCd + OwnerAgeGroupCd, family = binomial, data = owner_40s_df)
summary(chihuahua_age_bracket)

# Collinearity because model didn't converge. We calculate variance inflation factors
vif_values <- car::vif(chihuahua_age_bracket)
print(vif_values)


"""
The VIF shows that  the model has perfect multicollinearity. It creates in linear dependencies among predictor variables. What this means, is that when there are categorical variables with many levels, it leads to unreliable predictions.
So, if Pet Paradise is trying to predict which customers want to buy a product based on their age braket, breed of dog, etc, such a model with these singularities might suggest targeting certain groups of customers when, in fact, the data is too ambiguous to make such recommendations confidently
We will compare the first two of the three models to determine the better fit instead.
"""

### Comaparing models

# AIC and BIC for Model 1: Dog Sex vs. Age
aic_model1 <- AIC(glm_dog_sex_age)
bic_model1 <- BIC(glm_dog_sex_age)

# AIC and BIC for Model 2: Chihuahua Ownership in 40s
aic_model2 <- AIC(chihuahua_binomial)
bic_model2 <- BIC(chihuahua_binomial)

# Print AIC and BIC values for both models
cat("Model 1 - Dog Sex vs. Age:\n")
cat("AIC:", aic_model1, "\n")
cat("BIC:", bic_model1, "\n\n")

cat("Model 2 - Chihuahua Ownership in 40s:\n")
cat("AIC:", aic_model2, "\n")
cat("BIC:", bic_model2, "\n")

"""
We see from the Akaike Information Criterion and Bayesian Information Criterion that the second model, i.e. Chihuahua Ownership in an owner's 40s, has much lower AIC and BIC values than the Dog Sex vs. Age model. So model 2 provides a better fit to the data.
Because the models contain fewer than 2 terms, checking collinearity doesn't make sense. We check the confusion matrices instead.
"""
# Confusion matrix for Dog Sex vs. Age
predicted_probabilities <- predict(glm_dog_sex_age, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
confusion_glm1 <- table(df_EN$DogSexCd, predicted_classes)
confusion_glm1

# Plot confusion matrix as heatmap
heatmap.2(confusion_glm1,
          main = "Confusion Matrix",
          col = c("lightblue", "green", "blue"),
          density.info = "none",
          trace = "none",
          key = TRUE,
          keysize = 1.5,
          margins = c(5, 5),
          cexRow = 1,
          cexCol = 1,
          labRow = c("Actual Female", "Actual Male"),
          labCol = c("Predicted Female", "Predicted Male"))

# 1. Confusion matrix values
TN <- 15813
FP <- 19596
FN <- 14672
TP <- 20886

# accuracy and precision
accuracy <- ((TP + TN) / (TP + TN + FP + FN)) * 100
precision <- (TP / (TP + FP)) * 100

# sensitivity
recall <- (TP / (TP + FN)) * 100

"""
The first confusion matrix shows that there are correctly predicted the negatives was 15,813 instances.
The false positive representing incorrectly predicted the positive cases was 19,596 false positives.
There were 14,672 false negative predictions and 20,886 true positives.

This means the first binomial model has an accuracy of 52% and a precision of 52%.
The Sensitivity was around 59%, where of all actual positive instances were correctly identified by the model.
"""
