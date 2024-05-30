library(dplyr) # Data manipulation, cleaning.
# library(e1071)  # library for SVMs
library(ggplot2) # Data visualization, plotting
library(MASS) # Statistical methods, data analysis.
library(caret) # Machine learning, model training.
# library(forecast) # needs debugging

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

###
### 2.1. Model 2: Poisson GLM

# In short: Use case: What is the expected number of registrations per year in each district?
## Uses district and reference year as predictors, my Poisson GLM hopes to estimate
## the expected number of registrations in each district for a given year.

"""
Overview of the Model

We constructed a Poisson Generalized Linear Model (GLM) to estimate the number of dog registrations (NumberOfDogs)
based on the predictors ReferenceYear and DistrictCoded. The goal was to identify trends and distributions
in dog ownership across Zurich’s neighborhoods to support Pet Paradise's expansion strategy.
"""
# Fit/build Poisson GLM model
poisson_model <- glm(NumberOfDogs ~ ReferenceYear + DistrictCoded,
                     family = poisson, data = df.dogs)

# Summary of the Poisson GLM
summary(poisson_model)

"""
2.1. Interpretation of GLM Possion model:

Key Findings

    Intercept: The intercept is 0.6227, but its p-value (0.831) indicates it is not statistically significant. This suggests the baseline number of dog registrations is not significantly different from zero when other predictors are at their baseline levels.
    ReferenceYear: The coefficient for ReferenceYear is -0.0003 with a p-value of 0.832. This indicates no significant trend over the years in the number of dog registrations. Hence, temporal trends in dog registrations are not a primary factor for Pet Paradise's planning.
    DistrictCoded: The coefficient for DistrictCoded is -0.0001 with a p-value of 0.927, indicating no significant difference in dog registrations across different districts. This suggests that district-specific variations in dog registrations might not be substantial.

Business Insights and Recommendations

    Geographical Expansion:
        Stable Registration Numbers: Given that neither ReferenceYear nor DistrictCoded significantly impact the number of dog registrations, dog ownership seems stable across different years and districts. Therefore, Pet Paradise can consider expanding uniformly across districts rather than focusing on specific areas with presumed higher dog populations.
        Micro-Targeting Strategy: Despite the lack of significant district-specific trends, further segmentation at a finer geographical level (e.g., neighborhoods or quarters) may reveal more granular insights. Pet Paradise should consider additional local factors such as income levels, housing types, and proximity to parks which may influence dog ownership.

    Product Offerings:
        Breed-Specific Products: Since breed popularity trends weren't significantly captured in the GLM, Pet Paradise should maintain a diverse inventory catering to various breeds. Customer feedback and localized surveys can provide insights into specific breed preferences.
        Dog Size and Age: The model's limitations suggest that additional data on dog size and age could refine predictions. Pet Paradise should stock products tailored to different life stages and sizes (e.g., puppy supplies, senior dog food, small and large breed toys).

    Marketing Strategies:
        Localized Campaigns: Given the uniformity suggested by the model, marketing efforts can be distributed evenly but should still be tailored to local tastes and preferences identified through direct customer engagement.
        Educational Initiatives: Pet Paradise can enhance community engagement through workshops on dog care, which can help identify local needs and preferences, fostering customer loyalty.

So in conclusion, this poisson GLM analysis shows a stability in numbers across Zurich.
Pet Paradise should leverage this stability and expand based on other factors.

"""

# Optional: cross validation (for my learning purposes)
# Set seed for reproducibility
set.seed(123)

# Define train control for cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Train the Poisson GLM model using cross-validation
cv_model <- train(NumberOfDogs ~ ReferenceYear + DistrictCoded,
                  data = df.dogs, method = "glm", family = poisson, trControl = train_control)

# Print the cross-validated model results
print(cv_model)

"""
My learning from the cross valid.
Can be taken out from R Markdown if needed.
Model Deviance and AIC

    Null Deviance: 191.58 on 70,966 degrees of freedom
        Explanation: The null deviance measures how well the response variable (number of dog registrations) is predicted by a model that includes only the intercept. This represents the variance in the data that a simple model with no predictors would explain. The high value indicates considerable variation in the number of dog registrations across the dataset.

    Residual Deviance: 191.53 on 70,964 degrees of freedom
        Explanation: The residual deviance measures how well the response variable is predicted by the full model with the predictors (ReferenceYear and DistrictCoded). The minimal reduction from the null deviance (191.58 to 191.53) suggests that the predictors in the model do not significantly improve the fit compared to the null model.

    AIC (Akaike Information Criterion): 142,281
        Explanation: AIC is a measure of the relative quality of statistical models for a given dataset. It balances the model fit and complexity. Lower AIC values indicate a better-fitting model. The very high AIC value indicates that while the model may have a reasonable fit, it is quite complex relative to the amount of information it provides.

    Fisher Scoring Iterations: 4
        Explanation: This indicates the number of iterations taken by the Fisher scoring algorithm to converge to a solution. Four iterations suggest that the model parameters converged relatively quickly, which is typical for GLMs with well-behaved data.

Cross-Validation Results

    Cross-Validation Setup:
        Method: 5-fold cross-validation
        Purpose: To assess the model's predictive performance and generalizability by partitioning the data into five subsets, training on four, and validating on the fifth, rotating this process across all subsets.

    Resampling Results:

        RMSE (Root Mean Square Error): 0.0593
            Explanation: RMSE measures the average magnitude of prediction errors. A lower RMSE indicates better predictive accuracy. The very low RMSE suggests that the model's predictions are close to the actual values of dog registrations, although, given the context, this could also indicate the model’s low variability in the response variable.

        R-squared: 0.0002289192
            Explanation: R-squared indicates the proportion of variance in the response variable explained by the predictors. The very low R-squared value suggests that the predictors (ReferenceYear and DistrictCoded) explain almost none of the variability in the number of dog registrations. This is consistent with the insignificant coefficients observed.

        MAE (Mean Absolute Error): 0.0069
            Explanation: MAE measures the average magnitude of errors in predictions without considering their direction. The very low MAE, similar to RMSE, indicates the predictions are close to the actual values but should be interpreted cautiously in the context of the small variation in the response.

Business Insights and Recommendations

    Model Fit and Predictive Power:
        The model's null and residual deviance, combined with the very high AIC, suggest that the inclusion of ReferenceYear and DistrictCoded does not significantly improve the prediction of dog registrations.
        The cross-validation results reinforce this, with very low R-squared values indicating that these predictors explain almost none of the variability in the data.

    Further Data Collection:
        Given the low explanatory power of the current predictors, it is crucial to gather more detailed and varied data. Incorporating additional variables such as breed types, dog ages, owner demographics, housing types, and income levels might uncover significant trends and improve model accuracy.

    Geographical and Temporal Stability:
        The minimal difference between null and residual deviance implies that dog registration patterns are relatively stable across districts and years. This suggests that current registration trends can be expected to continue, allowing for uniform expansion planning.

    Localized Marketing and Product Strategy:
        Despite the model's limitations, Pet Paradise should maintain diverse product offerings to cater to various breeds and life stages. Engaging directly with customers through surveys and feedback can help identify specific needs and preferences.
        Localized marketing efforts should be informed by community-specific data beyond district-level aggregates. Neighborhood-level analysis could reveal more nuanced patterns.

    Enhanced Predictive Modeling:
        Future modeling efforts should consider advanced techniques and more granular data segmentation. For example, clustering algorithms could help identify distinct groups within the customer base, while time-series analysis might capture underlying trends not visible in the current model.

"""
# Poisson Vizualsation
# Generate predictions using the fitted model
predictions <- predict(poisson_model, type = "response")

# Visualize the actual vs predicted number of dogs
# Poisson Visualization
# Generate predictions using the fitted model
predictions <- predict(poisson_model, type = "response")

# Visualize the actual vs predicted number of dogs
ggplot(df.dogs, aes(x = NumberOfDogs, y = predictions)) +
  geom_point() +
  labs(title = "Actual vs Predicted Number of Dogs",
       x = "Actual Number of Dogs",
       y = "Predicted Number of Dogs")

"""
Interpretation of vizualisation:
Poor Fit: if the model were performing well, it'd be a more diagonal spread of points,
showing a clear linear relationship between actual and predicted values.
But the current chart does not show this pattern, so it is suggesting that the model
is not capturing the variability in the actual data. Upon reflection, the model was
refined to sum the number of dogs per year, i.e. aggregate it.
"""

# Reiteration of poisson model, now with dogs summed by year for prediction
# Load necessary libraries

# I now sum/aggregate the data by year and district
sum_data <- df.dogs %>%
  group_by(ReferenceYear, DistrictCoded) %>%
  summarize(NumberOfDogs = sum(NumberOfDogs))

# New fit of  Poisson GLM model
poisson_model <- glm(NumberOfDogs ~ ReferenceYear + DistrictCoded,
                     family = poisson, data = sum_data)
summary(poisson_model)
predictions <- predict(poisson_model, type = "response")
sum_data$PredictedNumberOfDogs = predictions

# Visualize the actual vs predicted number of dogs
ggplot(sum_data, aes(x = NumberOfDogs, y = PredictedNumberOfDogs)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Actual vs Predicted Number of Dog Registrations",
       x = "Actual Number of Dog Registrations",
       y = "Predicted Number of Dog Registrations")

# To check which poisson model is better, I compare the goodness-of-fit.
# This can be done using AIC (because same dataset, same response variable, same distribution (poisson))
# and comparing coefficients.

# Nochmals first poisson model
summary_first <- summary(first_poisson_model)
print("First poisson model:")
print(summary_first)

null_deviance_first <- summary_first$deviance[1]
residual_deviance_first <- summary_first$deviance[2]
coefficients_first <- summary_first$coefficients

# Nochmals second poisson model
summary_second <- summary(second_poisson_model)
print("Second poisson model:")
print(summary_second)

null_deviance_second <- summary_second$deviance[1]
residual_deviance_second <- summary_second$deviance[2]
coefficients_second <- summary_second$coefficients


"""
First poisson model:
(Intercept)    0.6227053  2.9106351   0.214    0.831
ReferenceYear -0.0003063  0.0014414  -0.213    0.832
DistrictCoded -0.0001025  0.0011205  -0.091    0.927

Null deviance: 191.58  on 70966  degrees of freedom
Residual deviance: 191.53  on 70964  degrees of freedom
AIC: 142281

Interpretation: the difference between null and residual deviance is miniscule - few
decimals. Means the model doesn't explain much variablility, poor fit.

Second poisson model:
(Intercept)   -81.666277   2.925105  -27.92   <2e-16 ***
ReferenceYear   0.043686   0.001448   30.16   <2e-16 ***
DistrictCoded  -0.009640   0.000368  -26.20   <2e-16 ***

Null deviance: 24077  on 110  degrees of freedom
Residual deviance: 22173  on 108  degrees of freedom
AIC: 23067

exp(0.043686) # output 1.044654. So increase by a factor of 1.044654 per Reference Year.
# same as increase of dogs by 4.5% per year.

Interpretation: big difference between null deviance (24077) and residual (22173).
Degrees of freedom here are lower, so the predictors improved the model's fit.
The AIC value (23'067) is lower than the first model's AIC (142'281), means better fit.
The coefficients of the second model are statistically significant where p-value < 0.001,
and shows an increase of registrations per year (RefYear = 0.044).
The negative district coefficient needs to be log transformed to be interpreted since it's a poisson model.
exp(District coefficient) = exp(−0.009640) = ca. 0.96. This is a percentage.
So for every one-unit increase in District (ie just one neighbourhood to the next),
the predicted number of dog registrations approximately decreases by 0.96% decreases.


Thus, the 2nd poisson model with summed values per year is more appropriate to use.
"""


# check which neighbouhood is worth expanding into
# ERROR CODE. TRY TO DEBUG IF TIME.

"""
neighborhood_data <- df.dogs %>%
  group_by(ReferenceYear, QuarterCoded) %>%
  summarize(NumberOfDogs = sum(NumberOfDogs))

# fit
poisson_model_neighborhood <- glm(NumberOfDogs ~ ReferenceYear + QuarterCoded,
                                  family = poisson, data = neighborhood_data)
summary(poisson_model_neighborhood)

# calc residuals, then ID highest residuals
neighborhood_data$residuals <- residuals(poisson_model_neighborhood, type = "response")

high_residual_neighborhoods <- neighborhood_data %>%
  filter(residuals > 0) %>%
  arrange(desc(residuals)) %>%
  group_by(QuarterCoded) %>%
  summarize(TotalResiduals = sum(residuals))

# Top neighborhoods
top_neighborhoods <- high_residual_neighborhoods %>%
  arrange(desc(TotalResiduals)) %>%
  top_n(5, TotalResiduals)

# time series forecast for next 5 years
install.packages("forecast")
library(forecast)

# Example for one neighborhood
neighborhood_ts <- neighborhood_data %>%
  filter(QuarterCoded == "specific_neighborhood") %>%
  ts(start = min(neighborhood_data$ReferenceYear), frequency = 1)

fit <- auto.arima(neighborhood_ts$NumberOfDogs)
forecast(fit, h = 5)
"""

#Poisson model conclusion

"""
Key Findings
ReferenceYear:
The coefficient for ReferenceYear is 0.044, with a strongly significant p-value.
There is an increasing trend in dog registrations, so Pet Paradise has a basis for expansion to meet increasing dog ownership needs.
DistrictCoded:
The coefficient for DistrictCoded is, after log transformation, 96% with a strongly significant p-value.
There’s a 4% decrease in dog registrations as the district code increases, there is variation between neighbourhoods. Pet paradise should avoid expanding into these districts with consistently fewer dog registrations. More granular segmentation level (e.g., Quartiers) may provide even more targeted insights, although for a business, a expanding in the business district of a given neighbourhood may be more appropriate than in the living Quartier of the most dog-populous sub-area. Pet Paradise can focus on districts with higher registrations for more intensive marketing campaigns.
Conclusion
In the second poisson GLM analysis, it is shown that Pet Paradise can expand in District 1 in particular,
and that the demand for dogs grows by 4.5% each year.

"""

