library(dplyr) # Data manipulation, cleaning.
# library(e1071)  # library for SVMs
library(ggplot2) # Data visualization, plotting
library(MASS) # Statistical methods, data analysis.
library(caret) # Machine learning, model training.
# library(forecast) # needs debugging

# Caution! You might need to change the path to open the csv on your device.
df_EN <- read.csv("Datasets/df_EN.csv")
head(df_EN)

###
### 2.1. Model 2: Poisson GLM

# In short: Use case: What is the expected number of registrations per year in each district?
## Uses district and reference year as predictors, my Poisson GLM hopes to estimate
## the expected number of registrations in each district for a given year.

"""
Overview of the Model

We constructed a Poisson Generalized Linear Model (GLM) to estimate the number of dog registrations (NumberOfDogs)
based on the predictors KeyDateYear and DistrictCd. The goal was to identify trends and distributions
in dog ownership across Zurich’s neighborhoods to support Pet Paradise's expansion strategy.
"""
#```{r poisson_intro}
# Fit/build Poisson GLM model
poisson_model <- glm(NumberOfDogs ~ KeyDateYear + DistrictCd,
                     family = poisson, data = df_EN)

# Summary of the Poisson GLM
summary(poisson_model)
# ```

#```{r poisson_cross_validation}
# cross validation
set.seed(123)
control <- trainControl(method = "cv", number = 5)

# Train the Poisson GLM model thanks to cross-validation
cv_model <- train(NumberOfDogs ~ KeyDateYear + DistrictCd,
                  data = df_EN, method = "glm", family = poisson, trControl = control)
print(cv_model)
# ```

"""
2.1. Interpretation of GLM Possion model:

Key Findings

KeyDateYear: The coefficient for KeyDateYear is -0.0003 with a p-value of 0.832. There's no significant trend over the years in the number of dog registrations. Hence, seasonal trends in dog registrations are not a primary factor for Pet Paradise's planning.
DistrictCd: The coefficient for the neighbourhood is -0.0001 with a p-value of 0.927, so no significant difference in dog registrations across different districts. This suggests that district-specific variations in dog registrations might not be substantial.

Specifically in terms of client recommendations, geographical expansion can be addressed. The stable registration numbers mean that given that neither KeyDateYear nor DistrictCd significantly impact the number of dog registrations, dog ownership seems stable across different years and districts. Therefore, Pet Paradise can consider expanding uniformly across districts rather than focusing on specific areas with presumed higher dog populations.
Therefore in conclusion, this poisson GLM analysis shows a stability in numbers across Zurich. Pet Paradise should leverage this stability and expand based on other factors.

"""

"""
Model Deviance and AIC

With a null deviance of 191.58 on 70,966 degrees of freedom, the high value indicates considerable variation in the number of dog registrations across the dataset.
The residual deviance of 191.53 on 70,964 degrees of freedom how a minimal reducement from the null deviance which means that  the predictors in the model do not significantly improve the fit compared to the null model.
The high AIC value indicates that while the model may have a reasonable fit, it is quite complex relative to the amount of information it provides.
A Fisher Scoring Iterations of four iterations suggests that the model parameters have quickly converged, which we can expect for GLMs with well-behaved data.

Regarding the cross-validation, we used a 5-fold validation, i.e. breaking the data into five subsets, training on four, and testing on the fifth.
The low Root Mean Square Error of 0.0593 shows that the model's predictions are close to the actual values of dog registrations.
This R-squared shows that the predictors (here KeyDateYear and DistrictCd) explain almost none of the variability in the number of dog registrations. This is consistent with the insignificant coefficients observed.

In terms of client insights, the null hypothesis was proven correct in this case; the stability in the number of dog registrations across different years and districts suggests that Pet Paradise can plan for uniform expansion without focusing on specific districts.
Other factors such as owner's age, whether they own a pedigree dog or not, etc might be more influential in determining the demand for pet services, which we will examine in further models.

"""
#```{r poisson_vizualsation}
# Generate predictions using the fitted model
predictions <- predict(poisson_model, type = "response")

# Visualize the actual vs predicted number of dogs
ggplot(df_EN, aes(x = NumberOfDogs, y = predictions)) +
  geom_point() +
  labs(title = "Actual vs Predicted Number of Dogs",
       x = "Actual Number of Dogs",
       y = "Predicted Number of Dogs")
#```
"""
Interpretation of vizualisation:
The vizualisation helps us see that there is a poor fit; if the model were performing well, it'd be a more diagonal spread of points,
showing a clear linear relationship between actual and predicted values.
But the current chart does not show this pattern, so it is suggesting that the model
is not capturing the variability in the actual data. Upon reflection, the model was
refined to sum the number of dogs per year, i.e. aggregate it.
"""

#```{r poisson_2nd_model}
# Reiteration of poisson model, now with dogs summed by year for prediction.

# I now sum/aggregate the data by year and district
sum_data <- df_EN %>%
  group_by(KeyDateYear, DistrictCd) %>%
  summarize(NumberOfDogs = sum(NumberOfDogs))

# New fit of  Poisson GLM model
poisson_model <- glm(NumberOfDogs ~ KeyDateYear + DistrictCd,
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

#```
"""
To check which poisson model is better, we compare the goodness-of-fit.
This can be done using AIC because the models come from the same dataset, have the same response variable, as well as the same poisson distribution.
We also compare coefficients.
"""

#```{r poisson_comparisons}
#first poisson model
summary_first <- summary(first_poisson_model)
print("First poisson model:")
print(summary_first)

null_deviance_first <- summary_first$deviance[1]
residual_deviance_first <- summary_first$deviance[2]
coefficients_first <- summary_first$coefficients

# second poisson model
summary_second <- summary(second_poisson_model)
print("Second poisson model:")
print(summary_second)

null_deviance_second <- summary_second$deviance[1]
residual_deviance_second <- summary_second$deviance[2]
coefficients_second <- summary_second$coefficients

#```

"""
First poisson model:
(Intercept)    0.6227053  2.9106351   0.214    0.831
KeyDateYear -0.0003063  0.0014414  -0.213    0.832
DistrictCd -0.0001025  0.0011205  -0.091    0.927

Null deviance: 191.58  on 70966  degrees of freedom
Residual deviance: 191.53  on 70964  degrees of freedom
AIC: 142281

Interpretation: the difference between null and residual deviance is miniscule - few
decimals. Means the model doesn't explain much variablility, poor fit.

Second poisson model:
(Intercept)   -81.666277   2.925105  -27.92   <2e-16 ***
KeyDateYear   0.043686   0.001448   30.16   <2e-16 ***
DistrictCd  -0.009640   0.000368  -26.20   <2e-16 ***

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

#Poisson model conclusion

"""
Key Findings
KeyDateYear:
The coefficient for KeyDateYear is 0.044, with a strongly significant p-value.
There is an increasing trend in dog registrations, so Pet Paradise has a basis for expansion to meet increasing dog ownership needs.
DistrictCd:
The coefficient for DistrictCd is, after log transformation, 96% with a strongly significant p-value.
There’s a 4% decrease in dog registrations as the district code increases, there is variation between neighbourhoods. Pet paradise should avoid expanding into these districts with consistently fewer dog registrations. More granular segmentation level (e.g., Quartiers) may provide even more targeted insights, although for a business, a expanding in the business district of a given neighbourhood may be more appropriate than in the living Quartier of the most dog-populous sub-area. Pet Paradise can focus on districts with higher registrations for more intensive marketing campaigns.
Conclusion
In the second poisson GLM analysis, it is shown that Pet Paradise can expand in District 1 in particular,
and that the demand for dogs grows by 4.5% each year.

"""

