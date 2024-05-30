library(dplyr) # Data manipulation, cleaning.
library(e1071)  # library for SVMs
library(ggplot2) # Data visualization, plotting
library(MASS) # Statistical methods, data analysis.
library(caret) # For model training
library(doParallel)

df_EN <- read.csv("../Datasets/df_EN.csv")

# What owner age brackets / income brackets should Pet Paradise target for
# puppy / senior dog merchandise?

#### DATA PREPARATION ####

head(df_EN)

df_EN_svm <- df_EN

# Replacing dog age unknowns (999) with mean
mean_dog_age <- mean(df_EN_svm$DogAgeGroupCd[df_EN_svm$DogAgeGroupCd != 999], na.rm = TRUE)

df_EN_svm <- df_EN_svm %>%
  mutate(DogAgeGroupCd = ifelse(DogAgeGroupCd == 999, mean_dog_age, DogAgeGroupCd))

# Replacing owner age unknowns (999) with mean
mean_owner_age <- mean(df_EN_svm$OwnerAgeGroupCd[df_EN_svm$OwnerAgeGroupCd != 999], na.rm = TRUE)

df_EN_svm <- df_EN_svm %>%
  mutate(OwnerAgeGroupCd = ifelse(OwnerAgeGroupCd == 999, mean_owner_age, OwnerAgeGroupCd))

#### VISUALIZATION ####


# Summarize the counts for each combination of ages
count_data <- df_EN_svm %>% 
  group_by(DogAgeGroupCd, OwnerAgeGroupCd) %>% 
  summarize(count = n())

# Create the heatmap plot with larger dots
ggplot(count_data, aes(x = DogAgeGroupCd, y = OwnerAgeGroupCd, size = count)) +
  geom_point(color = "darksalmon", alpha = 0.7) +  # Adjust color and transparency of dots
  scale_size_continuous(range = c(5, 12)) +  # Adjust the range of dot sizes
  labs(title = "Owner Age Group vs Dog Age Group",
       x = "Dog Age Group",
       y = "Owner Age Group") +
  theme_minimal()

#### DATA SAMPLING ####

# Create 70% data partition
set.seed(123)  # for reproducibility
intrain <- createDataPartition(y = df_EN_svm$OwnerAgeGroupCd, p = 0.7, list = FALSE)
training <- df_EN_svm[intrain, ]
testing <- df_EN_svm[-intrain, ]


#### LINEAR KERNEL ####

# Train control
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

svm_linear <- train(OwnerAgeGroupCd ~ DogAgeGroupCd,
                    data = training,
                    method = "svmLinear",
                    trControl = trctrl,
                    trace = TRUE,
                    tuneLength = 10)

saveRDS(svm_linear, file = "../ML1_Final_Dogs_cache/svm/svm_linear_model.RDS")

#### MODEL EVALUATION ####

# Predictions on testing data
predictions <- predict(svm_linear, testing)
predictions <- round(predict(svm_linear, testing), -1) # Round to nearest x10

# Create a table of predicted vs. true values using the testing data
misclass <- table(predict = predictions, truth = round(testing$OwnerAgeGroupCd, -1))
misclass

# Convert misclass to a data frame
misclass_df <- as.data.frame.table(misclass)

# Plot confusion matrix
ggplot(misclass_df, aes(x = truth, y = predict, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix",
       x = "Reference",
       y = "Prediction",
       fill = "Frequency")














#### RADIAL KERNEL #####


# Train SVM with Radial Basis Function (RBF) kernel
svm_rbf <- train(OwnerAgeGroupCd ~ DogAgeGroupCd,
                 data = training,
                 method = "svmRadial",
                 trControl = trctrl,
                 trace = TRUE,
                 tuneLength = 10
)

svm_rbf

# Save the models

saveRDS(svm_rbf, file = "../ML1_Final_Dogs_cache/svm/svm_radial_model.RDS")
