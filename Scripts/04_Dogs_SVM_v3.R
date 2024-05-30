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

ggplot(df_EN_svm, aes(x = DogAgeGroupCd, y = OwnerAgeGroupCd)) +
  geom_point() +
  labs(title = "Scatterplot of Owner Age Group vs Dog Age Group",
       x = "Dog Age Group",
       y = "Owner Age Group") +
  theme_minimal()

#### DATA SAMPLING ####

# Create 70% data partition
set.seed(123)  # for reproducibility
intrain <- createDataPartition(y = df_EN_svm$OwnerAgeGroupCd, p = 0.7, list = FALSE)
training <- df_EN_svm[intrain, ]
testing <- df_EN_svm[-intrain, ]


#### MODEL TRAINING ####

# Train control
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

svm_linear <- train(OwnerAgeGroupCd ~ DogAgeGroupCd,
                    data = training,
                    method = "svmLinear",
                    trControl = trctrl,
                    trace = TRUE,
                    tuneLength = 10)

# Train SVM with Radial Basis Function (RBF) kernel
svm_rbf <- train(OwnerAgeGroupCd ~ DogAgeGroupCd,
                 data = training,
                 method = "svmRadial",
                 trControl = trctrl,
                 trace = TRUE,
                 tuneLength = 10
)

svm_linear
svm_rbf

# Save the models
saveRDS(svm_linear, file = "../ML1_Final_Dogs_cache/svm/svm_linear_model.RDS")
saveRDS(svm_rbf, file = "../ML1_Final_Dogs_cache/svm/svm_radial_model.RDS")
