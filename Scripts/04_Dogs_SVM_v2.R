library(dplyr) # Data manipulation, cleaning.
library(e1071)  # library for SVMs
library(ggplot2) # Data visualization, plotting
library(MASS) # Statistical methods, data analysis.
library(caret) # For model training
library(doParallel)

df_EN <- read.csv("../Datasets/df_EN.csv")
head(df_EN)

#### DATA PREPARATION ####

df_EN_svm <- df_EN %>%
  filter(DogAgeGroupSort < 100) %>%
  mutate_if(is.character, as.factor)

# Count the occurrences of each primary breed
breed_counts <- df_EN_svm %>%
  count(PrimaryBreed)

# Identify primary breeds occurring only once
rare_primary_breeds <- breed_counts %>%
  filter(n == 1) %>%
  pull(PrimaryBreed)

# Create a new factor variable with PrimaryBreed
df_EN_svm <- df_EN_svm %>%
  mutate(MainBreed = ifelse(PrimaryBreed %in% rare_primary_breeds, "Rare breed", as.character(PrimaryBreed)))

# Convert the new variable to a factor
df_EN_svm$MainBreed <- factor(df_EN_svm$MainBreed)

# Remove NAs
df_EN_svm <- df_EN_svm[complete.cases(df_EN_svm$MainBreed), ]


#### DATA SAMPLING ####

# Create 70% data partition
set.seed(123)  # for reproducibility
intrain <- createDataPartition(y = df_EN_svm$MainBreed, p = 0.7, list = FALSE)
training <- df_EN_svm[intrain, ]
testing <- df_EN_svm[-intrain, ]


#### MODEL TRAINING ####

# Train control
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

# Parallel processing setup
num_cores <- detectCores() - 1
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
print(paste("Number of parallel tasks:", num_cores))

# SVM model training with additional predictors
svm_linear <- train(MainBreed ~ DistrictCd + OwnerAgeGroup + OwnerSex + DogAgeGroup + DogSex,
                    data = training,
                    method = "svmLinear",
                    trControl = trctrl,
                    preProcess = c("center", "scale"),
                    trace = TRUE,
                    tuneLength = 10)

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Save the model
saveRDS(svm_linear, file = "../svm/svm_linear_model.RDS")

# Check the model training log
print(svm_linear)
