# LINEAR MODEL

# Effect of categorical variable
# Does the breed-status have an effect on the age at which dogs are registered?

library(ggplot2)
library(tidyverse)
library(plotly)


df_EN <- read.csv("Datasets/df_EN.csv")
head(df_EN)

# Removing outliers
df_EN_cleaned <- subset(df_EN, DogAgeGroupSort < 100)
# Setting MixedBreed to factor
df_EN_cleaned$MixedBreed <- factor(df_EN_cleaned$MixedBreed)
# Setting reference to pure breeds
df_EN_cleaned$MixedBreed <- relevel(df_EN_cleaned$MixedBreed, 
                                    ref = "Pedigree dog")

## BOXPLOTS

boxplots_ggplot <- ggplot(df_EN_cleaned, aes(x = MixedBreed,
                                             y = DogAgeGroupSort, 
                                             fill = MixedBreed)) +
  geom_boxplot() +
  labs(x = "Mixed Breed Status", y = "Dog Registration Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert to an interactive plot
ggplotly(boxplots_ggplot)


## LINEAR MODEL
# Linear model 1
lm.dogs.1 <- lm(DogAgeGroupSort ~ MixedBreed, data = df_EN_cleaned)
summary(lm.dogs.1)

## DROP 1
drop1(lm.dogs.1, test = "F")

## GLTH
if (!require(multcomp, quietly = TRUE)) {
  install.packages("multcomp")
  library(multcomp)
}

# Perform contrast test
glth.test.1 <- glht(model = lm.dogs.1,
                    linfct = mcp(MixedBreed = "Tukey"))
summary(glth.test.1)
