# This script is for exploring the data and generating code snippets

################################################################################

# Loading libraries
library(tidyverse)
library(ggplot2)

################################################################################

# Importing CSV file
dogs <- read.csv("Data/kul100od1001.csv")

# Setting NA's (AlterV10Cd = 999 years)
dogs <- dogs %>%
  mutate(AlterV10Cd = ifelse(AlterV10Cd == 999, NA, AlterV10Cd))

################################################################################

# GENERAL OBSERVATIONS

# These observations are done on the entire data set (2015-2023)

dim(dogs)
head(dogs)

# Years that the data set covers
unique_years <- dogs %>%
  distinct(StichtagDatJahr)

unique_years

# Counting dogs by sex
sex_counts <- dogs %>%
  group_by(SexLang) %>%
  summarize(count = n())

sex_counts

# Counting owners by age-group
owner_counts <- dogs %>%
  group_by(AlterV10Cd) %>%
  summarize(count = n())

owner_counts

################################################################################

# DOG COUNT BY SEX AND BY YEAR

sex_counts_sorted <- dogs %>%
  group_by(StichtagDatJahr, SexLang) %>%
  summarize(count = n())

sex_counts_sorted

# Barplot
ggplot(sex_counts_sorted,
       aes(x = factor(StichtagDatJahr),
           y = count,
           fill = SexLang)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_minimal() +
  labs(title = "Registered dogs by sex and year",
       fill = "Sex") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  scale_fill_manual(values = c("männlich" = "lightblue",
                               "weiblich" = "pink"))

################################################################################

# DOG COUNT BY OWNER AGE GROUP

# Barplot
ggplot(owner_counts,
       aes(x = factor(AlterV10Cd),
           y = count,
           fill = AlterV10Cd)) +
  geom_bar(stat = "identity",
           position = "dodge",
           fill = "darkgrey") +
  theme_minimal() +
  labs(title = "Registered dogs by owner age group",
       fill = "Age group") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "")

################################################################################

# DOG COUNt bY SEX AND OWNER AGE GROUP

sex_age_counts <- dogs %>%
  group_by(AlterV10Cd, SexLang) %>%
  summarize(count = n())

# Barplot
ggplot(sex_age_counts,
       aes(x = factor(AlterV10Cd),
           y = count,
           fill = SexLang)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_minimal() +
  labs(title = "Registered dogs by wwner age group and sex",
       x = "Age Group",
       y = "Dog Count",
       fill = "Sex") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") + 
  scale_y_continuous(name = "") +
  scale_fill_manual(values = c("männlich" = "lightblue",
                               "weiblich" = "pink"))
