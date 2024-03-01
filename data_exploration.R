# This script is for exploring the data and generating code snippets
# If CSV fails to import, make sure that directory is set to:
# Session -> Set Working Directory -> To Source File Location

################################################################################

# Loading libraries
library(tidyverse)
library(ggplot2)

################################################################################

# Importing CSV file
dogs <- read.csv("Data/kul100od1001.csv")

################################################################################

# DATA CLEANING

# Setting NA's (AlterV10Cd = 999 years, AlterVHundSort = 999)
dogs <- dogs %>%
  mutate(
    AlterV10Cd = ifelse(AlterV10Cd == 999, NA, AlterV10Cd),
    AlterVHundSort = ifelse(AlterVHundSort == 999, NA, AlterVHundSort)
  )

################################################################################

# GENERAL OBSERVATIONS

# These observations are done on the entire data set (2015-2023)

dim(dogs)
head(dogs)

# Years that the data set covers
year_count <- dogs %>%
  distinct(StichtagDatJahr)
year_count

################################################################################

# DOG COUNT BY DOG SEX AND BY YEAR

dogSex_count_year <- dogs %>%
  group_by(StichtagDatJahr, SexHundLang) %>%
  summarize(count = n())

dogSex_count_year

# Barplot
ggplot(dogSex_count_year,
       aes(x = factor(StichtagDatJahr),
           y = count,
           fill = SexHundLang)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_minimal() +
  labs(title = "Registered dogs by dog sex per year",
       fill = "Dog sex") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  scale_fill_manual(values = c("männlich" = "lightblue",
                               "weiblich" = "pink"))

################################################################################

# DOG COUNT BY OWNER SEX AND BY YEAR

ownSex_count_year <- dogs %>%
  group_by(StichtagDatJahr, SexLang) %>%
  summarize(count = n())

ownSex_count_year

# Barplot
ggplot(ownSex_count_year,
       aes(x = factor(StichtagDatJahr),
           y = count,
           fill = SexLang)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_minimal() +
  labs(title = "Registered dogs by owner sex per year",
       fill = "Owner sex") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  scale_fill_manual(values = c("männlich" = "lightblue",
                               "weiblich" = "pink"))

################################################################################

# DOG COUNT BY OWNER AGE GROUP

# Counting owners by age-group
ownAge_count <- dogs %>%
  group_by(AlterV10Cd) %>%
  summarize(count = n())
ownAge_count

# Barplot
ggplot(ownAge_count,
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

# DOG COUNT BY SEX AND OWNER AGE GROUP

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
  labs(title = "Registered dogs by owner age group and owner sex",
       x = "Age Group",
       y = "Dog Count",
       fill = "Sex") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") + 
  scale_y_continuous(name = "") +
  scale_fill_manual(values = c("männlich" = "lightblue",
                               "weiblich" = "pink"))

################################################################################

# BOXPLOTS OF REGISTERED DOG AGES PER YEAR

# Boxplot
ggplot(dogs, aes(x = factor(StichtagDatJahr), y = AlterVHundSort)) +
  geom_boxplot(fill = "black", color = "black", alpha = 0.1) +  # Adjust fill color and transparency
  theme_minimal() +
  labs(title = "Boxplots of registered dog ages per year",
       x = "",
       y = "")

################################################################################

# Counting all dog race mixes

race_mix_counts <- dogs %>%
  group_by(RasseMischlingLang) %>%
  summarize(count = n())

race_mix_counts

################################################################################

# Counting all dog races: 394

races_counts <- dogs %>%
  group_by(Rasse1Text) %>%
  summarize(count = n())

races_counts

################################################################################

# Counting race types: 4

racesType_count <- dogs %>%
  group_by(RassentypLang) %>%
  summarize(count = n())

racesType_count
