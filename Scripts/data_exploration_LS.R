# This script is for exploring the data and generating code snippets
# If CSV fails to import, make sure that directory is set to:
# Session -> Set Working Directory -> To Source File Location

################################################################################

# Loading libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(mgcv)
#### LS added: ################
library(zoo)

################################################################################

# Importing CSV file
df.dogs <- read.csv("../Datasets/kul100od1001.csv")

# Importing city polygons
st.zh <- st_read("../Datasets/stzh.adm_stadtkreise_a.geojson")

################################################################################

# TRANSLATION OF COLUMN NAMES

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
write.csv(df.dogs, "kul100od1001_EN.csv", row.names = FALSE)
head(df.dogs)
colnames(df.dogs)


################################################################################

# DATA CLEANING + ENRICHMENT

# Setting NA's (AgeV10Coded = 999 years, DogAgeSort = 999)
df.dogs <- df.dogs %>%
  mutate(
    AgeV10Coded = ifelse(AgeV10Coded == 999, NA, AgeV10Coded),
    AlterVHundSort = ifelse(DogAgeSort == 999, NA, DogAgeSort)
  )

### LS added #######################################################
# Calculate the mean of the column
avg_AgeV10Coded <- mean(df.dogs$AgeV10Coded, na.rm = TRUE)

# Round down to the nearest tens
rounded_value <- floor(avg_AgeV10Coded / 10) * 10

# Replace NA values with the rounded down value
df.dogs <- df.dogs %>%
  mutate(
    AgeV10Coded = ifelse(is.na(AgeV10Coded), rounded_value, AgeV10Coded),
    AlterVHundSort = ifelse(is.na(AlterVHundSort), rounded_value, AlterVHundSort)
  )

write.csv(df.dogs, "kul100od1001_EN_avg.csv", row.names = FALSE)

#############################################################

# New DogSize column
df.dogs <- df.dogs %>%
  mutate(DogSize = case_when(
    BreedTypeLong == "Kleinwüchsig" ~ "small",
    BreedTypeLong == "Rassentypenliste I" ~ "large",
    BreedTypeLong == "Rassentypenliste II" ~ "banned",
    TRUE ~ NA_character_
  ))

################################################################################

# GENERAL OBSERVATIONS

# These observations are done on the entire data set (2015-2023)

dim(df.dogs)
str(df.dogs)

# Years that the data set covers
year_count <- df.dogs %>%
  distinct(ReferenceYear)
year_count

################################################################################

df.dogs <- df.dogs %>%
  mutate(
    OwnerSexText = as.factor(OwnerSexText),
    AgeV10Text = as.factor(AgeV10Text),
    Breed1Text = as.factor(Breed1Text),
    Breed2Text = as.factor(Breed2Text),
    DogSexText = as.factor(DogSexText),
    DogColorText = as.factor(DogColorText),
    DogSize = as.factor(DogSize),
    DogAgeText = as.factor(DogAgeText)
  )

################################################################################

# MODEL 01 -- DOG COUNT BY DISTRICT + LINEAR REGRESSIONS

dog_count_per_neighborhood_year <- df.dogs %>%
  group_by(DistrictText, ReferenceYear) %>%
  summarize(DogCount = n())

ggplot(dog_count_per_neighborhood_year,
       aes(x = ReferenceYear,
           y = DogCount,
           color = DistrictText)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Dog registrations by neighborhood - Linear Model",
       x = "",
       y = "",
       color = "District") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(dog_count_per_neighborhood_year$ReferenceYear))

################################################################################

# MODEL 02 -- DOG COUNT BY DISTRICT + SMOOTHER

ggplot(dog_count_per_neighborhood_year,
       aes(x = ReferenceYear, y = DogCount,
           color = DistrictText)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Dog registrations by neighborhood - Smoother",
       x = "",
       y = "",
       color = "District") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(dog_count_per_neighborhood_year$ReferenceYear))

################################################################################

# MODEL 03 -- DOG COUNT BY DISTRICT + QUADRATIC REGRESSION

ggplot(dog_count_per_neighborhood_year,
       aes(x = ReferenceYear, y = DogCount,
           color = DistrictText)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(title = "Dog registrations by neighborhood - Quadratic Regression",
       x = "",
       y = "",
       color = "District") +
  theme_minimal() +
  scale_x_continuous(breaks = unique(dog_count_per_neighborhood_year$ReferenceYear))

################################################################################

# DOG COUNT BY DOG SEX AND BY YEAR

df.dogsex_count_year <- df.dogs %>%
  group_by(ReferenceYear, DogSexText) %>%
  summarize(count = n())

df.dogsex_count_year

# Barplot
ggplot(df.dogsex_count_year,
       aes(x = factor(ReferenceYear),
           y = count,
           fill = DogSexText)) +
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

ownSex_count_year <- df.dogs %>%
  group_by(ReferenceYear, OwnerSexText) %>%
  summarize(count = n())

ownSex_count_year

# Barplot
ggplot(ownSex_count_year,
       aes(x = factor(ReferenceYear),
           y = count,
           fill = OwnerSexText)) +
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
ownAge_count <- df.dogs %>%
  group_by(AgeV10Coded) %>%
  summarize(count = n())
ownAge_count

# Barplot
ggplot(ownAge_count,
       aes(x = factor(AgeV10Coded),
           y = count,
           fill = AgeV10Coded)) +
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

sex_age_counts <- df.dogs %>%
  group_by(AgeV10Coded, OwnerSexText) %>%
  summarize(count = n())

# Barplot
ggplot(sex_age_counts,
       aes(x = factor(AgeV10Coded),
           y = count,
           fill = OwnerSexText)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  theme_minimal() +
  labs(title = "Registered dogs by owner age group and owner sex",
       x = "Age Group",
       y = "Dog Count",
       fill = "Sex") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  scale_fill_manual(values = c("männlich" = "lightblue",
                               "weiblich" = "pink"))

################################################################################

# BOXPLOTS OF REGISTERED DOG AGES PER YEAR

# Boxplot
ggplot(df.dogs, aes(x = factor(ReferenceYear), y = AlterVHundSort)) +
  geom_boxplot(fill = "black", color = "black", alpha = 0.1) +
  theme_minimal() +
  labs(title = "Boxplots of registered dog ages per year",
       x = "",
       y = "")

################################################################################

# RACE TYPE COUNT PER NEIGHBOUDHOOD

# Counting race types: 4
raceType_count <- df.dogs %>%
  group_by(DogSize, DistrictText) %>%
  summarize(count = n())

# Barplot
ggplot(raceType_count,
       aes(x = factor(DistrictText),
           y = count,
           fill = DogSize)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  facet_wrap(~ DogSize, scales = "free_y", nrow = 2) +
  theme_minimal() +
  labs(title = "Dog size per neighbourhood",
       x = "",
       y = "",
       fill = "Race type") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "")


################################################################################

# RACE TYPE COUNT PER NEIGHBOURHOOD MAP

dogSize_count_geo <- merge(st.zh,
                   raceType_count,
                   by.x = "bezeichnung",
                   by.y = "DistrictText")
dogSize_count_geo$DogSize <- factor(dogSize_count_geo$DogSize)

ggplot(dogSize_count_geo) +
  geom_sf(aes(fill = DogSize,
              alpha = count),
          color = "white",
          size = 0.2)  +
  geom_sf_text(aes(label = count),
               size = 2,
               check_overlap = TRUE) +
  facet_wrap(~ DogSize,
             nrow = 2) +
  labs(title = "Map with dog sizes",
       xlab = "Longitude",
       ylab = "Latitude") +
  scale_fill_manual(name = "Race type",
                    values = scales::hue_pal()(length(unique(dogSize_count_geo$DogSize)))) +
  scale_alpha_continuous(name = "Register count",
                         range = c(0.2, 1)) +
  theme_minimal()

############################################################

### LS added Binomial
# Create a tibble dataframe to count dogs by dog sex
dog_sex_counts <- df.dogs %>%
  group_by(DogSexCoded) %>%
  summarize(count = n())

# Binomial regression plot
ggplot(dog_sex_counts, aes(x = DogSexCoded, y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Count of Male vs. Female Dogs",
       x = "Sex",
       y = "Count") +
  theme_minimal() +
  geom_smooth(method = "glm", formula = y ~ x, method.args = list(family = "binomial"))

# Recode DogSexCoded to be binary (1 for female, 0 for male)
df.dogs$DogSexCoded <- ifelse(df.dogs$DogSexCoded == 2, 1, 0)


# Fit binomial regression model
binomial_model <- glm(DogSexCoded ~ AgeV10Coded, data = df.dogs, family = binomial())

# Display summary of the model
summary(binomial_model)


#############################################################

### LS added poissan
# Create a tibble dataframe to count dogs by dog sex
dog_sex_counts <- df.dogs %>%
  group_by(DogSexCoded) %>%
  summarize(count = n())

# Binomial regression plot
ggplot(dog_sex_counts, aes(x = DogSexCoded, y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Count of Male vs. Female Dogs",
       x = "Sex",
       y = "Count") +
  theme_minimal() +
  geom_smooth(method = "glm", formula = y ~ x, method.args = list(family = "binomial"))

# Create a tibble dataframe to count dogs by dog sex
dog_sex_counts <- df.dogs %>%
  group_by(DogSexCoded) %>%
  summarize(count = n())

# Binomial regression plot
ggplot(dog_sex_counts, aes(x = DogSexCoded, y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Count of Male vs. Female Dogs",
       x = "Sex",
       y = "Count") +
  theme_minimal() +
  geom_smooth(method = "glm", formula = y ~ x, method.args = list(family = "binomial"))

