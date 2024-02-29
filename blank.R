# This script is for exploring the data and generating code snippets

################################################################################

# Loading libraries

library(tidyverse)
library(dplyr)

################################################################################

# Importing CSV file

dogs <- read.csv("Data/kul100od1001.csv")

################################################################################

# GENERAL OBSERVATIONS

head(dogs)
dim(dogs)

# Years that the data set covers

unique_years <- dogs %>%
  distinct(StichtagDatJahr)

unique_years

# Counting dogs by sex

sex_counts <- dogs %>%
  group_by(SexLang) %>%
  summarize(count = n())

sex_counts