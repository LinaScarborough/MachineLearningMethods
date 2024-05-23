library(dplyr) 
library(plotly) 
library(ggplot2) 
library(stringr)

# Loading dataset downloaded from: https://opendata.swiss/de/dataset/hundebestande-der-stadt-zurich-seit-2015/resource/5f8eafd2-367f-489c-a075-42426d14c586
df <- read.csv("kul100od1001.csv")

# Inspect dataset
head(df)

# Check structure of dataset
str(df)

# Duplicate and rename df
df_EN <- df

# Define translations for each column name
colname_EN <- c("KeyDateYear",      "DatastatusCd",  "OwnerId",   "AgeV10Cd",    "AgeV10Long",    "AgeV10Sort",    "SexCd", "SexLong", "SexSort", "DistrictCd","DistrictLong","DistrictSort","QuarCd", "QuarLong", "QuarSort", "Breed1Text", "Breed2Text", "BreedMixCd",       "BreedMongrelLong",  "MixedBreedSort",      "BreedTypeCd", "BreedTypeLong", "BreedTypeSort", "BirthDateDogYear", "AgeVDogCd",    "AgeVDogLong",    "AgeVDogSort",    "SexVDogCd", "SexDogLong",  "SexDogSort",  "DogColorText",   "DogAmount")
colname_DE <- c("StichtagDatJahr ", "DatenstandCd ", "HalterId ", "AlterV10Cd ", "AlterV10Lang ", "AlterV10Sort ", "SexCd", "SexLang", "SexSort", "KreisCd",   "KreisLang",  "KreisSort",    "QuarCd", "QuarLang", "QuarSort", "Rasse1Text", "Rasse2Text", "RasseMischlingCd", "RasseMischlingLang", "RasseMischlingSort", "RassentypCd", "RassentypLang", "RassentypSort", "GebDatHundJahr",   "AlterVHundCd", "AlterVHundLang", "AlterVHundSort", "SexHundCd", "SexHundLang", "SexHundSort", "HundefarbeText", "AnzHunde")

# Assign translated column names
colnames(df_EN) <- colname_EN

      
  u_BreedMongrelLong   <- unique(df_EN$BreedMongrelLong)
  u_BreedTypeLong      <- unique(df_EN$BreedTypeLong)     
  u_DogColorText       <- unique(df_EN$DogColorText)    


print(u_BreedMongrelLong)
print(u_BreedTypeLong)
print(u_DogColorText)





# Define the old and new strings and Apply str_replace_all() across all columns of the dataframe
# df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, old_string, new_string))

df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "- bis ", " to "))
df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "-Jährige", " old"))

df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "männlich", "male"))
df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "weiblich", "female"))     

df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "Keine", "none"))
df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "Unbekannt", "Unknown"))


df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "Rassehund", "Pedigree dog"))
df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "Mischling, beide Rassen bekannt", "Mixed breed, both breeds known"))
df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "Mischling, sekundäre Rasse unbekannt", "Mixed breed, secondary breed unknown"))
df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "Mischling, beide Rassen unbekannt", "Mixed breed, both breeds unknown"))


df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "Kleinwüchsig", "Small stature"))
df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "Rassentypenliste I", "Breed type list I"))    
df_EN[] <- lapply(df_EN, function(x) str_replace_all(x, "Rassentypenliste II", "Breed type list II"))

# Replacement in ONE column only
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "schwarz", "black")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "saufarben", "burgundy")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "schwarz", "black")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "braun", "brown")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "weiss", "white")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "grau", "gray")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "silber", "silver")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "rot", "red")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "gelb", "yellow")

df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "hell", "light")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "dunkel", "dark")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "gestromt", "brindle")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "schimmel", "mold")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "zweifarbig", "2colors")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "dreifarbig", "3colors")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "vierfarbig", "4colors")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "gemischt", "mixed")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "meliert", "mottled")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "hirschred mit Maske", "stag with mask")
df_EN$DogColorText <- str_replace_all(df_EN$DogColorText, "löwenfarbig", "lion-colored")

u_DogColorText       <- unique(df_EN$DogColorText) 
print(u_DogColorText)

# Print the modified dataframe
View(df_EN)



