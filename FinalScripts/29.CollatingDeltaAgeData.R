#### 29. Collating the ∆Age Data #####

#### Prepping the sheet clock predictions/ ∆age for use in the survival analysis
## manually calculating ∆age

#### Packages ####
library(tidyverse)

#### Data ####
### Ageing data
f_data <- read_csv("OutputData/FinalModelData/SheetClock/f_predictions.csv")
m_data <- vroom::vroom("OutputData/FinalModelData/SheetClock/m_predictions.csv")

#### Collating ####
## adding sex
f_data$Sex <- "1"
m_data$Sex <- "2"

full_data <- rbind(f_data, m_data)

#### Reformatting ####
full_data <- full_data %>% 
  mutate(ID = testID,
         CAge = testAgeY,
         PAge = testAgeYPRED) %>% 
  mutate(delta_age = PAge - CAge) %>% 
  select(ID, CAge, PAge, delta_age, Sex)

## making a list of target IDs
ID_list <- unique(c(full_data$ID))


### Capture Data
female_sheet_z <- read_csv("OutputData/TransformedData/female_sheet_z_NEW.csv")
female_sheet_z <- female_sheet_z %>% drop_na()
female_sheet_z <- female_sheet_z %>% 
  select(ID, CapYear, Weight.z)

female_sheet2 <- female_sheet_z %>% 
  filter(ID %in% ID_list) %>% 
  drop_na()

## checking for duplicates
duplicates <- female_sheet2 %>%
  # Group by the unique combination
  group_by(ID, CapYear) %>%
  # Filter for groups that appear more than once
  filter(n() > 1) %>%
  # Keep only the relevant columns
  select(ID, CapYear) %>%
  # Optional: Keep only unique rows of these duplicates 
  # (remove if you want to see every raw instance)
  distinct()



male_sheet_z <- read_csv("OutputData/TransformedData/male_sheet_z_NEW2.csv")
male_sheet_z <- male_sheet_z %>% drop_na()
male_sheet_z <- male_sheet_z %>% 
  select(ID, CapYear, Weight.z)

male_sheet2 <- male_sheet_z %>% 
  filter(ID %in% ID_list) %>% 
  drop_na()

## checking for duplicates
duplicates <- male_sheet2 %>%
  # Group by the unique combination
  group_by(ID, CapYear) %>%
  # Filter for groups that appear more than once
  filter(n() > 1) %>%
  # Keep only the relevant columns
  select(ID, CapYear) %>%
  # Optional: Keep only unique rows of these duplicates 
  # (remove if you want to see every raw instance)
  distinct()


### Birth Data
birthData <- read_csv("OutputData/TransformedData/BirthPheno_recode.csv")
birthData <- birthData %>% 
  select(ID, BirthYear) %>% 
  filter(ID %in% ID_list)

### Collating cap and birth year
cap_birth_year_f <- left_join(female_sheet2, birthData)
cap_birth_year_f <- cap_birth_year_f %>% 
  mutate(CAge = CapYear - BirthYear)

cap_birth_year_m <- left_join(male_sheet2, birthData)
cap_birth_year_m <- cap_birth_year_m %>% 
  mutate(CAge = CapYear - BirthYear)

capBirth <- rbind(cap_birth_year_f, cap_birth_year_m)
capBirth <- unique(capBirth)


#### Adding birth Year and weight ####
full_data2 <- left_join(full_data, capBirth)

### checking for duplicates
duplicates <- full_data2 %>%
  # Group by the unique combination
  group_by(ID, CAge) %>%
  # Filter for groups that appear more than once
  filter(n() > 1) %>%
  # Keep only the relevant columns
  select(ID, CAge) %>%
  # Optional: Keep only unique rows of these duplicates 
  # (remove if you want to see every raw instance)
  distinct()


#### Resave ####
#write_csv(full_data2, "OutputData/FinalModelData/AccelerationScores/delta_age_collation_V5_weight.csv")

#### For the composite clock ####
#### Data ####
### Ageing data
f_data <- read_csv("OutputData/FinalModelData/PhenotypeClock/f_predictions.csv")
m_data <- vroom::vroom("OutputData/FinalModelData/PhenotypeClock/m_predictions.csv")

### Capture Data
female_sheet_z <- read_csv("OutputData/TransformedData/female_sheet_z.csv")
female_sheet_z <- female_sheet_z %>% 
  select(ID, CapYear)

male_sheet_z <- read_csv("OutputData/TransformedData/male_sheet_z.csv")
male_sheet_z <- male_sheet_z %>% 
  select(ID, CapYear)

### Birth Data
birthData <- read_csv("OutputData/TransformedData/BirthPheno_recode.csv")
birthData <- birthData %>% 
  select(ID, BirthYear)

### Collating cap and birth year
cap_birth_year_f <- left_join(female_sheet_z, birthData)
cap_birth_year_f <- cap_birth_year_f %>% 
  mutate(CAge = CapYear - BirthYear)

cap_birth_year_m <- left_join(male_sheet_z, birthData)
cap_birth_year_m <- cap_birth_year_m %>% 
  mutate(CAge = CapYear - BirthYear)

capBirth <- rbind(cap_birth_year_f, cap_birth_year_m)
capBirth <- unique(capBirth)

#### Collating ####
## adding sex
f_data$Sex <- "1"
m_data$Sex <- "2"

full_data <- rbind(f_data, m_data)

#### Reformatting ####
full_data <- full_data %>% 
  mutate(ID = testID,
         CAge = testAgeY,
         PAge = testAgeYPRED) %>% 
  mutate(delta_age = PAge - CAge) %>% 
  select(ID, CAge, PAge, delta_age, Sex)

#### Addding birth Year ####
full_data2 <- left_join(full_data, capBirth)


#### Resave ####
write_csv(full_data2, "OutputData/FinalModelData/AccelerationScores/FullClock/delta_age_collation_COMPOSITE.csv")

