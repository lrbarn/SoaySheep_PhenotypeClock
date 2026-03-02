#### 3b. Reformatting the early life data ####
## prepping the data ready for the early life modelling by mean centering

#### packages ####
library(tidyverse)

#### Data ####
birth_data <- read.csv("OutputData/EarlyLife/Birth_corrected2.csv")
FEC_data <- read_csv("OutputData/EarlyLife/FECaverage_Y.csv")
NAO_data <- read_csv("OutputData/EarlyLife/NAO_firstWinter.csv")
population_data <- read_csv("OutputData/EarlyLife/VillagePopulations.csv")

#### Need to split up by sex!
## Just adjust the input at the top, commenting out m/f
## getting the female IDs from the predicted ages....
# age_predictions_f <- read_csv("OutputData/FinalModelData/SheetClock/f_predictions.csv")
# f_IDs <- age_predictions_f %>% select(testID)

# age_predictions_m <- read_csv("OutputData/FinalModelData/SheetClock/m_predictions.csv")
# m_IDs <- age_predictions_m %>% select(testID)

#### BirthData ####
## NOTE FOR FEMALE
## birth data has 6471 IDs
## age predictions has 1603
## f_IDs has 1603

## NOTE FOR MALE
## birth Data has 6471 IDS
## age predictions has 1078 IDs
## m_IDs has 1078

## formatting
birth_data <- birth_data %>% 
  filter(ID %in% f_IDs$testID) 

mean_birthWt <- mean(birth_data$birthWeight_resid, na.rm = TRUE)
sd_birthWt <- sd(birth_data$birthWeight_resid, na.rm = TRUE)

birth_data <- birth_data %>% 
  mutate(birthWeight_resid.z = ((birthWeight_resid - mean_birthWt) / sd_birthWt),
         singleton_coded = factor(case_when(singleton == "YES" ~ 0,
                                            singleton == "NO" ~ 1))) %>% 
  select(ID, birthWeight_resid, birthWeight_resid.z, singleton)

#### Fec data ####
mean_fec <- mean(FEC_data$meanFEC, na.rm = TRUE)
sd_FEC <- sd(FEC_data$meanFEC, na.rm = TRUE)

FEC_data <- FEC_data %>% 
  mutate(mean_fec.z = ((meanFEC - mean_fec)/ sd_FEC))

#### NAO ####
# only need to do once
NAO_data <- NAO_data %>% 
  filter(Year >= 1985)
mean_NAO <- mean(NAO_data$DJF, na.rm = TRUE)
sd_NAO <- mean(NAO_data$DJF, na.rm = TRUE)

NAO_data <- NAO_data %>% 
  mutate(NAO.z = ((DJF - mean_NAO)/sd_NAO))

#### Village Populations ####
# only need to do once
mean_pop <- mean(population_data$VillTotal_Womb, na.rm = TRUE)
sd_pop <- mean(population_data$VillTotal_Womb, na.rm = TRUE)

population_data <- population_data %>% 
  mutate(VillTotal_birth.z = ((VillTotal_birth - mean_pop) / sd_pop),
         VillTotal_womb.z = ((VillTotal_Womb - mean_pop) / sd_pop))

#### Saving Output ####
# write_csv(birth_data, "OutputData/EarlyLife/Z_birth_f.csv")
# 
# 
# ## only need one version since its sex independent
# write_csv(NAO_data, "OutputData/EarlyLife/Z_NAO.csv")
# write_csv(population_data, "OutputData/EarlyLife/Z_pop.csv")
# write_csv(FEC_data, "OutputData/EarlyLife/Z_FEC_m.csv")