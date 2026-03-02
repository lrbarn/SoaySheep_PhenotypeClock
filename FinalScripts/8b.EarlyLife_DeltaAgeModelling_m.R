#### 34. ∆Age and Early life - Males ####

## this version is comparing the difference between the ∆age results from the full age range clock and the adult only clock which, this will indicate whether the signal of "accelerated" ageing is picked up even when the "developmental" stages aren't included in the clock model

## this is built on 27 but to reduce confusion there is a new script

## removing nested mumID to be inline with the female one

#### Packages ####
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggeffects)
library(rptR)

#### Data ####
age_predictions_ALL <- read_csv("OutputData/FinalModelData/SheetClock/m_predictions.csv")

## Swapping the ∆age from the adult only clock
# age_predictions_ADULT <- read_csv("OutputData/FinalModelData/SheetClock_Adults/m_predictions.csv")

##### Formatting ####
age_predictions_ALL <- age_predictions_ALL %>% 
  mutate(ID = testID,
         CAge = testAgeY,
         CAge2 = CAge^2,
         PAge = testAgeYPRED) %>% 
  mutate(delta_age = PAge - CAge) %>% 
  select(ID, CAge, CAge2, PAge, delta_age)


##### MetaData ####

metaData <- read_csv("OutputData/TransformedData/BirthPheno_recode.csv")
metaData$Sex <- as.factor(metaData$Sex)

## maternal IDs that are missing
mumIDs <- read_csv("RawData/mumIDs.csv")
age_predictions_ALL <- left_join(age_predictions_ALL, mumIDs)

## Early Life Adversity Measures 
birth_data <- read.csv("OutputData/EarlyLife/Z_birth_m.csv")
FEC_data <- read_csv("OutputData/EarlyLife/Z_FEC.csv")
NAO_data <- read_csv("OutputData/EarlyLife/Z_NAO.csv")
population_data <- read_csv("OutputData/EarlyLife/Z_pop.csv")


##### Formatting #####
# ## Adult data
# full_data_Adult <- left_join(age_predictions_ADULT, metaData)
# full_data_Adult <- left_join(full_data_Adult, birth_data)
# full_data_Adult <- left_join(full_data_Adult, FEC_data)
# full_data_Adult <- left_join(full_data_Adult, maternalLoss_data)
# full_data_Adult <- left_join(full_data_Adult, NAO_data)
# full_data_Adult <- left_join(full_data_Adult, population_data)
# 
# full_data_Adult <- full_data_Adult %>% 
#   select(ID, CAge, CAge2, PAge, delta_age, MumID, birthWeight_resid, singleton, meanFEC, Year, DJF, VillTotal_birth, VillTotal_Womb, BirthYear)

## All age data
full_data_ALL <- left_join(age_predictions_ALL, metaData)
full_data_ALL <- left_join(full_data_ALL, birth_data)
full_data_ALL <- left_join(full_data_ALL, FEC_data)
full_data_ALL <- left_join(full_data_ALL, NAO_data)
full_data_ALL <- left_join(full_data_ALL, population_data)

full_data_ALL <- full_data_ALL %>% 
  select(ID, CAge, CAge2, PAge, delta_age, MumID, Year, BirthYear,
         birthWeight_resid.z, singleton, mean_fec.z,  NAO.z, VillTotal_birth.z, VillTotal_womb.z)
#### Splitting up by age groupings

full_data_lambs <- full_data_ALL %>% 
  filter(CAge == 0)
full_data_yearling <- full_data_ALL %>% 
  filter(CAge == 1)
full_data_adult <- full_data_ALL %>% 
  filter(CAge >= 2)

#### Tidy Up ####
rm(list = c("birth_data", "FEC_data", "metaData", "mumIDs", "NAO_data", "population_data"))

#### Early Life Models ####

##### Full Age Range ####
## may no longer be relevant
mod_early_all <- lmer(delta_age ~ NAO.z + birthWeight_resid.z + singleton + VillTotal_birth.z + VillTotal_womb.z + mean_fec.z +
                        CAge + CAge2 +
                         (1|ID),
                      # BirthYear removed as singular
                      data = (full_data_ALL %>% filter(CAge >0)))

summary(mod_early_all)

##### ∆age from the adult clock ####
## Check which version of ∆age is going in (full clock or adult only clock)
mod_early_adult <- lmer(delta_age ~ NAO.z + birthWeight_resid.z + singleton + VillTotal_birth.z + VillTotal_womb.z + mean_fec.z +
                          CAge + CAge2 +
                           (1|ID),
                        # BirthYear removed as singular
                        data = full_data_adult)

summary(mod_early_adult)

## to have the same as the lambs but not needed any more
mod_early_adult_ALT <- lmer(delta_age ~ birthWeight_resid.z + singleton + VillTotal_womb.z + mean_fec.z +
                              CAge + CAge2 +
                              (1|BirthYear)  + (1|ID),
                            data = full_data_adult)

summary(mod_early_adult_ALT)

##### Yearlings only ####
## this is the same as the adult model since they have experienced the same early life
mod_early_yearling <- lmer(delta_age  ~ NAO.z + birthWeight_resid.z + singleton + VillTotal_birth.z + VillTotal_womb.z + mean_fec.z +
                             (1|MumID) , 
                           # no ID random effect because there is only one obs per individual
                           # singular with birth year so changed to mum ID
                           data = full_data_yearling)

summary(mod_early_yearling)


##### Developing 0-1 sheep ####
## not using this version, breaking down to lambs, yearlings and adults
##so including the lambs but removing first winter NAO to see what difference it makes

mod_early_DEVELOP <- lmer(delta_age ~ birthWeight_resid.z + singleton  + VillTotal_womb.z + mean_fec.z +
                            CAge + # dropped CAge2 since it is the same at this age
                            (1|ID), #singular so dropped birth year
                          data = (full_data_ALL %>% filter(CAge <2))) 

summary(mod_early_DEVELOP)

##### Lambs (0 years) #####
# reduced to the ELA that a lamb has actually experienced
## have removed CAge and (1|ID) since there is only one record each
# vill total birth still included since super high correlation between oct and aug
mod_early_lambs <- lmer(delta_age ~ birthWeight_resid.z + singleton + VillTotal_birth.z + VillTotal_womb.z + mean_fec.z +
                          (1|BirthYear),
                        data = full_data_lambs)

summary(mod_early_lambs)




#### Plotting ####
##### Full age range #####
predicted_early_all <- predict_response(mod_early_all, terms = "birthWeight_resid.z [all]")

PLOT_earlyAll_birthWeight_m <- 
  ggplot() +
  geom_point(data = full_data_ALL, aes(x = birthWeight_resid.z, y = delta_age), alpha =0.5) +
  geom_line(data = predicted_early_all, aes(x = x, y = predicted),
            colour = "lightblue4") +
  geom_ribbon(data = predicted_early_all, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.4, fill = "lightblue4") +
  labs(
    x = "Birth Weight (Residuals)",
    y = "Delta Age",
  ) +
  theme_bw()

##### Adults only clock ####
predicted_early_adult <- predict_response(mod_early_adult, terms = "birthWeight_resid.z [all]")

PLOT_earlyAdult_birthWeight_m <-
  ggplot() +
  geom_point(data = full_data_adult, aes(x = birthWeight_resid.z, y = delta_age), alpha =0.5) +
  geom_line(data = predicted_early_adult, aes(x = x, y = predicted),
            colour = "lightblue4") +
  geom_ribbon(data = predicted_early_adult, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.4, fill = "lightblue4") +
  labs(
    x = "Birth Weight (Residuals)",
    y = "Delta Age",
  ) +
  theme_bw()


#### Repeatability of ∆Age ####
##### All age clock #####
mod_repeat_all <- rpt(delta_age ~ (1|ID), data = full_data_ALL, grname = "ID", datatype = "Gaussian", nboot = 1000)

##### Adult Clock
mod_repeat_adult <- rpt(delta_age ~ (1|ID), data = full_data_Adult, grname = "ID", datatype = "Gaussian", nboot = 1000)

##### testing just those with > 1 observation 
indObs <- (count(full_data_ALL, ID, name = "numObs"))
longitudinalIDs <- indObs %>% filter(numObs >1)

mod_repeat_all2 <- rpt(delta_age ~ (1|ID), data = full_data_ALL %>% filter(ID %in% longitudinalIDs$ID), grname = "ID", datatype = "Gaussian", nboot = 1000)
