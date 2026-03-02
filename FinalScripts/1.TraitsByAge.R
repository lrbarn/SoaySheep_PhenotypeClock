#### 37. Modelling and plotting each trait by age ####

#### Packages ####
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggeffects)
library(gridExtra)

#### Data ####
BirthPheno_recode <- read_csv("OutputData/TransformedData/BirthPheno_recode.csv")


##### Female Data #####
female_sheet <- read_csv("OutputData/TransformedData/female_sheet_z_NEW.csv")
female_behaviour <- read_csv("OutputData/TransformedData/female_behaviour_zNEW.csv")
female_sample_z <- read_csv("OutputData/TransformedData/female_sample_z_NEW.csv")

# reformatting behaviour
female_behaviour <- female_behaviour %>% 
  mutate(ID = id,
         CapYear = year) %>% 
  select(-id, -year)


##### Combining and dropping incomplete #####
female_data <- full_join(female_sheet, female_behaviour)
female_data <- full_join(female_data, female_sample_z)

female_clockData <- left_join(female_data, BirthPheno_recode, by = "ID")

female_clockData <- female_clockData %>% 
  mutate(AgeY = CapYear - BirthYear) %>% 
  select(ID, AgeY, CapYear,
         # sheet data
         UnshedWool_recode, Scouring_recode, TeethDeform_recode, Milk_recode, 
         Weight.z, ForeLeg.z, Teeth.z, Keds.z,
         # behaviour data
         home.range.size.z,
         ff.deg.z, ff.stre.z, ff.eigen.z, ff.clust.z, ff.betw.z, ff.close.z, ff.part.coef.z, ff.mean.stre.z, ff.mean.gs.z, ff.soc.sel.z,
         
         os.deg.z, os.stre.z, os.mean.stre.z, os.soc.sel.z,
         
         # sample data
         IgA.z, IgE.z, IgG.z,
         RTL.z,
         Strongyles.z, Coccidea.z)

#### Male Data ####

male_sheet <- read_csv("OutputData/TransformedData/male_sheet_z_NEW2.csv")
male_behaviour <- read_csv("OutputData/TransformedData/male_behaviour_zNEW.csv")
male_sample_z <- read_csv("OutputData/TransformedData/male_sample_z_NEW.csv")

## reformatting behaviour
male_behaviour <- male_behaviour %>% 
  mutate(ID = id,
         CapYear = year) %>% 
  select(-id, -year)

#### Combining and dropping incomplete ####
male_data <- full_join(male_sheet, male_behaviour)
male_data <- full_join(male_data, male_sample_z)

male_clockData <- left_join(male_data, BirthPheno_recode, by = "ID")

male_clockData <- male_clockData %>% 
  mutate(AgeY = CapYear - BirthYear) %>% 
  select(ID, AgeY, CapYear,
         # sheet data
         UnshedWool_recode, Scouring_recode, TeethDeform_recode, BrokenHorns_recode, 
         Weight.z, ForeLeg.z, Teeth.z, Keds.z, HornLen.z, HornCirc.z, BolCirc.z, BolLen.z,
         # behaviour data
         home.range.size.z,
         mm.deg.z, mm.stre.z, mm.eigen.z, mm.clust.z, mm.betw.z, mm.close.z, mm.part.coef.z, mm.mean.stre.z, mm.mean.gs.z, mm.soc.sel.z, 
         
         os.deg.z, os.stre.z, os.mean.stre.z, os.soc.sel.z,
         
         # sample data
         IgA.z, IgE.z, IgG.z,
         RTL.z,
         Strongyles.z, Coccidea.z)

## Cleanup 
rm(BirthPheno_recode, female_behaviour, female_data, female_sample_z, female_sheet,
   male_behaviour, male_data, male_sample_z, male_sheet)

#### Adding in Age Last Measure ####
female_clockData <- female_clockData %>% 
  group_by(ID) %>% 
  mutate(ALM = last(AgeY)) %>% 
  ungroup()

female_clockData <- female_clockData %>% 
  mutate(AgeY2 = AgeY^2,
         Sex = "Female")

male_clockData <- male_clockData %>% 
  group_by(ID) %>% 
  mutate(ALM = last(AgeY)) %>% 
  ungroup()

male_clockData <- male_clockData %>% 
  mutate(AgeY2 = AgeY^2,
         Sex = "Male")



## combined data
all_data <- full_join(female_clockData, male_clockData)



#### Models and Plots ####
##### Unshed Wool ####
mod_unshed_f <- glmer(UnshedWool_recode ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID), data = female_clockData,
                     family = binomial(link = "logit"))

mod_unshed_m <- glmer(UnshedWool_recode ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = male_clockData,
                      family = binomial(link = "logit"))

## still failing to converge
mod_unshed_m_ALT  <- glmer(UnshedWool_recode ~ AgeY + ALM +
                             (1|CapYear), data = male_clockData,
                           family = binomial(link = "logit"))

### Plot
PLOT_unshed <- 
ggplot(data = all_data, aes(x = AgeY, y = UnshedWool_recode, fill = Sex, colour = Sex)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()



##### Scouring #####
mod_scouring_f <- glmer(Scouring_recode ~ AgeY + AgeY2 + ALM +
                         (1|CapYear) + (1|ID), data = female_clockData,
                       family = binomial(link = "logit"))

mod_scouring_m <- glmer(Scouring_recode ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID), data = male_clockData,
                        family = binomial(link = "logit"))

# removed cap year as well as age2 -> still failing to converge
mod_scouring_m_ALT <- glmer(Scouring_recode ~ AgeY + ALM +
                               (1|ID), data = male_clockData,
                            family = binomial(link = "logit"))

PLOT_scouring <- 
  ggplot(data = all_data, aes(x = AgeY, y = Scouring_recode, fill = Sex, colour = Sex)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Milk #####

## fails to converge
mod_milk <- glmer(Milk_recode ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID), data = female_clockData, 
                  family = binomial(link = "logit"))

PLOT_milk <- 
  ggplot(data = all_data, aes(x = AgeY, y = Milk_recode, fill = Sex, colour = Sex)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()

##### Deformities #####
mod_deform_f <- glmer(TeethDeform_recode ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = female_clockData, 
                      family = binomial(link = "logit"))

mod_deform_m <- glmer(TeethDeform_recode ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = male_clockData, 
                      family = binomial(link = "logit"))

PLOT_deform <- 
  ggplot(data = all_data, aes(x = AgeY, y = TeethDeform_recode, fill = Sex, colour = Sex)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()



##### Broken Horns #####
mod_broken_m <- glmer(BrokenHorns_recode ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = male_clockData, 
                      family = binomial(link = "logit"))
## dropping the squared age
mod_broken_m_ALT <- glmer(BrokenHorns_recode ~ AgeY + ALM +
                            (1|CapYear) + (1|ID), data = male_clockData, 
                          family = binomial(link = "logit"))

## plot
PLOT_broken <- 
  ggplot(data = all_data, aes(x = AgeY, y = BrokenHorns_recode, fill = Sex, colour = Sex)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Weight #####
mod_weight_f <- lmer(Weight.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID), data = female_clockData)

mod_weight_m <- lmer(Weight.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID), data = male_clockData)

PLOT_weight <- 
  ggplot(data = all_data, aes(x = AgeY, y = Weight.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()



##### Foreleg #####
mod_foreleg_f <- lmer(ForeLeg.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = female_clockData)

mod_foreleg_m <- lmer(ForeLeg.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = male_clockData)

PLOT_foreLeg <- 
  ggplot(data = all_data, aes(x = AgeY, y = ForeLeg.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Ked Count ####
mod_ked_f <- lmer(Keds.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID), data = female_clockData)

mod_ked_m <- lmer(Keds.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID), data = male_clockData)

mod_ked_m_ALT <- lmer(Keds.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear), data = male_clockData)

PLOT_keds <- 
  ggplot(data = all_data, aes(x = AgeY, y = Keds.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Incisor Count #####
mod_teeth_f <- lmer(Teeth.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID), data = female_clockData)

mod_teeth_m <- lmer(Teeth.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID), data = male_clockData)

mod_teeth_m_ALT <- lmer(Teeth.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear), data = male_clockData)

PLOT_teeth <- 
  ggplot(data = all_data, aes(x = AgeY, y = Teeth.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()



##### Horn Length #####
mod_hornLen_m <- lmer(HornLen.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = male_clockData)

PLOT_hornLen <- 
  ggplot(data = all_data, aes(x = AgeY, y = HornLen.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Horn Circ #####
mod_hornCirc_m <- lmer(HornCirc.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = male_clockData)

PLOT_hornCirc <- 
  ggplot(data = all_data, aes(x = AgeY, y = HornCirc.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Bolcirc ####
mod_bolCirc_m <- lmer(BolCirc.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = male_clockData)

PLOT_bolCirc <- 
  ggplot(data = all_data, aes(x = AgeY, y = BolCirc.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### BolLen ####
mod_bolLen_m <- lmer(BolLen.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID), data = male_clockData)

PLOT_bolLen <- 
  ggplot(data = all_data, aes(x = AgeY, y = BolLen.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Strongyles #####
mod_strongyles_f <- lmer(Strongyles.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = female_clockData)

mod_strongyles_m <- lmer(Strongyles.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID), data = male_clockData)

PLOT_strongyles <- 
  ggplot(data = all_data, aes(x = AgeY, y = Strongyles.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()



##### Coccidia #####
mod_coccidea_f <- lmer(Coccidea.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID), data = female_clockData)

mod_coccidea_m <- lmer(Coccidea.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = male_clockData)


PLOT_coccidea <- 
  ggplot(data = all_data, aes(x = AgeY, y = Coccidea.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()

##### IgA #####
mod_iga_f <- lmer(IgA.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID), data = female_clockData)

mod_iga_m <- lmer(IgA.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID), data = male_clockData)

PLOT_iga <- 
  ggplot(data = all_data, aes(x = AgeY, y = IgA.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()

##### IgE #####
mod_ige_f <- lmer(IgE.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID), data = female_clockData)

mod_ige_m <- lmer(IgE.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID), data = male_clockData)

PLOT_ige <- 
  ggplot(data = all_data, aes(x = AgeY, y = IgE.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()

##### IgG #####
mod_igg_f <- lmer(IgE.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID), data = female_clockData)

mod_igg_m <- lmer(IgE.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID), data = male_clockData)

PLOT_igg <- 
  ggplot(data = all_data, aes(x = AgeY, y = IgG.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()

##### RTL #####
mod_rtl_f <- lmer(RTL.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID), data = female_clockData)

mod_rtl_m <- lmer(RTL.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID), data = male_clockData)
## dropping age2 from the male model
mod_rtl_m_ALT <- lmer(RTL.z ~ AgeY + ALM +
                        (1|CapYear) + (1|ID), data = male_clockData)

PLOT_RTL <- 
  ggplot(data = all_data, aes(x = AgeY, y = RTL.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Home Range #####
mod_home_f <- lmer(home.range.size.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID), data = female_clockData)
## drop the age2
mod_home_f_ALT <- lmer(home.range.size.z ~ AgeY + ALM +
                     (1|CapYear) + (1|ID), data = female_clockData)


mod_home_m <- lmer(home.range.size.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID), data = male_clockData)

## drop the age2
mod_home_m_ALT <- lmer(home.range.size.z ~ AgeY + ALM +
                     (1|CapYear) + (1|ID), data = male_clockData)

PLOT_home <- 
  ggplot(data = all_data, aes(x = AgeY, y = home.range.size.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Degree -ss ####
mod_deg.ff <- lmer(ff.deg.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID), data = female_clockData)

mod_deg.ff_alt <- lmer(ff.deg.z ~ AgeY + ALM +
                         (1|CapYear) + (1|ID), data = female_clockData)


mod_deg.mm <- lmer(mm.deg.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID), data = male_clockData)

PLOT_degree.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = ff.deg.z, fill = Sex, colour = Sex)) +
               geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = mm.deg.z, fill = Sex, colour = Sex)) +    
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = ff.deg.z, fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = mm.deg.z, fill = Sex, colour = Sex)) +
  ylab("ss.degree.z") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Degree - os ####
mod_deg_os_f <- lmer(os.deg.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID), data = female_clockData)

## drop age2
mod_deg_os_f_ALT <- lmer(os.deg.z ~ AgeY + ALM +
                           (1|CapYear) + (1|ID), data = female_clockData)

mod_deg_os_m <- lmer(os.deg.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID), data = male_clockData)

PLOT_degree.os <- 
  ggplot(data = all_data, aes(x = AgeY, y = os.deg.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()

##### Strength - ss ####
mod_stren.ff <- lmer(ff.stre.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID), data = female_clockData)

mod_stren.mm <- lmer(mm.stre.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID), data = male_clockData)

PLOT_stren.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = ff.stre.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = mm.stre.z, fill = Sex, colour = Sex)) +    
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = ff.stre.z, fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = mm.stre.z, fill = Sex, colour = Sex)) +
  ylab("ss.stre.z") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()



##### Strength -os #####
mod_stre_os_f <- lmer(os.stre.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = female_clockData)

mod_stre_os_f_ALT <- lmer(os.stre.z ~ AgeY + ALM +
                            (1|CapYear) + (1|ID), data = female_clockData)


mod_stre_os_m <- lmer(os.stre.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = male_clockData)

PLOT_stre.os <- 
  ggplot(data = all_data, aes(x = AgeY, y = os.stre.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Participation - ss #####
mod_partcoef.ff <- lmer(ff.part.coef.z ~ AgeY + AgeY2 + ALM +
                           (1|CapYear) + (1|ID), data = female_clockData)
## drop squared
mod_partcoef.ff_ALT <- lmer(ff.part.coef.z ~ AgeY + ALM +
                              (1|CapYear) + (1|ID), data = female_clockData)


mod_partcoef.mm <- lmer(mm.part.coef.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID), data = male_clockData)
## drop squared
mod_partcoef.mm_ALT <- lmer(mm.part.coef.z ~ AgeY + ALM +
                              (1|CapYear) + (1|ID), data = male_clockData)

PLOT_partcoef.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = ff.part.coef.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = mm.part.coef.z, fill = Sex, colour = Sex)) +    
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = ff.part.coef.z, fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = mm.part.coef.z, fill = Sex, colour = Sex)) +
  ylab("ss.part.coef.z") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()

##### Mean Strength - ss #####
mod_meanStre.ff <- lmer(ff.mean.stre.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID), data = female_clockData)

mod_meanStre.mm <- lmer(mm.mean.stre.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID), data = male_clockData)

PLOT_meanStre.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = ff.mean.stre.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = mm.mean.stre.z, fill = Sex, colour = Sex)) +    
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = ff.mean.stre.z, fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = mm.mean.stre.z, fill = Sex, colour = Sex)) +
  ylab("ss.part.coef.z") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()



##### Mean Strength - os #####
mod_meanStre_os_f <- lmer(os.mean.stre.z ~ AgeY + AgeY2 + ALM +
                            (1|CapYear) + (1|ID), data = female_clockData)
## drop age2
mod_meanStre_os_f_ALT <- lmer(os.mean.stre.z ~ AgeY + ALM +
                                (1|CapYear) + (1|ID), data = female_clockData)


mod_meanStre_os_m <- lmer(os.mean.stre.z ~ AgeY + AgeY2 + ALM +
                            (1|CapYear) + (1|ID), data = male_clockData)

PLOT_mean.stre.os <- 
  ggplot(data = all_data, aes(x = AgeY, y = os.mean.stre.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()

##### social selectivity - ss #####
mod_socsel.ff <- lmer(ff.soc.sel.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = female_clockData)

mod_socsel.mm <- lmer(mm.soc.sel.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID), data = male_clockData)

PLOT_socsel.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = ff.soc.sel.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = mm.soc.sel.z, fill = Sex, colour = Sex)) +    
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = ff.soc.sel.z, fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = mm.soc.sel.z, fill = Sex, colour = Sex)) +
  ylab("ss.soc.sel.z") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### social selectivity - os #####
mod_socsel_os_f <- lmer(os.soc.sel.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID), data = female_clockData)

mod_socsel_os_f_ALT <- lmer(os.soc.sel.z ~ AgeY + ALM +
                              (1|CapYear) + (1|ID), data = female_clockData)

mod_socsel_os_m <- lmer(os.soc.sel.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID), data = male_clockData)

PLOT_soc.sel.os <- 
  ggplot(data = all_data, aes(x = AgeY, y = os.soc.sel.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### eigen ####
mod_eigen_f <- lmer(ff.eigen.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID), data = female_clockData)

mod_eigen_m <- lmer(mm.eigen.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID), data = male_clockData)

PLOT_eigen.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = ff.eigen.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = mm.eigen.z, fill = Sex, colour = Sex)) +    
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = ff.eigen.z, fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = mm.eigen.z, fill = Sex, colour = Sex)) +
  ylab("ss.eigen.z") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()



##### betweenness #####
mod_betw_f <- lmer(ff.betw.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID), data = female_clockData)

mod_betw_f_ALT <- lmer(ff.betw.z ~ AgeY + ALM +
                         (1|CapYear) + (1|ID), data = female_clockData)

mod_betw_m <- lmer(mm.betw.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID), data = male_clockData)

PLOT_betw.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = ff.betw.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = mm.betw.z, fill = Sex, colour = Sex)) +    
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = ff.betw.z, fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = mm.betw.z, fill = Sex, colour = Sex)) +
  ylab("ss.betw.z") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Closeness #####
mod_close_f <- lmer(ff.close.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID), data = female_clockData)

mod_close_f_ALT <- lmer(ff.close.z ~ AgeY + ALM +
                          (1|CapYear) + (1|ID), data = female_clockData)


mod_close_m <- lmer(mm.close.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID), data = male_clockData)

PLOT_close.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = ff.close.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = mm.close.z, fill = Sex, colour = Sex)) +    
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = ff.close.z, fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = mm.close.z, fill = Sex, colour = Sex)) +
  ylab("ss.close.z") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


##### Clustering #####
mod_clust_f <- lmer(ff.clust.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID), data = female_clockData)

mod_clust_f_ALT <- lmer(ff.clust.z ~ AgeY + ALM +
                          (1|CapYear) + (1|ID), data = female_clockData)

mod_clust_m <- lmer(mm.clust.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID), data = male_clockData)

PLOT_clust.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = ff.clust.z, fill = Sex, colour = Sex)) +
  geom_point(alpha = 0.5, data = all_data, aes(x = AgeY, y = mm.clust.z, fill = Sex, colour = Sex)) +    
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = ff.clust.z, fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, aes(x = AgeY, y = mm.clust.z, fill = Sex, colour = Sex)) +
  ylab("ss.clust.z") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",                                 "Male" = "lightblue4")) +     theme_bw()


#### Panels of Plots ####
plot_names <- ls(pattern = "^PLOT_")

# 2. Get the actual objects into a list
plot_list <- mget(plot_names)

# 3. Combine them into one big panel
# 'ncol' or 'nrow' can be used to control the grid shape
combined_panel <- patchwork::wrap_plots(plot_list) +
  patchwork::plot_layout(axis_titles = "collect", guides = "collect") 
ggsave("AgeByTrait.png", combined_panel, height=7.5, width=12)
