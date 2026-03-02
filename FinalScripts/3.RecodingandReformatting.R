#### 3. Data Formatting ####
## All the data going into the phenotype needs to be mean centred and standardised
## additionally, some need recoding so that they are in more binary format

#### Set Up ####
library(tidyverse)

#### Phenotype Recoding ####
## Morph ##
# recoding the usual 1-4 morph type to be coat colour and type
# dark becomes 0 and light becomes 1
# wild becomes 0 and self becomes 1
BirthPheno <- read_csv("RawData/SheetData/PhenotypeClock_sheet_birthPheno.csv")


## these are fixed throughout lifetime so actually NOT to be included in the sheet clocks since they are predicting probability of capturing/ survival rather than age
BirthPheno <- BirthPheno %>% 
  mutate( 
    colour = factor(case_when(Coat == "1" ~ 0,
                              Coat == "2" ~ 0,
                              Coat == "3" ~ 1,
                              Coat == "4" ~ 1,
                              Coat == NA ~ NA
                              )),
    pattern = factor(case_when(Coat == "1" ~ 0,
                               Coat == "2" ~ 1,
                               Coat == "3" ~ 0,
                               Coat == "4" ~ 1,
                               Coat == NA ~ NA
                               )),
    horn_recode = factor(case_when(Sex == 1 & Horn == 2 ~ 0,
                            Sex == 1 & Horn == 3 ~ 1,
                            Sex == 1 & Horn == 1 ~ 0.5,
                            Sex == 2 & Horn == 1 ~ 1,
                            Sex == 2 & Horn == 3 ~ 0,
                            Horn == NA ~ NA
                            ))
  )

BirthPheno_recode <- BirthPheno %>% 
  select(-Coat, -Horn)


female_sheet <- read_csv("RawData/SheetData/PhenotypeClock_sheet_females.csv")

female_sheet <- female_sheet %>%
  mutate(
    UnshedWool_recode = factor(case_when(UnshedWool == "N" ~ 0,
                                         UnshedWool == "S" ~ 0,
                                         UnshedWool == "1" ~ 0,
                                         UnshedWool == "2" ~ 0,
                                         UnshedWool == "3" ~ 0,
                                         UnshedWool == "Y" ~ 1,
                                         UnshedWool == "4" ~ 1,
                                         UnshedWool == "5" ~ 1,
                                         UnshedWool == "6" ~ 1,
                                         UnshedWool == NA ~ NA)),
    Scouring_recode = factor(case_when(Scouring == "N" ~ 0,
                                       Scouring == "P" ~ 1,
                                       Scouring == "Y" ~ 1,
                                       Scouring == NA ~ NA)),
    Milk_recode = factor(case_when(Milk == "N" ~ 0,
                                   Milk == "P"~ 1,
                                   Milk == "Y" ~ 1)),
    TeethDeform_recode = factor(case_when(TeethDeform == "0" ~ 0,
                                          TeethDeform == "-1" ~ 1))
  )

male_sheet <- read_csv("RawData/SheetData/PhenotypeClock_sheet_males.csv")

male_sheet <- male_sheet %>%
  mutate(
    BrokenHorns_recode = factor(case_when((LHBroken + RHBroken) == "0" ~ 0,
                                   (LHBroken + RHBroken) == "-1" ~ 1,
                                   (LHBroken + RHBroken) == "-2" ~ 1,
                                   (LHBroken + RHBroken) == NA ~ NA)),
    UnshedWool_recode = factor(case_when(UnshedWool == "N" ~ 0,
                                         UnshedWool == "S" ~ 0,
                                         UnshedWool == "1" ~ 0,
                                         UnshedWool == "2" ~ 0,
                                         UnshedWool == "3" ~ 0,
                                         UnshedWool == "Y" ~ 1,
                                         UnshedWool == "4" ~ 1,
                                         UnshedWool == "5" ~ 1,
                                         UnshedWool == "6" ~ 1,
                                         UnshedWool == NA ~ NA)),
    Scouring_recode = factor(case_when(Scouring == "N" ~ 0,
                                       Scouring == "P" ~ 1,
                                       Scouring == "Y" ~ 1,
                                       Scouring == NA ~ NA)),
    TeethDeform_recode = factor(case_when(TeethDeform == "0" ~ 0,
                                          TeethDeform == "-1" ~ 1))
  )

#### Mean Centering ####

##### Females #####
female_sheet_means <- female_sheet %>% 
  select(Weight, ForeLeg, Keds, Teeth) %>% 
  summarise(mean_Weight = mean(Weight, na.rm = TRUE),
          mean_ForeLeg = mean(ForeLeg, na.rm = TRUE),
          mean_Keds = mean(Keds, na.rm = TRUE),
          mean_Teeth = mean(Teeth, na.rm = TRUE),
          sd_Weight = sd(Weight, na.rm = TRUE),
          sd_Foreleg = sd(ForeLeg, na.rm = TRUE),
          sd_Keds = sd(Keds, na.rm = TRUE),
          sd_Teeth = sd(Teeth, na.rm = TRUE))

female_sheet <- female_sheet %>% 
  mutate(
    Weight_centered = Weight - female_sheet_means$mean_Weight,
    ForeLeg_centered = ForeLeg - female_sheet_means$mean_ForeLeg,
    Teeth_centered = Teeth - female_sheet_means$mean_Teeth,
    Keds_centered = Keds - female_sheet_means$mean_Keds)

female_sheet <- female_sheet %>% 
  mutate(
    Weight.z = Weight_centered / female_sheet_means$sd_Weight,
    ForeLeg.z = ForeLeg_centered / female_sheet_means$sd_Foreleg,
    Teeth.z = Teeth_centered / female_sheet_means$sd_Teeth,
    Keds.z = Keds_centered / female_sheet_means$sd_Keds
  )

female_sheet_z <- female_sheet %>% 
  select(ID, Sex, CapRef, CapYear, CapMonth, CapDay, UnshedWool_recode, Scouring_recode, TeethDeform_recode, Milk_recode, Weight.z, ForeLeg.z, Teeth.z, Keds.z)

##### Males #####

male_sheet_means <- male_sheet %>% 
  select(Weight, ForeLeg, Keds, Teeth, HornLen, HornCirc, BolCirc, BolLen) %>% 
  summarise(mean_Weight = mean(Weight, na.rm = TRUE),
            mean_ForeLeg = mean(ForeLeg, na.rm = TRUE),
            mean_Keds = mean(Keds, na.rm = TRUE),
            mean_Teeth = mean(Teeth, na.rm = TRUE),
            mean_HornLen = mean(HornLen, na.rm = TRUE),
            mean_HornCirc = mean(HornCirc, na.rm = TRUE),
            mean_BolCirc = mean(BolCirc, na.rm = TRUE),
            mean_BolLen = mean(BolLen, na.rm = TRUE),
            
            sd_Weight = sd(Weight, na.rm = TRUE),
            sd_Foreleg = sd(ForeLeg, na.rm = TRUE),
            sd_Keds = sd(Keds, na.rm = TRUE),
            sd_Teeth = sd(Teeth, na.rm = TRUE),
            sd_HornLen = sd(HornLen, na.rm = TRUE),
            sd_HornCirc = sd(HornCirc, na.rm = TRUE),
            sd_BolCirc = sd(BolCirc, na.rm = TRUE),
            sd_BolLen = sd(BolLen, na.rm = TRUE))


male_sheet <- male_sheet %>% 
  mutate(
    Weight_centered = Weight - male_sheet_means$mean_Weight,
    ForeLeg_centered = ForeLeg - male_sheet_means$mean_ForeLeg,
    Teeth_centered = Teeth - male_sheet_means$mean_Teeth,
    Keds_centered = Keds - male_sheet_means$mean_Keds,
    HornLen_centered = HornLen - male_sheet_means$mean_HornLen,
    HornCirc_centered = HornCirc - male_sheet_means$mean_HornCirc,
    BolCirc_centered = BolCirc - male_sheet_means$mean_BolCirc,
    BolLen_centered = BolLen - male_sheet_means$mean_BolLen
    )

male_sheet <- male_sheet %>% 
  mutate(
    Weight.z = Weight_centered / male_sheet_means$sd_Weight,
    ForeLeg.z = ForeLeg_centered / male_sheet_means$sd_Foreleg,
    Teeth.z = Teeth_centered / male_sheet_means$sd_Teeth,
    Keds.z = Keds_centered / male_sheet_means$sd_Keds,
    HornLen.z = HornLen_centered / male_sheet_means$sd_HornLen,
    HornCirc.z = HornCirc_centered / male_sheet_means$sd_HornCirc,
    BolCirc.z = BolCirc_centered / male_sheet_means$sd_BolCirc,
    BolLen.z = BolLen_centered / male_sheet_means$sd_BolLen
  )

male_sheet_z <- male_sheet %>% 
  select(ID, Sex, CapRef, CapYear, CapMonth, CapDay, UnshedWool_recode, Scouring_recode, TeethDeform_recode, Weight.z, ForeLeg.z, Teeth.z, Keds.z, HornLen.z, HornCirc.z, BolCirc.z, BolLen.z, BrokenHorns_recode)




# #### Behaviour Mean Centring - FEMALES ####
# ## NOTE THIS IS THE OLD VERSION, SEE NEW BELOW 7/11/25
# 
# behaviourData <- read_csv("RawData/BehaviourData/sheep_behaviouraldata_frailtyclock.csv")
# 
# female_behaviour <- behaviourData %>% 
#   filter(sex == 1) %>% 
#   select(year, id, sex, age, home.range.size, 
#          # female-female interactions
#          ff.deg, ff.stre, ff.eigen, ff.clust, ff.betw, ff.close, ff.part.coef, ff.mean.stre, ff.mean.gs, ff.soc.sel, ff.top3.stre, 
#          # female-male interactions
#          fm.deg, fm.stre, fm.eigen, fm.clust, fm.betw, fm.close, fm.part.coef, fm.mean.stre, fm.mean.gs, fm.soc.sel, fm.top3.stre)
# 
# 
# female_behaviour_means <- female_behaviour %>% 
#   select(home.range.size, 
#          # female-female interactions
#          ff.deg, ff.stre, ff.eigen, ff.clust, ff.betw, ff.close, ff.part.coef, ff.mean.stre, ff.mean.gs, ff.soc.sel, ff.top3.stre, 
#          # female-male interactions
#          fm.deg, fm.stre, fm.eigen, fm.clust, fm.betw, fm.close, fm.part.coef, fm.mean.stre, fm.mean.gs, fm.soc.sel, fm.top3.stre) %>% 
#   summarise(home.range.size_mean = mean(home.range.size, na.rm = TRUE),
#             home.range.size_sd = sd(home.range.size, na.rm = TRUE),
#             
#             
#             ff.deg_mean = mean(ff.deg, na.rm = TRUE),
#             ff.stre_mean = mean(ff.stre, na.rm = TRUE),
#             ff.eigen_mean = mean(ff.eigen, na.rm = TRUE),
#             ff.clust_mean = mean(ff.clust, na.rm = TRUE),
#             ff.betw_mean = mean(ff.betw, na.rm = TRUE),
#             ff.close_mean = mean(ff.close, na.rm = TRUE),
#             ff.part.coef_mean = mean(ff.part.coef, na.rm = TRUE),
#             ff.mean.stre_mean = mean(ff.mean.stre, na.rm = TRUE),
#             ff.mean.gs_mean = mean(ff.mean.gs, na.rm = TRUE),
#             ff.soc.sel_mean = mean(ff.soc.sel, na.rm = TRUE),
#             ff.top3.stre_mean = mean(ff.top3.stre, na.rm = TRUE),
#             
#             fm.deg_mean = mean(fm.deg, na.rm = TRUE),
#             fm.stre_mean = mean(fm.stre, na.rm = TRUE),
#             fm.eigen_mean = mean(fm.eigen, na.rm = TRUE),
#             fm.clust_mean = mean(fm.clust, na.rm = TRUE),
#             fm.betw_mean = mean(fm.betw, na.rm = TRUE),
#             fm.close_mean = mean(fm.close, na.rm = TRUE),
#             fm.part.coef_mean = mean(fm.part.coef, na.rm = TRUE),
#             fm.mean.stre_mean = mean(fm.mean.stre, na.rm = TRUE),
#             fm.mean.gs_mean = mean(fm.mean.gs, na.rm = TRUE),
#             fm.soc.sel_mean = mean(fm.soc.sel, na.rm = TRUE),
#             fm.top3.stre_mean = mean(fm.top3.stre, na.rm = TRUE),
#             
#             
#             ff.deg_sd = sd(ff.deg, na.rm = TRUE),
#             ff.stre_sd = sd(ff.stre, na.rm = TRUE),
#             ff.eigen_sd = sd(ff.eigen, na.rm = TRUE),
#             ff.clust_sd = sd(ff.clust, na.rm = TRUE),
#             ff.betw_sd = sd(ff.betw, na.rm = TRUE),
#             ff.close_sd = sd(ff.close, na.rm = TRUE),
#             ff.part.coef_sd = sd(ff.part.coef, na.rm = TRUE),
#             ff.mean.stre_sd = sd(ff.mean.stre, na.rm = TRUE),
#             ff.mean.gs_sd = sd(ff.mean.gs, na.rm = TRUE),
#             ff.soc.sel_sd = sd(ff.soc.sel, na.rm = TRUE),
#             ff.top3.stre_sd = sd(ff.top3.stre, na.rm = TRUE),
#             
#             fm.deg_sd = sd(fm.deg, na.rm = TRUE),
#             fm.stre_sd = sd(fm.stre, na.rm = TRUE),
#             fm.eigen_sd = sd(fm.eigen, na.rm = TRUE),
#             fm.clust_sd = sd(fm.clust, na.rm = TRUE),
#             fm.betw_sd = sd(fm.betw, na.rm = TRUE),
#             fm.close_sd = sd(fm.close, na.rm = TRUE),
#             fm.part.coef_sd = sd(fm.part.coef, na.rm = TRUE),
#             fm.mean.stre_sd = sd(fm.mean.stre, na.rm = TRUE),
#             fm.mean.gs_sd = sd(fm.mean.gs, na.rm = TRUE),
#             fm.soc.sel_sd = sd(fm.soc.sel, na.rm = TRUE),
#             fm.top3.stre_sd = sd(fm.top3.stre, na.rm = TRUE))
# 
# #### CENTERING ###
# female_behaviour <- female_behaviour %>% 
#   mutate(
#     home.range.size_centered = home.range.size - female_behaviour_means$home.range.size_mean,
#     
#     ff.deg_centered = ff.deg - female_behaviour_means$ff.deg_mean,
#     ff.stre_centered = ff.stre - female_behaviour_means$ff.stre_mean,
#     ff.eigen_centered = ff.eigen - female_behaviour_means$ff.eigen_mean,
#     ff.clust_centered = ff.clust - female_behaviour_means$ff.clust_mean,
#     ff.betw_centered = ff.betw - female_behaviour_means$ff.betw_mean,
#     ff.close_centered = ff.close - female_behaviour_means$ff.close_mean,
#     ff.part.coef_centered = ff.part.coef - female_behaviour_means$ff.part.coef_mean,
#     ff.mean.stre_centered = ff.stre - female_behaviour_means$ff.stre_mean,
#     ff.mean.gs_centered = ff.mean.gs - female_behaviour_means$ff.mean.gs_mean,
#     ff.soc.sel_centered = ff.soc.sel - female_behaviour_means$ff.soc.sel_mean,
#     ff.top3.stre_centered = ff.top3.stre - female_behaviour_means$ff.top3.stre_mean,
#     
#     fm.deg_centered = fm.deg - female_behaviour_means$fm.deg_mean,
#     fm.stre_centered = fm.stre - female_behaviour_means$fm.stre_mean,
#     fm.eigen_centered = fm.eigen - female_behaviour_means$fm.eigen_mean,
#     fm.clust_centered = fm.clust - female_behaviour_means$fm.clust_mean,
#     fm.betw_centered = fm.betw - female_behaviour_means$fm.betw_mean,
#     fm.close_centered = fm.close - female_behaviour_means$fm.close_mean,
#     fm.part.coef_centered = fm.part.coef - female_behaviour_means$fm.part.coef_mean,
#     fm.mean.stre_centered = fm.stre - female_behaviour_means$fm.stre_mean,
#     fm.mean.gs_centered = fm.mean.gs - female_behaviour_means$fm.mean.gs_mean,
#     fm.soc.sel_centered = fm.soc.sel - female_behaviour_means$fm.soc.sel_mean,
#     fm.top3.stre_centered = fm.top3.stre - female_behaviour_means$fm.top3.stre_mean
#   )
# 
# 
# 
# #### STANDARDISING 
# female_behaviour <- female_behaviour %>% 
#   mutate(
#     home.range.size.z = home.range.size_centered / female_behaviour_means$home.range.size_sd,
#     
#     ff.deg.z = ff.deg_centered / female_behaviour_means$ff.deg_sd,
#     ff.stre.z = ff.stre_centered / female_behaviour_means$ff.stre_sd,
#     ff.eigen.z = ff.eigen_centered / female_behaviour_means$ff.eigen_sd,
#     ff.clust.z = ff.clust_centered / female_behaviour_means$ff.clust_sd,
#     ff.betw.z = ff.betw_centered / female_behaviour_means$ff.betw_sd,
#     ff.close.z = ff.close_centered / female_behaviour_means$ff.close_sd,
#     ff.part.coef.z = ff.part.coef_centered / female_behaviour_means$ff.part.coef_sd,
#     ff.mean.stre.z = ff.stre_centered / female_behaviour_means$ff.stre_sd,
#     ff.mean.gs.z = ff.mean.gs_centered / female_behaviour_means$ff.mean.gs_sd,
#     ff.soc.sel.z = ff.soc.sel_centered / female_behaviour_means$ff.soc.sel_sd,
#     ff.top3.stre.z = ff.top3.stre_centered / female_behaviour_means$ff.top3.stre_sd,
#     
#     fm.deg.z = fm.deg_centered / female_behaviour_means$fm.deg_sd,
#     fm.stre.z = fm.stre_centered / female_behaviour_means$fm.stre_sd,
#     fm.eigen.z = fm.eigen_centered / female_behaviour_means$fm.eigen_sd,
#     fm.clust.z = fm.clust_centered / female_behaviour_means$fm.clust_sd,
#     fm.betw.z = fm.betw_centered / female_behaviour_means$fm.betw_sd,
#     fm.close.z = fm.close_centered / female_behaviour_means$fm.close_sd,
#     fm.part.coef.z = fm.part.coef_centered / female_behaviour_means$fm.part.coef_sd,
#     fm.mean.stre.z = fm.stre_centered / female_behaviour_means$fm.stre_sd,
#     fm.mean.gs.z = fm.mean.gs_centered / female_behaviour_means$fm.mean.gs_sd,
#     fm.soc.sel.z = fm.soc.sel_centered / female_behaviour_means$fm.soc.sel_sd,
#     fm.top3.stre.z = fm.top3.stre_centered / female_behaviour_means$fm.top3.stre_sd
#   )
# 
# female_behaviour_z <- female_behaviour %>%
#   select(
#     id,
#     year,
#     home.range.size.z,
#     ff.deg.z, ff.stre.z, ff.eigen.z, ff.clust.z, ff.betw.z, ff.close.z, ff.part.coef.z, ff.mean.stre.z, ff.mean.gs.z, ff.soc.sel.z, ff.top3.stre.z,
#     
#     fm.deg.z, fm.stre.z, fm.eigen.z, fm.clust.z, fm.betw.z, fm.close.z, fm.part.coef.z, fm.mean.stre.z, fm.mean.gs.z, fm.soc.sel.z, fm.top3.stre.z
#     )
# 
# 
# #### Behaviour Mean Centering - MALES ####
# ## NOTE THIS IS THE OLD VERSION, SEE NEW BELOW 7/11/25
# 
# 
# behaviourData <- read_csv("RawData/BehaviourData/sheep_behaviouraldata_frailtyclock.csv")
# 
# male_behaviour <- behaviourData %>% 
#   filter(sex == 2) %>% 
#   select(year, id, sex, age, home.range.size, 
#          # male-male interactions
#          mm.deg, mm.stre, mm.eigen, mm.clust, mm.betw, mm.close, mm.part.coef, mm.mean.stre, mm.mean.gs, mm.soc.sel, mm.top3.stre, 
#          # female-male interactions
#          fm.deg, fm.stre, fm.eigen, fm.clust, fm.betw, fm.close, fm.part.coef, fm.mean.stre, fm.mean.gs, fm.soc.sel, fm.top3.stre)
# 
# 
# male_behaviour_means <- male_behaviour %>% 
#   select(home.range.size, 
#          # male-male interactions
#          mm.deg, mm.stre, mm.eigen, mm.clust, mm.betw, mm.close, mm.part.coef, mm.mean.stre, mm.mean.gs, mm.soc.sel, mm.top3.stre, 
#          # female-male interactions
#          fm.deg, fm.stre, fm.eigen, fm.clust, fm.betw, fm.close, fm.part.coef, fm.mean.stre, fm.mean.gs, fm.soc.sel, fm.top3.stre) %>% 
#   summarise(home.range.size_mean = mean(home.range.size, na.rm = TRUE),
#             home.range.size_sd = sd(home.range.size, na.rm = TRUE),
#             
#             
#             mm.deg_mean = mean(mm.deg, na.rm = TRUE),
#             mm.stre_mean = mean(mm.stre, na.rm = TRUE),
#             mm.eigen_mean = mean(mm.eigen, na.rm = TRUE),
#             mm.clust_mean = mean(mm.clust, na.rm = TRUE),
#             mm.betw_mean = mean(mm.betw, na.rm = TRUE),
#             mm.close_mean = mean(mm.close, na.rm = TRUE),
#             mm.part.coef_mean = mean(mm.part.coef, na.rm = TRUE),
#             mm.mean.stre_mean = mean(mm.mean.stre, na.rm = TRUE),
#             mm.mean.gs_mean = mean(mm.mean.gs, na.rm = TRUE),
#             mm.soc.sel_mean = mean(mm.soc.sel, na.rm = TRUE),
#             mm.top3.stre_mean = mean(mm.top3.stre, na.rm = TRUE),
#             
#             fm.deg_mean = mean(fm.deg, na.rm = TRUE),
#             fm.stre_mean = mean(fm.stre, na.rm = TRUE),
#             fm.eigen_mean = mean(fm.eigen, na.rm = TRUE),
#             fm.clust_mean = mean(fm.clust, na.rm = TRUE),
#             fm.betw_mean = mean(fm.betw, na.rm = TRUE),
#             fm.close_mean = mean(fm.close, na.rm = TRUE),
#             fm.part.coef_mean = mean(fm.part.coef, na.rm = TRUE),
#             fm.mean.stre_mean = mean(fm.mean.stre, na.rm = TRUE),
#             fm.mean.gs_mean = mean(fm.mean.gs, na.rm = TRUE),
#             fm.soc.sel_mean = mean(fm.soc.sel, na.rm = TRUE),
#             fm.top3.stre_mean = mean(fm.top3.stre, na.rm = TRUE),
#             
#             
#             mm.deg_sd = sd(mm.deg, na.rm = TRUE),
#             mm.stre_sd = sd(mm.stre, na.rm = TRUE),
#             mm.eigen_sd = sd(mm.eigen, na.rm = TRUE),
#             mm.clust_sd = sd(mm.clust, na.rm = TRUE),
#             mm.betw_sd = sd(mm.betw, na.rm = TRUE),
#             mm.close_sd = sd(mm.close, na.rm = TRUE),
#             mm.part.coef_sd = sd(mm.part.coef, na.rm = TRUE),
#             mm.mean.stre_sd = sd(mm.mean.stre, na.rm = TRUE),
#             mm.mean.gs_sd = sd(mm.mean.gs, na.rm = TRUE),
#             mm.soc.sel_sd = sd(mm.soc.sel, na.rm = TRUE),
#             mm.top3.stre_sd = sd(mm.top3.stre, na.rm = TRUE),
#             
#             fm.deg_sd = sd(fm.deg, na.rm = TRUE),
#             fm.stre_sd = sd(fm.stre, na.rm = TRUE),
#             fm.eigen_sd = sd(fm.eigen, na.rm = TRUE),
#             fm.clust_sd = sd(fm.clust, na.rm = TRUE),
#             fm.betw_sd = sd(fm.betw, na.rm = TRUE),
#             fm.close_sd = sd(fm.close, na.rm = TRUE),
#             fm.part.coef_sd = sd(fm.part.coef, na.rm = TRUE),
#             fm.mean.stre_sd = sd(fm.mean.stre, na.rm = TRUE),
#             fm.mean.gs_sd = sd(fm.mean.gs, na.rm = TRUE),
#             fm.soc.sel_sd = sd(fm.soc.sel, na.rm = TRUE),
#             fm.top3.stre_sd = sd(fm.top3.stre, na.rm = TRUE))
# 
# #### CENTERING ###
# male_behaviour <- male_behaviour %>% 
#   mutate(
#     home.range.size_centered = home.range.size - male_behaviour_means$home.range.size_mean,
#     
#     mm.deg_centered = mm.deg - male_behaviour_means$mm.deg_mean,
#     mm.stre_centered = mm.stre - male_behaviour_means$mm.stre_mean,
#     mm.eigen_centered = mm.eigen - male_behaviour_means$mm.eigen_mean,
#     mm.clust_centered = mm.clust - male_behaviour_means$mm.clust_mean,
#     mm.betw_centered = mm.betw - male_behaviour_means$mm.betw_mean,
#     mm.close_centered = mm.close - male_behaviour_means$mm.close_mean,
#     mm.part.coef_centered = mm.part.coef - male_behaviour_means$mm.part.coef_mean,
#     mm.mean.stre_centered = mm.stre - male_behaviour_means$mm.stre_mean,
#     mm.mean.gs_centered = mm.mean.gs - male_behaviour_means$mm.mean.gs_mean,
#     mm.soc.sel_centered = mm.soc.sel - male_behaviour_means$mm.soc.sel_mean,
#     mm.top3.stre_centered = mm.top3.stre - male_behaviour_means$mm.top3.stre_mean,
#     
#     fm.deg_centered = fm.deg - male_behaviour_means$fm.deg_mean,
#     fm.stre_centered = fm.stre - male_behaviour_means$fm.stre_mean,
#     fm.eigen_centered = fm.eigen - male_behaviour_means$fm.eigen_mean,
#     fm.clust_centered = fm.clust - male_behaviour_means$fm.clust_mean,
#     fm.betw_centered = fm.betw - male_behaviour_means$fm.betw_mean,
#     fm.close_centered = fm.close - male_behaviour_means$fm.close_mean,
#     fm.part.coef_centered = fm.part.coef - male_behaviour_means$fm.part.coef_mean,
#     fm.mean.stre_centered = fm.stre - male_behaviour_means$fm.stre_mean,
#     fm.mean.gs_centered = fm.mean.gs - male_behaviour_means$fm.mean.gs_mean,
#     fm.soc.sel_centered = fm.soc.sel - male_behaviour_means$fm.soc.sel_mean,
#     fm.top3.stre_centered = fm.top3.stre - male_behaviour_means$fm.top3.stre_mean
#   )
# 
# 
# 
# #### STANDARDISING 
# male_behaviour <- male_behaviour %>% 
#   mutate(
#     home.range.size.z = home.range.size_centered / male_behaviour_means$home.range.size_sd,
#     
#     mm.deg.z = mm.deg_centered / male_behaviour_means$mm.deg_sd,
#     mm.stre.z = mm.stre_centered / male_behaviour_means$mm.stre_sd,
#     mm.eigen.z = mm.eigen_centered / male_behaviour_means$mm.eigen_sd,
#     mm.clust.z = mm.clust_centered / male_behaviour_means$mm.clust_sd,
#     mm.betw.z = mm.betw_centered / male_behaviour_means$mm.betw_sd,
#     mm.close.z = mm.close_centered / male_behaviour_means$mm.close_sd,
#     mm.part.coef.z = mm.part.coef_centered / male_behaviour_means$mm.part.coef_sd,
#     mm.mean.stre.z = mm.stre_centered / male_behaviour_means$mm.stre_sd,
#     mm.mean.gs.z = mm.mean.gs_centered / male_behaviour_means$mm.mean.gs_sd,
#     mm.soc.sel.z = mm.soc.sel_centered / male_behaviour_means$mm.soc.sel_sd,
#     mm.top3.stre.z = mm.top3.stre_centered / male_behaviour_means$mm.top3.stre_sd,
#     
#     fm.deg.z = fm.deg_centered / male_behaviour_means$fm.deg_sd,
#     fm.stre.z = fm.stre_centered / male_behaviour_means$fm.stre_sd,
#     fm.eigen.z = fm.eigen_centered / male_behaviour_means$fm.eigen_sd,
#     fm.clust.z = fm.clust_centered / male_behaviour_means$fm.clust_sd,
#     fm.betw.z = fm.betw_centered / male_behaviour_means$fm.betw_sd,
#     fm.close.z = fm.close_centered / male_behaviour_means$fm.close_sd,
#     fm.part.coef.z = fm.part.coef_centered / male_behaviour_means$fm.part.coef_sd,
#     fm.mean.stre.z = fm.stre_centered / male_behaviour_means$fm.stre_sd,
#     fm.mean.gs.z = fm.mean.gs_centered / male_behaviour_means$fm.mean.gs_sd,
#     fm.soc.sel.z = fm.soc.sel_centered / male_behaviour_means$fm.soc.sel_sd,
#     fm.top3.stre.z = fm.top3.stre_centered / male_behaviour_means$fm.top3.stre_sd
#   )
# 
# male_behaviour_z <- male_behaviour %>%
#   select(
#     id,
#     year,
#     home.range.size.z,
#     mm.deg.z, mm.stre.z, mm.eigen.z, mm.clust.z, mm.betw.z, mm.close.z, mm.part.coef.z, mm.mean.stre.z, mm.mean.gs.z, mm.soc.sel.z, mm.top3.stre.z,
#     
#     fm.deg.z, fm.stre.z, fm.eigen.z, fm.clust.z, fm.betw.z, fm.close.z, fm.part.coef.z, fm.mean.stre.z, fm.mean.gs.z, fm.soc.sel.z, fm.top3.stre.z
#   )
# 
# ## renaming
# male_behaviour_z <- male_behaviour_z %>% 
#   mutate(ID = id,
#          CapYear = year) %>% 
#   select(-id, -year)
# 
# female_behaviour_z <- female_behaviour_z %>% 
#   mutate(ID = id,
#          CapYear = year) %>% 
#   select(-id, -year)
# 

#### SampleData Mean Centering ####
Ig_female <- read_csv("RawData/SampleData/PhenotypeClock_sample_ig_female_NEW.csv")
Para_female <- read_csv("RawData/SampleData/PhenotypeClock_sample_parasites_females.csv")
RTL_female <- read_csv("RawData/SampleData/PhenotypeClock_sample_RTL_female.csv")

Ig_male <- read_csv("RawData/SampleData/PhenotypeClock_sample_ig_male_NEW.csv")
Para_male <- read_csv("RawData/SampleData/PhenotypeClock_sample_parasites_males.csv")
RTL_male <- read_csv("RawData/SampleData/PhenotypeClock_sample_RTL_male.csv")

### some capture refs have more than one parasite measure associated with it so will for now do the mean for each capture

Para_female <- Para_female %>% 
  group_by(CapRef) %>% 
  summarise(ID = first(ID),
            Strongyles = mean(Strongyles, na.rm = TRUE),
            Coccidea = mean(Coccidea, na.rm = TRUE))
Para_male <- Para_male %>% 
  group_by(CapRef) %>% 
  summarise(ID = first(ID),
            Strongyles = mean(Strongyles, na.rm = TRUE),
            Coccidea = mean(Coccidea, na.rm = TRUE))

##### Female #####

female_Ig_means <- Ig_female %>% 
  select(IgA_Tc, IgE_Tc, IgG_Tc) %>% 
  summarise(mean_IgA = mean(IgA_Tc, na.rm = TRUE),
            mean_IgE = mean(IgE_Tc, na.rm = TRUE),
            mean_IgG = mean(IgG_Tc, na.rm = TRUE),

            sd_IgA = sd(IgA_Tc, na.rm = TRUE),
            sd_IgE = sd(IgE_Tc, na.rm = TRUE),
            sd_IgG = sd(IgG_Tc, na.rm = TRUE))

Ig_female <- Ig_female %>% 
  mutate(
    IgA_centered = IgA_Tc - female_Ig_means$mean_IgA,
    IgE_centered = IgE_Tc - female_Ig_means$mean_IgE,
    IgG_centered = IgG_Tc - female_Ig_means$mean_IgG)

Ig_female <- Ig_female %>% 
  mutate(
    IgA.z = IgA_centered / female_Ig_means$sd_IgA,
    IgE.z = IgE_centered / female_Ig_means$sd_IgE,
    IgG.z = IgG_centered / female_Ig_means$sd_IgG
  )

female_Ig.Z <- Ig_female %>% select(
  CapRef, CapYear, ID, IgA.z, IgE.z, IgG.z
)


female_para_means <- Para_female %>% 
  select(Strongyles, Coccidea) %>% 
  summarise(mean_Strongyles = mean(Strongyles, na.rm = TRUE),
            mean_Coccidea = mean(Coccidea, na.rm = TRUE),
            
            sd_Strongyles = sd(Strongyles, na.rm = TRUE),
            sd_Coccidea = sd(Coccidea, na.rm = TRUE))

Para_female <- Para_female %>% 
  mutate(
    Strongyles_centered = Strongyles - female_para_means$mean_Strongyles,
    Coccidea_centered = Coccidea - female_para_means$mean_Coccidea
  )

female_para.Z <- Para_female %>% 
  mutate(
    Strongyles.z = Strongyles_centered / female_para_means$sd_Strongyles,
    Coccidea.z = Coccidea_centered / female_para_means$sd_Coccidea
  ) %>% 
  select(CapRef, ID, Strongyles.z, Coccidea.z)

female_RTL_means <- RTL_female %>% 
  select(RTL) %>% 
  summarise(mean_RTL = mean(RTL, na.rm = TRUE),
            sd_RTL = sd(RTL, na.rm = TRUE))

RTL_female <- RTL_female %>% 
  mutate(
    RTL_centered = RTL - female_RTL_means$mean_RTL
  )

female_RTL.z <- RTL_female %>% 
  mutate(
    RTL.z = RTL_centered / female_RTL_means$sd_RTL
  ) %>% 
  select(CapRef, CapYear, ID, RTL.z)

##### Male #####
male_Ig_means <- Ig_male %>% 
  select(IgA_Tc, IgE_Tc, IgG_Tc) %>% 
  summarise(mean_IgA = mean(IgA_Tc, na.rm = TRUE),
            mean_IgE = mean(IgE_Tc, na.rm = TRUE),
            mean_IgG = mean(IgG_Tc, na.rm = TRUE),
            
            sd_IgA = sd(IgA_Tc, na.rm = TRUE),
            sd_IgE = sd(IgE_Tc, na.rm = TRUE),
            sd_IgG = sd(IgG_Tc, na.rm = TRUE))

Ig_male <- Ig_male %>% 
  mutate(
    IgA_centered = IgA_Tc - male_Ig_means$mean_IgA,
    IgE_centered = IgE_Tc - male_Ig_means$mean_IgE,
    IgG_centered = IgG_Tc - male_Ig_means$mean_IgG)

Ig_male <- Ig_male %>% 
  mutate(
    IgA.z = IgA_centered / male_Ig_means$sd_IgA,
    IgE.z = IgE_centered / male_Ig_means$sd_IgE,
    IgG.z = IgG_centered / male_Ig_means$sd_IgG
  )

male_Ig.Z <- Ig_male %>% select(
  CapRef, CapYear, ID, IgA.z, IgE.z, IgG.z
)

male_para_means <- Para_male %>% 
  select(Strongyles, Coccidea) %>% 
  summarise(mean_Strongyles = mean(Strongyles, na.rm = TRUE),
            mean_Coccidea = mean(Coccidea, na.rm = TRUE),
            
            sd_Strongyles = sd(Strongyles, na.rm = TRUE),
            sd_Coccidea = sd(Coccidea, na.rm = TRUE))

Para_male <- Para_male %>% 
  mutate(
    Strongyles_centered = Strongyles - male_para_means$mean_Strongyles,
    Coccidea_centered = Coccidea - male_para_means$mean_Coccidea
  )

male_para.Z <- Para_male %>% 
  mutate(
    Strongyles.z = Strongyles_centered / male_para_means$sd_Strongyles,
    Coccidea.z = Coccidea_centered / male_para_means$sd_Coccidea
  ) %>% 
  select(CapRef, ID, Strongyles.z, Coccidea.z)


male_RTL_means <- RTL_male %>% 
  select(RTL) %>% 
  summarise(mean_RTL = mean(RTL, na.rm = TRUE),
            sd_RTL = sd(RTL, na.rm = TRUE))

RTL_male <- RTL_male %>% 
  mutate(
    RTL_centered = RTL - male_RTL_means$mean_RTL
  )

male_RTL.z <- RTL_male %>% 
  mutate(
    RTL.z = RTL_centered / male_RTL_means$sd_RTL
  ) %>% 
  select(CapRef, CapYear, ID, RTL.z)

#### Collating the sample Data ####
##### Female ####
female_sample <- full_join(female_Ig.Z, female_RTL.z)
female_sample <- full_join(female_sample, female_para.Z)

##### Male ####
male_sample <- full_join(male_Ig.Z, male_RTL.z)
male_sample <- full_join(male_sample, male_para.Z)


#### Behaviour Data NEW VERSION ####
## As of 7/11/25
## couple of new variables and some have been dropped

behaviourData <- read_csv("RawData/BehaviourData/sheep_behaviouraldata_frailtyclock_6Nov25.csv")

##### Female #####
female_behaviour <- behaviourData %>% 
  filter(sex == 1) %>% 
  select(year, id, sex, age, home.range.size, 
         # female-female interactions
         ff.deg, ff.stre, ff.eigen, ff.clust, ff.betw, ff.close, ff.part.coef, ff.mean.stre, ff.mean.gs, ff.soc.sel, 
         # female-male interactions
         os.deg, os.stre, os.mean.stre, os.soc.sel)

female_behaviour_means <- female_behaviour %>% 
  select(home.range.size, 
         # female-female interactions
         ff.deg, ff.stre, ff.eigen, ff.clust, ff.betw, ff.close, ff.part.coef, ff.mean.stre, ff.mean.gs, ff.soc.sel, 
         # female-male interactions
         os.deg, os.stre, os.mean.stre, os.soc.sel) %>% 
  summarise(home.range.size_mean = mean(home.range.size, na.rm = TRUE),
            home.range.size_sd = sd(home.range.size, na.rm = TRUE),
            
            
            ff.deg_mean = mean(ff.deg, na.rm = TRUE),
            ff.stre_mean = mean(ff.stre, na.rm = TRUE),
            ff.eigen_mean = mean(ff.eigen, na.rm = TRUE),
            ff.clust_mean = mean(ff.clust, na.rm = TRUE),
            ff.betw_mean = mean(ff.betw, na.rm = TRUE),
            ff.close_mean = mean(ff.close, na.rm = TRUE),
            ff.part.coef_mean = mean(ff.part.coef, na.rm = TRUE),
            ff.mean.stre_mean = mean(ff.mean.stre, na.rm = TRUE),
            ff.mean.gs_mean = mean(ff.mean.gs, na.rm = TRUE),
            ff.soc.sel_mean = mean(ff.soc.sel, na.rm = TRUE),
            
            os.deg_mean = mean(os.deg, na.rm = TRUE),
            os.stre_mean = mean(os.stre, na.rm = TRUE),
            os.mean.stre_mean = mean(os.mean.stre, na.rm = TRUE),
            os.soc.sel_mean = mean(os.soc.sel, na.rm = TRUE),
            
            
            ff.deg_sd = sd(ff.deg, na.rm = TRUE),
            ff.stre_sd = sd(ff.stre, na.rm = TRUE),
            ff.eigen_sd = sd(ff.eigen, na.rm = TRUE),
            ff.clust_sd = sd(ff.clust, na.rm = TRUE),
            ff.betw_sd = sd(ff.betw, na.rm = TRUE),
            ff.close_sd = sd(ff.close, na.rm = TRUE),
            ff.part.coef_sd = sd(ff.part.coef, na.rm = TRUE),
            ff.mean.stre_sd = sd(ff.mean.stre, na.rm = TRUE),
            ff.mean.gs_sd = sd(ff.mean.gs, na.rm = TRUE),
            ff.soc.sel_sd = sd(ff.soc.sel, na.rm = TRUE),
            
            os.deg_sd = sd(os.deg, na.rm = TRUE),
            os.stre_sd = sd(os.stre, na.rm = TRUE),
            os.mean.stre_sd = sd(os.mean.stre, na.rm = TRUE),
            os.soc.sel_sd = sd(os.soc.sel, na.rm = TRUE)
            )

female_behaviour <- female_behaviour %>% 
  mutate(
    home.range.size_centered = home.range.size - female_behaviour_means$home.range.size_mean,
    
    ff.deg_centered = ff.deg - female_behaviour_means$ff.deg_mean,
    ff.stre_centered = ff.stre - female_behaviour_means$ff.stre_mean,
    ff.eigen_centered = ff.eigen - female_behaviour_means$ff.eigen_mean,
    ff.clust_centered = ff.clust - female_behaviour_means$ff.clust_mean,
    ff.betw_centered = ff.betw - female_behaviour_means$ff.betw_mean,
    ff.close_centered = ff.close - female_behaviour_means$ff.close_mean,
    ff.part.coef_centered = ff.part.coef - female_behaviour_means$ff.part.coef_mean,
    ff.mean.stre_centered = ff.mean.stre - female_behaviour_means$ff.mean.stre_mean,
    ff.mean.gs_centered = ff.mean.gs - female_behaviour_means$ff.mean.gs_mean,
    ff.soc.sel_centered = ff.soc.sel - female_behaviour_means$ff.soc.sel_mean,
    
    os.deg_centered = os.deg - female_behaviour_means$os.deg_mean,
    os.stre_centered = os.stre - female_behaviour_means$os.stre_mean,
    os.mean.stre_centered = os.mean.stre - female_behaviour_means$os.mean.stre_mean,
    os.soc.sel_centered = os.soc.sel - female_behaviour_means$os.soc.sel_mean
  )

female_behaviour <- female_behaviour %>% 
  mutate(
    home.range.size.z = home.range.size_centered / female_behaviour_means$home.range.size_sd,
    
    ff.deg.z = ff.deg_centered / female_behaviour_means$ff.deg_sd,
    ff.stre.z = ff.stre_centered / female_behaviour_means$ff.stre_sd,
    ff.eigen.z = ff.eigen_centered / female_behaviour_means$ff.eigen_sd,
    ff.clust.z = ff.clust_centered / female_behaviour_means$ff.clust_sd,
    ff.betw.z = ff.betw_centered / female_behaviour_means$ff.betw_sd,
    ff.close.z = ff.close_centered / female_behaviour_means$ff.close_sd,
    ff.part.coef.z = ff.part.coef_centered / female_behaviour_means$ff.part.coef_sd,
    ff.mean.stre.z = ff.mean.stre_centered / female_behaviour_means$ff.mean.stre_sd,
    ff.mean.gs.z = ff.mean.gs_centered / female_behaviour_means$ff.mean.gs_sd,
    ff.soc.sel.z = ff.soc.sel_centered / female_behaviour_means$ff.soc.sel_sd,

    os.deg.z = os.deg_centered / female_behaviour_means$os.deg_sd,
    os.stre.z = os.stre_centered / female_behaviour_means$os.stre_sd,
    os.mean.stre.z = os.mean.stre_centered / female_behaviour_means$os.mean.stre_sd,
    os.soc.sel.z = os.soc.sel_centered / female_behaviour_means$os.soc.sel_sd,
  )

female_behaviour_z <- female_behaviour %>%
  select(
    id,
    year,
    home.range.size.z,
    ff.deg.z, ff.stre.z, ff.eigen.z, ff.clust.z, ff.betw.z, ff.close.z, ff.part.coef.z, ff.mean.stre.z, ff.mean.gs.z, ff.soc.sel.z, 
    
    os.deg.z, os.stre.z, os.mean.stre.z, os.soc.sel.z
  )


##### male #####
male_behaviour <- behaviourData %>% 
  filter(sex == 2) %>% 
  select(year, id, sex, age, home.range.size, 
         # male-male interactions
         mm.deg, mm.stre, mm.eigen, mm.clust, mm.betw, mm.close, mm.part.coef, mm.mean.stre, mm.mean.gs, mm.soc.sel, 
         # male-male interactions
         os.deg, os.stre, os.mean.stre, os.soc.sel)

male_behaviour_means <- male_behaviour %>% 
  select(home.range.size, 
         # male-male interactions
         mm.deg, mm.stre, mm.eigen, mm.clust, mm.betw, mm.close, mm.part.coef, mm.mean.stre, mm.mean.gs, mm.soc.sel, 
         # male-male interactions
         os.deg, os.stre, os.mean.stre, os.soc.sel) %>% 
  summarise(home.range.size_mean = mean(home.range.size, na.rm = TRUE),
            home.range.size_sd = sd(home.range.size, na.rm = TRUE),
            
            
            mm.deg_mean = mean(mm.deg, na.rm = TRUE),
            mm.stre_mean = mean(mm.stre, na.rm = TRUE),
            mm.eigen_mean = mean(mm.eigen, na.rm = TRUE),
            mm.clust_mean = mean(mm.clust, na.rm = TRUE),
            mm.betw_mean = mean(mm.betw, na.rm = TRUE),
            mm.close_mean = mean(mm.close, na.rm = TRUE),
            mm.part.coef_mean = mean(mm.part.coef, na.rm = TRUE),
            mm.mean.stre_mean = mean(mm.mean.stre, na.rm = TRUE),
            mm.mean.gs_mean = mean(mm.mean.gs, na.rm = TRUE),
            mm.soc.sel_mean = mean(mm.soc.sel, na.rm = TRUE),
            
            os.deg_mean = mean(os.deg, na.rm = TRUE),
            os.stre_mean = mean(os.stre, na.rm = TRUE),
            os.mean.stre_mean = mean(os.mean.stre, na.rm = TRUE),
            os.soc.sel_mean = mean(os.soc.sel, na.rm = TRUE),
            
            
            mm.deg_sd = sd(mm.deg, na.rm = TRUE),
            mm.stre_sd = sd(mm.stre, na.rm = TRUE),
            mm.eigen_sd = sd(mm.eigen, na.rm = TRUE),
            mm.clust_sd = sd(mm.clust, na.rm = TRUE),
            mm.betw_sd = sd(mm.betw, na.rm = TRUE),
            mm.close_sd = sd(mm.close, na.rm = TRUE),
            mm.part.coef_sd = sd(mm.part.coef, na.rm = TRUE),
            mm.mean.stre_sd = sd(mm.mean.stre, na.rm = TRUE),
            mm.mean.gs_sd = sd(mm.mean.gs, na.rm = TRUE),
            mm.soc.sel_sd = sd(mm.soc.sel, na.rm = TRUE),
            
            os.deg_sd = sd(os.deg, na.rm = TRUE),
            os.stre_sd = sd(os.stre, na.rm = TRUE),
            os.mean.stre_sd = sd(os.mean.stre, na.rm = TRUE),
            os.soc.sel_sd = sd(os.soc.sel, na.rm = TRUE)
  )

male_behaviour <- male_behaviour %>% 
  mutate(
    home.range.size_centered = home.range.size - male_behaviour_means$home.range.size_mean,
    
    mm.deg_centered = mm.deg - male_behaviour_means$mm.deg_mean,
    mm.stre_centered = mm.stre - male_behaviour_means$mm.stre_mean,
    mm.eigen_centered = mm.eigen - male_behaviour_means$mm.eigen_mean,
    mm.clust_centered = mm.clust - male_behaviour_means$mm.clust_mean,
    mm.betw_centered = mm.betw - male_behaviour_means$mm.betw_mean,
    mm.close_centered = mm.close - male_behaviour_means$mm.close_mean,
    mm.part.coef_centered = mm.part.coef - male_behaviour_means$mm.part.coef_mean,
    mm.mean.stre_centered = mm.mean.stre - male_behaviour_means$mm.mean.stre_mean,
    mm.mean.gs_centered = mm.mean.gs - male_behaviour_means$mm.mean.gs_mean,
    mm.soc.sel_centered = mm.soc.sel - male_behaviour_means$mm.soc.sel_mean,
    
    os.deg_centered = os.deg - male_behaviour_means$os.deg_mean,
    os.stre_centered = os.stre - male_behaviour_means$os.stre_mean,
    os.mean.stre_centered = os.mean.stre - male_behaviour_means$os.mean.stre_mean,
    os.soc.sel_centered = os.soc.sel - male_behaviour_means$os.soc.sel_mean
  )

male_behaviour <- male_behaviour %>% 
  mutate(
    home.range.size.z = home.range.size_centered / male_behaviour_means$home.range.size_sd,
    
    mm.deg.z = mm.deg_centered / male_behaviour_means$mm.deg_sd,
    mm.stre.z = mm.stre_centered / male_behaviour_means$mm.stre_sd,
    mm.eigen.z = mm.eigen_centered / male_behaviour_means$mm.eigen_sd,
    mm.clust.z = mm.clust_centered / male_behaviour_means$mm.clust_sd,
    mm.betw.z = mm.betw_centered / male_behaviour_means$mm.betw_sd,
    mm.close.z = mm.close_centered / male_behaviour_means$mm.close_sd,
    mm.part.coef.z = mm.part.coef_centered / male_behaviour_means$mm.part.coef_sd,
    mm.mean.stre.z = mm.mean.stre_centered / male_behaviour_means$mm.mean.stre_sd,
    mm.mean.gs.z = mm.mean.gs_centered / male_behaviour_means$mm.mean.gs_sd,
    mm.soc.sel.z = mm.soc.sel_centered / male_behaviour_means$mm.soc.sel_sd,
    
    os.deg.z = os.deg_centered / male_behaviour_means$os.deg_sd,
    os.stre.z = os.stre_centered / male_behaviour_means$os.stre_sd,
    os.mean.stre.z = os.mean.stre_centered / male_behaviour_means$os.mean.stre_sd,
    os.soc.sel.z = os.soc.sel_centered / male_behaviour_means$os.soc.sel_sd,
  )

male_behaviour_z <- male_behaviour %>%
  select(
    id,
    year,
    home.range.size.z,
    mm.deg.z, mm.stre.z, mm.eigen.z, mm.clust.z, mm.betw.z, mm.close.z, mm.part.coef.z, mm.mean.stre.z, mm.mean.gs.z, mm.soc.sel.z, 
    
    os.deg.z, os.stre.z, os.mean.stre.z, os.soc.sel.z
  )






#### Writing The New Datasets ####

#write_csv(female_sheet_z, "OutputData/TransformedData/female_sheet_z_NEW.csv")
#write_csv(male_sheet_z, "OutputData/TransformedData/male_sheet_z_NEW2.csv")
#write_csv(BirthPheno_recode, "OutputData/TransformedData/BirthPheno_recode.csv")

#write_csv(female_behaviour_z, "OutputData/TransformedData/female_behaviour_zNEW.csv")
#write_csv(male_behaviour_z, "OutputData/TransformedData/male_behaviour_zNEW.csv")

#write_csv(female_sample, "OutputData/TransformedData/female_sample_z.csv")
#write_csv(male_sample, "OutputData/TransformedData/male_sample_z.csv")
