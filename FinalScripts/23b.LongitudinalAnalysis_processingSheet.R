#### 23b. Longitudinal Analysis - Processing Sheet Predictions ####
## Redoing with the processing sheet results which have more data pointes

#### Packages ####
library(tidyverse)
library(gridExtra)

#### Data ####
predictions_f <- read_csv("OutputData/FinalModelData/SheetClock/f_predictions.csv")
predictions_m <- read_csv("OutputData/FinalModelData/SheetClock/m_predictions.csv")


#### Female ####
##### Reformatting #####
## filtering to just longitudinal
indObs.f <- (count(predictions_f, testID, name = "numObs"))
longitudinalIDs.f <- indObs.f %>% filter(numObs >1)


f_long <- predictions_f %>% filter(testID %in% longitudinalIDs.f$testID)

## quick initial plot ##
ggplot(f_long, aes(x = testAgeY, y = testAgeYPRED, group = testID)) +
  geom_point() +
  geom_line() +
  theme_bw() 

## Zeroing the data and adding an age at first observation
f.Longitudinal_zeroed <- f_long %>% 
  arrange(testID, testAgeY) %>% 
  group_by(testID) %>% 
  mutate(chronologicalChange_FORPLOT = testAgeY - first(testAgeY),
         chronologicalChange = testAgeY - lag(testAgeY),
         AgeFirstObs = first(testAgeY),
         isFirst = ifelse(row_number() == 1, 1, 0)) %>% 
  mutate(chronologicalChange = ifelse(is.na(chronologicalChange), 0, as.numeric(chronologicalChange))) %>% 
  mutate(PredictedChange_FORPLOT = testAgeYPRED - first(testAgeYPRED),
         PredictedChange = testAgeYPRED - lag(testAgeYPRED)) %>% 
  mutate(PredictedChange = ifelse(is.na(PredictedChange), 0, as.numeric(PredictedChange)))%>% 
  mutate(OverEstimate = ifelse(PredictedChange > chronologicalChange, "Y", "N"))

## Plotting this
PLOT_femaleLong <-
ggplot(f.Longitudinal_zeroed, aes(x = chronologicalChange_FORPLOT, y = PredictedChange_FORPLOT)) +
  #geom_point() +
  geom_line(aes( group = testID), colour = "grey", alpha = 0.5) +
  geom_abline(linetype = 2, colour = "red") +
  facet_wrap(~AgeFirstObs,
             nrow = 4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Longitudinal Age Change (Females)",
       x = "Chronological Change (Years)",
       y = "Predicted Change (Years)") +
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 11) +
  theme_bw()

#### Male ####
##### Reformatting #####
## filtering to just longitudinal
indObs.m <- (count(predictions_m, testID, name = "numObs"))
longitudinalIDs.m <- indObs.m %>% filter(numObs >1)
s

m_long <- predictions_m %>% filter(testID %in% longitudinalIDs.m$testID)

## quick initial plot ##
ggplot(m_long, aes(x = testAgeY, y = testAgeYPRED, group = testID)) +
  geom_point() +
  geom_line() +
  theme_bw() 

## Zeroing the data and adding an age at first observation
m.Longitudinal_zeroed <- m_long %>% 
  arrange(testID, testAgeY) %>% 
  group_by(testID) %>% 
  mutate(chronologicalChange_FORPLOT = testAgeY - first(testAgeY),
         chronologicalChange = testAgeY - lag(testAgeY),
         AgeFirstObs = first(testAgeY),
         isFirst = ifelse(row_number() == 1, 1, 0)) %>% 
  mutate(chronologicalChange = ifelse(is.na(chronologicalChange), 0, as.numeric(chronologicalChange))) %>% 
  mutate(PredictedChange_FORPLOT = testAgeYPRED - first(testAgeYPRED),
         PredictedChange = testAgeYPRED - lag(testAgeYPRED)) %>% 
  mutate(PredictedChange = ifelse(is.na(PredictedChange), 0, as.numeric(PredictedChange)))%>% 
  mutate(OverEstimate = ifelse(PredictedChange > chronologicalChange, "Y", "N"))

## Plotting this
PLOT_maleLong <-
  ggplot(m.Longitudinal_zeroed, aes(x = chronologicalChange_FORPLOT, y = PredictedChange_FORPLOT)) +
  # Only geom_line will be grouped by individual testID
  geom_line(aes(group = testID), colour = "grey", alpha = 0.5) +
  
  # This will now calculate one line for all data within each facet
  geom_smooth(method = "lm", se = TRUE) +
  
  geom_abline(linetype = 2, colour = "red") +
  facet_wrap(~AgeFirstObs, nrow = 4) +
  labs(title = "Longitudinal Age Change (Males)",
       x = "Chronological Change (Years)",
       y = "Predicted Change (Years)") +
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 11) +
  theme_bw()

### Plotting as a multipanel
grid.arrange(PLOT_femaleLong, PLOT_maleLong,
             nrow= 1)

#### >2 observations ####

#### Female ####
##### Reformatting #####
## filtering to just longitudinal
longitudinalIDs2.f <- indObs.f %>% filter(numObs >2)


f_long2 <- predictions_f %>% filter(testID %in% longitudinalIDs2.f$testID)

## quick initial plot ##
ggplot(f_long2, aes(x = testAgeY, y = testAgeYPRED, group = testID)) +
  geom_point() +
  geom_line() +
  theme_bw() 

## Zeroing the data and adding an age at first observation
f.Longitudinal_zeroed2 <- f_long2 %>% 
  arrange(testID, testAgeY) %>% 
  group_by(testID) %>% 
  mutate(chronologicalChange_FORPLOT = testAgeY - first(testAgeY),
         chronologicalChange = testAgeY - lag(testAgeY),
         AgeFirstObs = first(testAgeY),
         isFirst = ifelse(row_number() == 1, 1, 0)) %>% 
  mutate(chronologicalChange = ifelse(is.na(chronologicalChange), 0, as.numeric(chronologicalChange))) %>% 
  mutate(PredictedChange_FORPLOT = testAgeYPRED - first(testAgeYPRED),
         PredictedChange = testAgeYPRED - lag(testAgeYPRED)) %>% 
  mutate(PredictedChange = ifelse(is.na(PredictedChange), 0, as.numeric(PredictedChange)))%>% 
  mutate(OverEstimate = ifelse(PredictedChange > chronologicalChange, "Y", "N"))

## Plotting this
PLOT_femaleLong2 <-
  ggplot(f.Longitudinal_zeroed2, aes(x = chronologicalChange, y = PredictedChange)) +
  #geom_point() +
  geom_line(aes( group = testID), colour = "grey", alpha = 0.5) +
  geom_abline(linetype = 2, colour = "red") +
  facet_wrap(~AgeFirstObs,
             nrow = 4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Longitudinal Age Change (Females)",
       x = "Chronological Change (Years)",
       y = "Predicted Change (Years)") +
  theme_bw()

#### Male ####
##### Reformatting #####
## filtering to just longitudinal
longitudinalIDs2.m <- indObs.m %>% filter(numObs >2)


m_long2 <- predictions_m %>% filter(testID %in% longitudinalIDs2.m$testID)

## quick initial plot ##
ggplot(m_long2, aes(x = testAgeY, y = testAgeYPRED, group = testID)) +
  geom_point() +
  geom_line() +
  theme_bw() 

## Zeroing the data and adding an age at first observation
m.Longitudinal_zeroed2 <- m_long2 %>% 
  arrange(testID, testAgeY) %>% 
  group_by(testID) %>% 
  mutate(chronologicalChange_FORPLOT = testAgeY - first(testAgeY),
         chronologicalChange = testAgeY - lag(testAgeY),
         AgeFirstObs = first(testAgeY),
         isFirst = ifelse(row_number() == 1, 1, 0)) %>% 
  mutate(chronologicalChange = ifelse(is.na(chronologicalChange), 0, as.numeric(chronologicalChange))) %>% 
  mutate(PredictedChange_FORPLOT = testAgeYPRED - first(testAgeYPRED),
         PredictedChange = testAgeYPRED - lag(testAgeYPRED)) %>% 
  mutate(PredictedChange = ifelse(is.na(PredictedChange), 0, as.numeric(PredictedChange)))%>% 
  mutate(OverEstimate = ifelse(PredictedChange > chronologicalChange, "Y", "N"))

## Plotting this
PLOT_maleLong2 <-
  ggplot(m.Longitudinal_zeroed2, aes(x = chronologicalChange, y = PredictedChange)) +
  # Only geom_line will be grouped by individual testID
  geom_line(aes(group = testID), colour = "grey", alpha = 0.5) +
  
  # This will now calculate one line for all data within each facet
  geom_smooth(method = "lm", se = FALSE) +
  
  geom_abline(linetype = 2, colour = "red") +
  facet_wrap(~AgeFirstObs, nrow = 3) +
  labs(title = "Longitudinal Age Change (Males)",
       x = "Chronological Change (Years)",
       y = "Predicted Change (Years)") +
  theme_bw()

### Plotting as a multipanel
#grid.arrange(PLOT_femaleLong2, PLOT_maleLong2, nrow= 1)


#### Binomial Testing ####

## need to filter out the first observation since there is no "change" associated with it

##### Female #####
## On the full data
binom.test(x = nrow(f.Longitudinal_zeroed %>% filter(OverEstimate == "Y",
                                                     isFirst == "0")),
           n = nrow(f.Longitudinal_zeroed %>% filter(isFirst == "0")),
           p = 0.5,
           conf.level = 0.95)

# ## On developing sheep
# # included age 2 here since it holds the information on the change between 1 and 2?
# binom.test(x = nrow(f.Longitudinal_zeroed %>% 
#                       filter(testAgeY <=2,
#                              OverEstimate == "Y",
#                              isFirst == "0")),
#            n = (nrow(f.Longitudinal_zeroed %>% filter(testAgeY <=2,
#                                                       isFirst == "0"))),
#            p = 0.5,
#            conf.level = 0.95)
# 
# ## Young sheep
# binom.test(x = nrow(f.Longitudinal_zeroed %>% 
#                       filter(testAgeY > 2 & testAgeY <= 4,
#                              OverEstimate == "Y",
#                              isFirst == "0")),
#            n = (nrow(f.Longitudinal_zeroed %>% filter(testAgeY > 2 & testAgeY <= 4,
#                                                       isFirst == "0"))),
#            p = 0.5,
#            conf.level = 0.95)

## Adult Sheep
binom.test(x = nrow(f.Longitudinal_zeroed %>% 
                      filter(testAgeY >= 2,
                             OverEstimate == "Y",
                             isFirst == "0")),
           n = (nrow(f.Longitudinal_zeroed %>% filter(testAgeY >= 2,
                                                      isFirst == "0"))),
           p = 0.5,
           conf.level = 0.95)


## Alternative Adult Sheep
binom.test(x = nrow(f.Longitudinal_zeroed %>% 
                      filter(testAgeY >= 2,
                             OverEstimate == "Y",
                             isFirst == "0")),
           n = (nrow(f.Longitudinal_zeroed %>% filter(testAgeY >= 2,
                                                      isFirst == "0"))),
           p = 0.5,
           conf.level = 0.95)

##### Male #####
## On the full data
binom.test(x = nrow(m.Longitudinal_zeroed %>% filter(OverEstimate == "Y",
                                                     isFirst == "0")),
           n = nrow(m.Longitudinal_zeroed %>% filter(isFirst == "0")),
           p = 0.5,
           conf.level = 0.95)

## On developing sheep
# binom.test(x = nrow(m.Longitudinal_zeroed %>% 
#                       filter(testAgeY <=2,
#                              OverEstimate == "Y",
#                              isFirst == "0")),
#            n = (nrow(m.Longitudinal_zeroed %>% filter(testAgeY <2,
#                                                       isFirst == "0"))),
#            p = 0.5,
#            conf.level = 0.95)

## Young sheep
# binom.test(x = nrow(m.Longitudinal_zeroed %>% 
#                       filter(testAgeY > 2 & testAgeY <= 4,
#                              OverEstimate == "Y",
#                              isFirst == "0")),
#            n = (nrow(m.Longitudinal_zeroed %>% filter(testAgeY > 1 & testAgeY <= 4,
#                                                       isFirst == "0"))),
#            p = 0.5,
#            conf.level = 0.95)

# ## Adult Sheep
# binom.test(x = nrow(m.Longitudinal_zeroed %>% 
#                       filter(testAgeY > 4,
#                              OverEstimate == "Y",
#                              isFirst == "0")),
#            n = (nrow(m.Longitudinal_zeroed %>% filter(testAgeY >4,
#                                                       isFirst == "0"))),
#            p = 0.5,
#            conf.level = 0.95)

## Alternative Adult Sheep
binom.test(x = nrow(m.Longitudinal_zeroed %>% 
                      filter(testAgeY >= 2,
                             OverEstimate == "Y",
                             isFirst == "0")),
           n = (nrow(m.Longitudinal_zeroed %>% filter(testAgeY >= 2,
                                                      isFirst == "0"))),
           p = 0.5,
           conf.level = 0.95)

