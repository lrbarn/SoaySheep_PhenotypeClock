#### 22. Figure Creation ####

#### Packages ####
library(tidyverse)
library(gridExtra)
library(patchwork)

#### Data ####
##### Clock Outputs#####

## Sheet Clock
female_sheetClock <- readr::read_csv("OutputData/FinalModelData/SheetClock/f_predictions.csv")

male_sheetClock <- readr::read_csv("OutputData/FinalModelData/SheetClock/m_predictions.csv")

## BioAssay Clock
female_bioClock <- readr::read_csv("OutputData/FinalModelData/BioAssayClock/f_predictions.csv")

male_bioClock <- readr::read_csv("OutputData/FinalModelData/BioAssayClock/m_predictions.csv")


## Behaviour Clock
female_behClock <- readr::read_csv("OutputData/FinalModelData/BehaviourClock/f_predictions.csv")

male_behClock <- readr::read_csv("OutputData/FinalModelData/BehaviourClock/m_predictions.csv")

## Composite Clock
female_compClock <- readr::read_csv("OutputData/FinalModelData/PhenotypeClock/f_predictions.csv")

male_compClock <- readr::read_csv("OutputData/FinalModelData/PhenotypeClock/m_predictions.csv")

##### Coefficient Sizes#####
mod_coef <- readxl::read_excel("OutputData/Results/ClockModelCoef_ZEROFILLIN_NEW.xlsx")

#### Multi Panel of All Clocks ####
## there will be a total of 8 graphs, 4 clocks by 2 sexes

##### Female Sheet Clock####
PLOT_femaleSheet <-
  ggplot(female_sheetClock, aes(x = testAgeY, y = testAgeYPRED)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE) +
  # labs(x = "Chronological Age (Years)",
  #      y = "Predicted Age (Years)",
  #      title = "Female Processing Sheet Clock"
  #      ) +
  xlim(-1, 15) +
  ylim(-1, 15) +
  geom_abline(intercept = 0, slope = 1, colour = "pink", linetype = 2) +
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 15) +
  theme_bw()

##### Male Sheet Clock####
PLOT_maleSheet <-
  ggplot(male_sheetClock, aes(x = testAgeY, y = testAgeYPRED)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE) +
  # labs(x = "Chronological Age (Years)",
  #      y = "Predicted Age (Years)",
  #      title = "Male Processing Sheet Clock") +
  xlim(-1, 11) +
  ylim(-1, 11) +
  geom_abline(intercept = 0, slope = 1, colour = "pink", linetype = 2) +
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 11) +
  theme_bw()

##### Female BioAssay Clock####
PLOT_femaleBio <-
  ggplot(female_bioClock, aes(x = testAgeY, y = testAgeYPRED)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE) +
  # labs(x = "Chronological Age (Years)",
  #      y = "Predicted Age (Years)",
  #      title = "Female BioAssay Clock") +
  xlim(0, 15) +
  ylim(-5.5, 15) +
  geom_abline(intercept = 0, slope = 1, colour = "pink", linetype = 2) +
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 15) +
  theme_bw()

##### Male BioAssay Clock####
PLOT_maleBio <-
  ggplot(male_bioClock, aes(x = testAgeY, y = testAgeYPRED)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE) +
  # labs(x = "Chronological Age (Years)",
  #      y = "Predicted Age (Years)",
  #      title = "Male BioAssay Clock") +
  xlim(0, 11) +
  ylim(-3, 11) +
  geom_abline(intercept = 0, slope = 1, colour = "pink", linetype = 2) +
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 11) +
  theme_bw()


##### Female Behaviour Clock####
PLOT_femaleBeh <-
  ggplot(female_behClock, aes(x = testAgeY, y = testAgeYPRED)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE) +
  # labs(x = "Chronological Age (Years)",
  #      y = "Predicted Age (Years)",
  #      title = "Female Behaviour Clock") +
  xlim(-1, 15) +
  ylim(-1, 15) +
  geom_abline(intercept = 0, slope = 1, colour = "pink", linetype = 2) +
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 15) +
  theme_bw()

##### Male Behaviour Clock####
PLOT_maleBeh <-
  ggplot(male_behClock, aes(x = testAgeY, y = testAgeYPRED)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE) +
  # labs(x = "Chronological Age (Years)",
  #      y = "Predicted Age (Years)",
  #      title = "Male Behaviour Clock") +
  xlim(0, 11) +
  ylim(-1, 11) +
  geom_abline(intercept = 0, slope = 1, colour = "pink", linetype = 2) +
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 11) +
  theme_bw()


##### Female Composite Clock ####

PLOT_femaleComp <-
  ggplot(female_compClock, aes(x = testAgeY, y = testAgeYPRED)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE) +
  # labs(x = "Chronological Age (Years)",
  #      y = "Predicted Age (Years)",
  #      title = "Female Composite Phenotype Clock") +
  xlim(-1, 15) +
  ylim(-1, 15) +
  geom_abline(intercept = 0, slope = 1, colour = "pink", linetype = 2) +
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 15) +
  theme_bw()

##### Male Composite Clock ####
PLOT_maleComp <-
ggplot(male_compClock, aes(x = testAgeY, y = testAgeYPRED)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE) +
  # labs(x = "Chronological Age (Years)",
  #      y = "Predicted Age (Years)",
  #      title = "Male Composite Phenotype Clock") +
  xlim(0, 11) +
  ylim(-1, 11) +
  geom_abline(intercept = 0, slope = 1, colour = "pink", linetype = 2) +
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 11) +
  theme_bw()

##### Combining into one panel #####

patch_sheet <- PLOT_femaleSheet + PLOT_maleSheet +
  plot_layout(axis_titles = "collect") &
  labs(x = "Chronological Age (Years)",
       y = "Predicted Age (Years)") 
patch_bio <- PLOT_femaleBio + PLOT_maleBio +
  plot_layout(axis_titles = "collect") &
  labs(x = "Chronological Age (Years)",
       y = "Predicted Age (Years)") 
patch_beh <- PLOT_femaleBeh + PLOT_maleBeh +
  plot_layout(axis_titles = "collect") &
  labs(x = "Chronological Age (Years)",
       y = "Predicted Age (Years)") 
patch_comp <- PLOT_femaleComp + PLOT_maleComp +
  plot_layout(axis_titles = "collect") &
  labs(x = "Chronological Age (Years)",
       y = "Predicted Age (Years)") 

patchwork <- patch_sheet /
  patch_bio/
  patch_beh /
  patch_comp +
  plot_annotation(tag_levels = "A",
                  tag_suffix = ")") 


patchwork <-
(PLOT_femaleSheet | PLOT_maleSheet) / 
  (PLOT_femaleBio | PLOT_maleBio) /
  (PLOT_femaleBeh | PLOT_maleBeh) /
  (PLOT_femaleComp | PLOT_maleComp)

final<-
patchwork +
  plot_layout(axis_titles = "collect") +
  plot_annotation(tag_levels = "A", tag_suffix = ")") &
  labs(x = "Chronological Age (Years)",
       y = "Predicted Age (Years)") 

final2 <- 
  patchwork +
plot_layout(axis_titles = "collect") + 
  plot_annotation(
    tag_levels = "A", 
    tag_suffix = ")"
  ) & 
  # This '&' applies the labels and theme to EVERY subplot
  labs(x = "Chronological Age (Years)", 
       y = "Predicted Age (Years)") &
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 12)
  )

#### OLD #####
grid.arrange(
  PLOT_femaleSheet, PLOT_maleSheet,
  PLOT_femaleBio, PLOT_maleBio,
  PLOT_femaleBeh, PLOT_maleBeh,
  PLOT_femaleComp, PLOT_maleComp,
  ncol = 2)


### Alternative
patchwork <-
  PLOT_femaleSheet + PLOT_maleSheet + PLOT_femaleBio + PLOT_maleBio +
  PLOT_femaleBeh + PLOT_maleBeh + PLOT_femaleComp + PLOT_maleComp +
  plot_layout(axes = "collect", ncol=2, byrow=TRUE) +
  plot_annotation(tag_levels = "A", tag_suffix = ")") &
  labs(x = "Chronological Age (Years)",
       y = "Predicted Age (Years)")



#### Random Effects Stacked Bar ####

# random_effects <- readxl::read_excel("OutputData/Results/RandomEffectsOnly.xlsx")
# 
# ggplot(random_effects, aes(x = Model, y = Proportion, fill = randomEffect)) +
#   geom_col() + 
#   scale_y_continuous(labels = scales::percent) +
#   labs(y = "Proportion of Variance", x = "Model", fill = "Random Effect") +
#   theme_bw()
# 

#### Clock coefficients lined up ####
# reformatting the data
mod_coef$clock <- as.factor(mod_coef$clock)
mod_coef$Variable <- as.factor(mod_coef$Variable)


##### Composite Clock #####
## Reorder by female variable size

mod_coef_composite <- mod_coef %>% 
  filter(Variable != "(Intercept)",
         clock == "Composite") %>%
  group_by(Variable) %>%
  mutate(female_sort_val = mean(Coefficient[Sex == "Female"])) %>%
  ungroup() %>%
  mutate(Variable = fct_reorder(Variable, female_sort_val))

PLOT_coefComp <-
ggplot(mod_coef_composite, 
       aes(x = Variable, y = Coefficient, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_point(data = mod_coef_composite %>% filter(filledIn == "y"),
             aes(x = Variable, y = 0, colour = Sex),              # Force y to 0
             shape = 4,                             # Shape 4 is an 'x', Shape 3 is a '+'
             size = 1,                              # Adjusted size (10 is very large)
             stroke = 1,                          # Makes the cross lines thicker   
             position = position_dodge2(width = 1, preserve = "total")
  ) +
  scale_fill_manual(values = c("Female" = "lightblue", 
                               "Male" = "lightblue4")) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  coord_flip() +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank())


##### Processing sheet####

mod_coef_sheet <- mod_coef %>% 
  filter(Variable != "(Intercept)",
         clock == "sheet") %>%
  group_by(Variable) %>%
  mutate(female_sort_val = mean(Coefficient[Sex == "Female"])) %>%
  ungroup() %>%
  mutate(Variable = fct_reorder(Variable, female_sort_val)) %>% 
  unique()

PLOT_coefSheet <-
ggplot(mod_coef_sheet,
       aes(x = Variable, y = Coefficient, fill = Sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1))+
  geom_point(data = mod_coef_sheet %>% filter(filledIn == "y"),
             aes(x = Variable, y = 0, colour = Sex),              # Force y to 0
             shape = 4,                             # Shape 4 is an 'x', Shape 3 is a '+'
             size = 1,                              # Adjusted size (10 is very large)
             stroke = 1.5,                          # Makes the cross lines thicker   
             position = position_dodge2(width = 1, preserve = "total")
          ) +
  scale_fill_manual(values = c("Female" = "lightblue", 
                               "Male" = "lightblue4")) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                               "Male" = "lightblue4")) +
  coord_flip() +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        legend.position = "none")


##### BioAssay ####

mod_coef_BA <- mod_coef %>% 
  filter(Variable != "(Intercept)",
         clock == "BioAssay") %>%
  group_by(Variable) %>%
  mutate(female_sort_val = mean(Coefficient[Sex == "Female"])) %>%
  ungroup() %>%
  mutate(Variable = fct_reorder(Variable, female_sort_val)) %>% 
  unique()


PLOT_coefBio <-
ggplot(mod_coef_BA,
       aes(x = Variable, y = Coefficient, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_point(data = mod_coef_BA %>% filter(filledIn == "y"),
             aes(x = Variable, y = 0, colour = Sex),              # Force y to 0
             shape = 4,                             # Shape 4 is an 'x', Shape 3 is a '+'
             size = 1,                              # Adjusted size (10 is very large)
             stroke = 1.5,                          # Makes the cross lines thicker   
             position = position_dodge2(width = 1, preserve = "total")
  ) +
  scale_fill_manual(values = c("Female" = "lightblue", 
                               "Male" = "lightblue4")) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  coord_flip() +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        legend.position = "none")

## Behaviour
mod_coef_beh<- mod_coef %>% 
  filter(Variable != "(Intercept)",
         clock == "behaviour") %>%
  group_by(Variable) %>%
  mutate(female_sort_val = mean(Coefficient[Sex == "Female"])) %>%
  ungroup() %>%
  mutate(Variable = fct_reorder(Variable, female_sort_val)) %>% 
  unique()

PLOT_coefBeh <- 
ggplot(mod_coef_beh,
       aes(x = Variable, y = Coefficient, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_point(data = mod_coef_beh %>% filter(filledIn == "y"),
             aes(x = Variable, y = 0, colour = Sex),              # Force y to 0
             shape = 4,                             # Shape 4 is an 'x', Shape 3 is a '+'
             size = 1,                              # Adjusted size (10 is very large)
             stroke = 1.5,                          # Makes the cross lines thicker   
             position = position_dodge2(width = 1, preserve = "total")
  ) +
  scale_fill_manual(values = c("Female" = "lightblue", 
                               "Male" = "lightblue4")) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  coord_flip() +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        legend.position = "none")

##### combining into one panel ####
lay <- rbind(c(1, 2),
             c(1, 3),
             c(1, 4))

grid.arrange(PLOT_coefComp,  # Plot 1
             PLOT_coefSheet, # Plot 2
             PLOT_coefBio,   # Plot 3
             PLOT_coefBeh,   # Plot 4
             layout_matrix = lay)

#### Trying to neaten up with patchwork####
stack <- PLOT_coefSheet / PLOT_coefBio / PLOT_coefBeh + 
  plot_layout(axis_titles = "collect") 

coef_patch <-  (stack | PLOT_coefComp) + 
  plot_layout(axis_titles = "collect") &
  plot_annotation(tag_levels = "A", tag_suffix = ")")
coef_patch



