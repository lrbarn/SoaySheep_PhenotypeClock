#### 1. Modelling and plotting each trait by age ####

#### Packages ####
library(tID_recodeyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggeffects)
library(gridExtra)

#### Data ####
traits_f <- read_csv("Writing/Data/ClockData_f.csv")
traits_m <- read_csv("Writing/Data/ClockData_m.csv")

#### Adding in Age Last Measure ####
traits_f <- traits_f %>% 
  group_by(ID_recode_recode) %>% 
  mutate(ALM = last(AgeY)) %>% 
  ungroup()

traits_m <- traits_m %>% 
  group_by(ID_recode_recode) %>% 
  mutate(ALM = last(AgeY)) %>% 
  ungroup()

#### Adding sex and age squared
traits_f <- traits_f %>% 
  mutate(AgeY2 = AgeY^2,
         Sex = "Female")

traits_m <- traits_m %>% 
  mutate(AgeY2 = AgeY^2,
         Sex = "Male")

## combined data
all_data <- full_join(traits_f, traits_m)


#### Models and Plots ####
##### Unshed Wool ####
mod_unshed_f <- glmer(UnshedWool_recode ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID_recode), data = traits_f,
                     family = binomial(link = "logit"))

mod_unshed_m <- glmer(UnshedWool_recode ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_m,
                      family = binomial(link = "logit"))

## dropping age^2
mod_unshed_m_ALT  <- glmer(UnshedWool_recode ~ AgeY + ALM +
                             (1|CapYear), data = traits_m,
                           family = binomial(link = "logit"))

### Plot
PLOT_unshed <- 
ggplot(data = all_data, aes(x = AgeY, y = UnshedWool_recode)) +
  geom_jitter(colour = "grey", height = 0.05, width = 0, alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE,
              aes(colour = Sex, fill = Sex)) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue", 
                               "Male" = "lightblue4")) +
  ylab("Unshed Wool") +
  xlab("Age (years)") +
  theme_bw()



##### Scouring #####
mod_scouring_f <- glmer(Scouring_recode ~ AgeY + AgeY2 + ALM +
                         (1|CapYear) + (1|ID_recode), data = traits_f,
                       family = binomial(link = "logit"))

mod_scouring_m <- glmer(Scouring_recode ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID_recode), data = traits_m,
                        family = binomial(link = "logit"))

# removed cap year as well as age2 -> still failing to converge
mod_scouring_m_ALT <- glmer(Scouring_recode ~ AgeY + ALM +
                               (1|ID_recode), data = traits_m,
                            family = binomial(link = "logit"))

PLOT_scouring <- 
  ggplot(data = all_data, aes(x = AgeY, y = Scouring_recode)) +
  geom_jitter(colour = "grey", height = 0.05, width = 0, alpha = 0.5, aes(shape = Sex)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE,
              aes(colour = Sex, fill = Sex)) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("Scouring") +
  xlab("Age (years)") +
  theme_bw()


##### Milk #####

mod_milk <- glmer(Milk_recode ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID_recode), data = traits_f, 
                  family = binomial(link = "logit"))

PLOT_milk <- 
  ggplot(data = all_data, aes(x = AgeY, y = Milk_recode)) +
  geom_jitter(colour = "grey", height = 0.05, width = 0, alpha = 0.5,
              size = 2, aes(shape = Sex)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE,
              aes(colour = Sex, fill = Sex)) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue", 
                               "Male" = "lightblue4")) +
  ylab("Milk") +
  xlab("Age (years)") +
  theme_bw()

##### Deformities #####
mod_deform_f <- glmer(TeethDeform_recode ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_f, 
                      family = binomial(link = "logit"))

mod_deform_m <- glmer(TeethDeform_recode ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_m, 
                      family = binomial(link = "logit"))

PLOT_deform <- 
  ggplot(data = all_data, aes(x = AgeY, y = TeethDeform_recode)) +
  geom_jitter(colour = "grey", height = 0.05, width = 0, alpha = 0.5,
              size = 2, aes(shape = Sex)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE,
              aes(colour = Sex, fill = Sex)) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue", 
                               "Male" = "lightblue4")) +
  ylab("Teeth Deformities") +
  xlab("Age (years)") +
  theme_bw()



##### Broken Horns #####
mod_broken_m <- glmer(BrokenHorns_recode ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_m, 
                      family = binomial(link = "logit"))

## dropping the squared age
mod_broken_m_ALT <- glmer(BrokenHorns_recode ~ AgeY + ALM +
                            (1|CapYear) + (1|ID_recode), data = traits_m, 
                          family = binomial(link = "logit"))

## plot
PLOT_broken <- 
  ggplot(data = all_data, aes(x = AgeY, y = BrokenHorns_recode)) +
  geom_jitter(colour = "grey", height = 0.05, width = 0, alpha = 0.5,
              size = 2, aes(shape = Sex)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE,
              aes(colour = Sex, fill = Sex)) +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue", 
                               "Male" = "lightblue4")) +
  ylab("Broken Horns") +
  xlab("Age (years)") +
  theme_bw()


##### Weight #####
mod_weight_f <- lmer(Weight.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID_recode), data = traits_f)

mod_weight_m <- lmer(Weight.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_weight <- 
  ggplot(data = all_data, aes(x = AgeY, y = Weight.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("Weight") +
  xlab("Age (years)") +
  theme_bw()



##### Foreleg #####
mod_foreleg_f <- lmer(ForeLeg.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_f)

mod_foreleg_m <- lmer(ForeLeg.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_foreLeg <- 
  ggplot(data = all_data, aes(x = AgeY, y = ForeLeg.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("Foreleg") +
  xlab("Age (years)") +
  theme_bw()


##### Ked Count ####
mod_ked_f <- lmer(Keds.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID_recode), data = traits_f)

mod_ked_m <- lmer(Keds.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID_recode), data = traits_m)

mod_ked_m_ALT <- lmer(Keds.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear), data = traits_m)

PLOT_keds <- 
  ggplot(data = all_data, aes(x = AgeY, y = Keds.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("Keds") +
  xlab("Age (years)") +
  theme_bw()


##### Incisor Count #####
mod_teeth_f <- lmer(Teeth.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID_recode), data = traits_f)

mod_teeth_m <- lmer(Teeth.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID_recode), data = traits_m)

mod_teeth_m_ALT <- lmer(Teeth.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear), data = traits_m)

PLOT_teeth <- 
  ggplot(data = all_data, aes(x = AgeY, y = Teeth.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("Adult Teeth") +
  xlab("Age (years)") +
  theme_bw()



##### Horn Length #####
mod_hornLen_m <- lmer(HornLen.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_hornLen <- 
  ggplot(data = all_data, aes(x = AgeY, y = HornLen.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("Horn Length") +
  xlab("Age (years)") +
  theme_bw()


##### Horn Circ #####
mod_hornCirc_m <- lmer(HornCirc.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_hornCirc <- 
  ggplot(data = all_data, aes(x = AgeY, y = HornCirc.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("Horn Circ.") +
  xlab("Age (years)") +
  theme_bw()


##### Bolcirc ####
mod_bolCirc_m <- lmer(BolCirc.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_bolCirc <- 
  ggplot(data = all_data, aes(x = AgeY, y = BolCirc.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("Testes Circ.") +
  xlab("Age (years)") +
  theme_bw()


##### BolLen ####
mod_bolLen_m <- lmer(BolLen.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_bolLen <- 
  ggplot(data = all_data, aes(x = AgeY, y = BolLen.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("Testes Circ.") +
  xlab("Age (years)") +
  theme_bw()


##### Strongyles #####
mod_strongyles_f <- lmer(Strongyles.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_f)

mod_strongyles_m <- lmer(Strongyles.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_strongyles <- 
  ggplot(data = all_data, aes(x = AgeY, y = Strongyles.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("Strongyles") +
  xlab("Age (years)") +
  theme_bw()


##### Coccidea.z#####
mod_coccidea_f <- lmer(Coccidea.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID_recode), data = traits_f)

mod_coccidea_m <- lmer(Coccidea.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_m)


PLOT_coccidea <- 
  ggplot(data = all_data, aes(x = AgeY, y = Coccidea.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("CoccID_recodeia") +
  xlab("Age (years)") +
  theme_bw()

##### IgA #####
mod_iga_f <- lmer(IgA.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID_recode), data = traits_f)

mod_iga_m <- lmer(IgA.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_iga <- 
  ggplot(data = all_data, aes(x = AgeY, y = IgA.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("IgA") +
  xlab("Age (years)") +
  theme_bw()

##### IgE #####
mod_ige_f <- lmer(IgE.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID_recode), data = traits_f)

mod_ige_m <- lmer(IgE.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_ige <- 
  ggplot(data = all_data, aes(x = AgeY, y = IgE.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("IgE") +
  xlab("Age (years)") +
  theme_bw()

##### IgG #####
mod_igg_f <- lmer(IgE.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID_recode), data = traits_f)

mod_igg_m <- lmer(IgE.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_igg <- 
  ggplot(data = all_data, aes(x = AgeY, y = IgG.z, fill = Sex, colour = Sex)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("IgG") +
  xlab("Age (years)") +
  theme_bw()

##### RTL #####
mod_rtl_f <- lmer(RTL.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID_recode), data = traits_f)

mod_rtl_m <- lmer(RTL.z ~ AgeY + AgeY2 + ALM +
                    (1|CapYear) + (1|ID_recode), data = traits_m)
## dropping age2 from the male model
mod_rtl_m_ALT <- lmer(RTL.z ~ AgeY + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_RTL <- 
  ggplot(data = all_data, aes(x = AgeY, y = RTL.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("RTL") +
  xlab("Age (years)") +
  theme_bw()


##### Home Range #####
mod_home_f <- lmer(home.range.size.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID_recode), data = traits_f)
## drop the age2
mod_home_f_ALT <- lmer(home.range.size.z ~ AgeY + ALM +
                     (1|CapYear) + (1|ID_recode), data = traits_f)


mod_home_m <- lmer(home.range.size.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID_recode), data = traits_m)

## drop the age2
mod_home_m_ALT <- lmer(home.range.size.z ~ AgeY + ALM +
                     (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_home <- 
  ggplot(data = all_data, aes(x = AgeY, y = home.range.size.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("Home Range Size") +
  xlab("Age (years)") +
  theme_bw()


##### Degree -ss ####
mod_deg.ff <- lmer(ff.deg.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID_recode), data = traits_f)

mod_deg.ff_alt <- lmer(ff.deg.z ~ AgeY + ALM +
                         (1|CapYear) + (1|ID_recode), data = traits_f)


mod_deg.mm <- lmer(mm.deg.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_degree.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, 
             aes(x = AgeY, y = ff.deg.z, shape = Sex, colour = "grey")) +
  geom_point(alpha = 0.5, data = all_data,
             aes(x = AgeY, y = mm.deg.z, shape = Sex, colour = "grey")) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = ff.deg.z, 
                  fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = mm.deg.z, 
                  fill = Sex, colour = Sex)) +
  ylab("SS Degree") +
  xlab("Age (years)") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  theme_bw()


##### Degree - os ####
mod_deg_os_f <- lmer(os.deg.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID_recode), data = traits_f)

## drop age2
mod_deg_os_f_ALT <- lmer(os.deg.z ~ AgeY + ALM +
                           (1|CapYear) + (1|ID_recode), data = traits_f)

mod_deg_os_m <- lmer(os.deg.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_degree.os <- 
  ggplot(data = all_data, aes(x = AgeY, y = os.deg.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("OS Degree") +
  xlab("Age (years)") +
  theme_bw()

##### Strength - ss ####
mod_stren.ff <- lmer(ff.stre.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID_recode), data = traits_f)

mod_stren.mm <- lmer(mm.stre.z ~ AgeY + AgeY2 + ALM +
                       (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_stren.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, 
             aes(x = AgeY, y = ff.stre.z, shape = Sex, colour = "grey")) +
  geom_point(alpha = 0.5, data = all_data,
             aes(x = AgeY, y = mm.stre.z, shape = Sex, colour = "grey")) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = ff.stre.z, 
                  fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = mm.stre.z, 
                  fill = Sex, colour = Sex)) +
  ylab("SS Strength") +
  xlab("Age (years)") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  theme_bw()



##### Strength -os #####
mod_stre_os_f <- lmer(os.stre.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_f)

mod_stre_os_f_ALT <- lmer(os.stre.z ~ AgeY + ALM +
                            (1|CapYear) + (1|ID_recode), data = traits_f)


mod_stre_os_m <- lmer(os.stre.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_stre.os <- 
  ggplot(data = all_data, aes(x = AgeY, y = os.stre.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("OS Strength") +
  xlab("Age (years)") +
  theme_bw()



##### Participation - ss #####
mod_partcoef.ff <- lmer(ff.part.coef.z ~ AgeY + AgeY2 + ALM +
                           (1|CapYear) + (1|ID_recode), data = traits_f)
## drop squared
mod_partcoef.ff_ALT <- lmer(ff.part.coef.z ~ AgeY + ALM +
                              (1|CapYear) + (1|ID_recode), data = traits_f)


mod_partcoef.mm <- lmer(mm.part.coef.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID_recode), data = traits_m)
## drop squared
mod_partcoef.mm_ALT <- lmer(mm.part.coef.z ~ AgeY + ALM +
                              (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_partcoef.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, 
             aes(x = AgeY, y = ff.part.coef.z, shape = Sex, colour = "grey")) +
  geom_point(alpha = 0.5, data = all_data,
             aes(x = AgeY, y = mm.part.coef.z, shape = Sex, colour = "grey")) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = ff.part.coef.z, 
                  fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = mm.part.coef.z, 
                  fill = Sex, colour = Sex)) +
  ylab("SS Part. Coef.") +
  xlab("Age (years)") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  theme_bw()

##### Mean Strength - ss #####
mod_meanStre.ff <- lmer(ff.mean.stre.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID_recode), data = traits_f)

mod_meanStre.mm <- lmer(mm.mean.stre.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_meanStre.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, 
             aes(x = AgeY, y = ff.mean.stre.z, shape = Sex, colour = "grey")) +
  geom_point(alpha = 0.5, data = all_data,
             aes(x = AgeY, y = mm.mean.stre.z, shape = Sex, colour = "grey")) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = ff.mean.stre.z, 
                  fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = mm.mean.stre.z, 
                  fill = Sex, colour = Sex)) +
  ylab("SS Mean Strength") +
  xlab("Age (years)") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  theme_bw()


##### Mean Strength - os #####
mod_meanStre_os_f <- lmer(os.mean.stre.z ~ AgeY + AgeY2 + ALM +
                            (1|CapYear) + (1|ID_recode), data = traits_f)
## drop age2
mod_meanStre_os_f_ALT <- lmer(os.mean.stre.z ~ AgeY + ALM +
                                (1|CapYear) + (1|ID_recode), data = traits_f)


mod_meanStre_os_m <- lmer(os.mean.stre.z ~ AgeY + AgeY2 + ALM +
                            (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_mean.stre.os <- 
  ggplot(data = all_data, aes(x = AgeY, y = os.mean.stre.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("OS Mean Strength") +
  xlab("Age (years)") +
  theme_bw()

##### Mean Group Size #####
mod_mean.gs_f <- lmer(ff.mean.gs.z ~ AgeY + AgeY2 + ALM +
                            (1|CapYear) + (1|ID_recode), data = traits_f)

mod_mean.gs_m <- lmer(mm.mean.gs.z ~ AgeY + AgeY2 + ALM +
                            (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_mean.gs <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, 
             aes(x = AgeY, y = ff.mean.gs.z, shape = Sex, colour = "grey")) +
  geom_point(alpha = 0.5, data = all_data,
             aes(x = AgeY, y = mm.mean.gs.z, shape = Sex, colour = "grey")) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = ff.mean.gs.z, 
                  fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = mm.mean.gs.z, 
                  fill = Sex, colour = Sex)) +
  ylab("Mean Group Size") +
  xlab("Age (years)") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  theme_bw()


##### social selectivity - ss #####
mod_socsel.ff <- lmer(ff.soc.sel.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_f)

mod_socsel.mm <- lmer(mm.soc.sel.z ~ AgeY + AgeY2 + ALM +
                        (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_socsel.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, 
             aes(x = AgeY, y = ff.soc.sel.z, shape = Sex, colour = "grey")) +
  geom_point(alpha = 0.5, data = all_data,
             aes(x = AgeY, y = mm.soc.sel.z, shape = Sex, colour = "grey")) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = ff.soc.sel.z, 
                  fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = mm.soc.sel.z, 
                  fill = Sex, colour = Sex)) +
  ylab("SS Social Diff.") +
  xlab("Age (years)") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  theme_bw()


##### social selectivity - os #####
mod_socsel_os_f <- lmer(os.soc.sel.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID_recode), data = traits_f)

mod_socsel_os_f_ALT <- lmer(os.soc.sel.z ~ AgeY + ALM +
                              (1|CapYear) + (1|ID_recode), data = traits_f)

mod_socsel_os_m <- lmer(os.soc.sel.z ~ AgeY + AgeY2 + ALM +
                          (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_soc.sel.os <- 
  ggplot(data = all_data, aes(x = AgeY, y = os.soc.sel.z)) +
  geom_point(colour = "grey", alpha = 0.5, size = 2, aes(shape = Sex)) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = Sex, fill = Sex))  +
  scale_colour_manual(values = c("Female" = "lightblue",
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  ylab("OS Social Diff.") +
  xlab("Age (years)") +
  theme_bw()

##### eigen ####
mod_eigen_f <- lmer(ff.eigen.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID_recode), data = traits_f)

mod_eigen_m <- lmer(mm.eigen.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_eigen.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, 
             aes(x = AgeY, y = ff.eigen.z, shape = Sex, colour = "grey")) +
  geom_point(alpha = 0.5, data = all_data,
             aes(x = AgeY, y = mm.eigen.z, shape = Sex, colour = "grey")) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = ff.soc.sel.z, 
                  fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = mm.eigen.z, 
                  fill = Sex, colour = Sex)) +
  ylab("SS Eigen") +
  xlab("Age (years)") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  theme_bw()


##### betweenness #####
mod_betw_f <- lmer(ff.betw.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID_recode), data = traits_f)

mod_betw_f_ALT <- lmer(ff.betw.z ~ AgeY + ALM +
                         (1|CapYear) + (1|ID_recode), data = traits_f)

mod_betw_m <- lmer(mm.betw.z ~ AgeY + AgeY2 + ALM +
                     (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_betw.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, 
             aes(x = AgeY, y = ff.betw.z, shape = Sex, colour = "grey")) +
  geom_point(alpha = 0.5, data = all_data,
             aes(x = AgeY, y = mm.betw.z, shape = Sex, colour = "grey")) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = ff.betw.z, 
                  fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = mm.betw.z, 
                  fill = Sex, colour = Sex)) +
  ylab("SS Between") +
  xlab("Age (years)") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  theme_bw()


##### Closeness #####
mod_close_f <- lmer(ff.close.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID_recode), data = traits_f)

mod_close_f_ALT <- lmer(ff.close.z ~ AgeY + ALM +
                          (1|CapYear) + (1|ID_recode), data = traits_f)


mod_close_m <- lmer(mm.close.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_close.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, 
             aes(x = AgeY, y = ff.close.z, shape = Sex, colour = "grey")) +
  geom_point(alpha = 0.5, data = all_data,
             aes(x = AgeY, y = mm.close.z, shape = Sex, colour = "grey")) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = ff.close.z, 
                  fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = mm.close.z, 
                  fill = Sex, colour = Sex)) +
  ylab("SS Close") +
  xlab("Age (years)") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  theme_bw()


##### Clustering #####
mod_clust_f <- lmer(ff.clust.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID_recode), data = traits_f)

mod_clust_f_ALT <- lmer(ff.clust.z ~ AgeY + ALM +
                          (1|CapYear) + (1|ID_recode), data = traits_f)

mod_clust_m <- lmer(mm.clust.z ~ AgeY + AgeY2 + ALM +
                      (1|CapYear) + (1|ID_recode), data = traits_m)

PLOT_clust.ss <- 
  ggplot() +
  geom_point(alpha = 0.5, data = all_data, 
             aes(x = AgeY, y = ff.clust.z, shape = Sex, colour = "grey")) +
  geom_point(alpha = 0.5, data = all_data,
             aes(x = AgeY, y = mm.clust.z, shape = Sex, colour = "grey")) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = ff.clust.z, 
                  fill = Sex, colour = Sex)) +
  geom_smooth(method = "lm", se = TRUE, data = all_data, 
              aes(x = AgeY, y = mm.clust.z, 
                  fill = Sex, colour = Sex)) +
  ylab("SS Clustering") +
  xlab("Age (years)") +
  scale_colour_manual(values = c("Female" = "lightblue", 
                                 "Male" = "lightblue4")) +
  scale_fill_manual(values = c("Female" = "lightblue",
                               "Male" = "lightblue4")) +
  theme_bw()

#### Panels of Plots ####
#plot_names <- ls(pattern = "^PLOT_")

plot_names <- c("PLOT_unshed", "PLOT_scouring", "PLOT_milk", "PLOT_deform", "PLOT_broken", "PLOT_weight", "PLOT_foreLeg", "PLOT_keds", "PLOT_teeth", "PLOT_hornLen", "PLOT_hornCirc", "PLOT_bolCirc", "PLOT_bolLen",
           "PLOT_strongyles", "PLOT_coccidea", "PLOT_iga", "PLOT_ige", "PLOT_igg", "PLOT_RTL",
           "PLOT_home", "PLOT_mean.gs",
           "PLOT_degree.os", "PLOT_degree.ss", "PLOT_stre.os", "PLOT_stren.ss", "PLOT_partcoef.ss", "PLOT_mean.stre.os", "PLOT_meanStre.ss", "PLOT_soc.sel.os", "PLOT_socsel.ss", "PLOT_eigen.ss", "PLOT_betw.ss", "PLOT_close.ss", "PLOT_clust.ss")


# Get the actual objects into a list
plot_list <- mget(plot_names)

# Combine them into one big panel
combined_panel <- patchwork::wrap_plots(plot_list) +
  patchwork::plot_layout(axis_titles = "collect", guides = "collect") 

