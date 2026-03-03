##Motivation: Survival analyses for phenotypic clock - code for sharing with reviewers and readers
#Author: Erin Siracusa
#Date last updated: 3-March-26


#### Libraries ####
library(tidyverse)
library(magrittr)
library(lme4)
library(ggeffects)
library(viridis)
library(emmeans)
library(ggpubr)
library(dotwhisker)
library(flextable)
library(kableExtra)
library(pixiedust)

#Set working directory
setwd("/Users/erinsiracusa/Dropbox/University of Edinburgh/Lucy Barnard/Barnard et al._PhilTrans/Analyses/Rscript")

#Use these data when running the models on the clock built from all sheep
delta_age <- read_csv("../Output/survival_data_fullclock.csv")

#Use these data when running the models on the clock built from adults only
delta_age <- read_csv("../Output/survival_data_adultonlyclock.csv")


#FUNCTIONS NEEDED FOR MODELS ------

#Overdispersion function

overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}



#ADULT SURVIVAL MODEL---------

adult_data <- delta_age %>%
  filter(age.cat == "adult" | age.cat == "young adult")

##FEMALE MODEL -----
female_adult_data <- adult_data %>%
  filter(Sex == 1)

#Z-score all variables 

female_adult_data %<>%
  mutate(delta_age.z = scale(delta_age),
         CAge.z = scale(CAge),
         mean.NAO.z = scale(mean.NAO),
         pop.size.z = scale(pop.size))

###Fit model --------

#Main model in manuscript
summary(Fadult_surv_mod <- glmer(surv_wint ~ scale(delta_age) + scale(CAge) + scale(mean.NAO) + scale(pop.size) + (1|ID) + (1|CapYear), family = binomial, data = female_adult_data))

#Check overdispersion
overdisp_fun(Fadult_surv_mod)

#Supplementary model including weight
summary(Fadult_surv_mod.supp <- glmer(surv_wint ~ delta_age.z + CAge.z + Weight.z + mean.NAO.z + pop.size.z + (1|ID) + (1|CapYear), family = binomial, data = female_adult_data))

#Check overdispersion
overdisp_fun(updated.Fadult_surv_mod.supp)

####Plot this-----

fm.dat <- ggpredict(Fadult_surv_mod, terms = "delta_age [all]")

jitter <- position_jitter(width = 0.1, height = 0.02)

Fadult.plot <- ggplot() +
  geom_ribbon(data = fm.dat,
              aes(x = x,
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.1)  +
  stat_smooth(data = fm.dat,
              aes(x = x ,
                  y = predicted),
              size = 1.5,
              alpha = .35,
              method="glm",
              method.args = list(family = "binomial"),
              show.legend = NA,
              se = F,
              color = "black") +
  geom_point(data = female_adult_data,
             aes(x = delta_age,
                 y = surv_wint),
             cex = 1,
             shape = 1,
             alpha = 0.3,
             position = jitter) + 
  theme_classic() + 
  xlab("Delta age") +
  ylab("Probability annual survival") +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  scale_x_continuous(breaks=seq(-8,4,4)) +
  theme(axis.text.x  = element_text(vjust=0.5, size=13, colour="black")) + 
  theme(axis.text.y  = element_text(vjust=0.5, size=13, colour="black")) + 
  theme(axis.title = element_text(size=18, vjust = -5))

Fadult.plot


##MALE MODEL------
male_adult_data <- adult_data %>%
  filter(Sex == 2)

#Z-score all variables 

male_adult_data %<>%
  mutate(delta_age.z = scale(delta_age),
         CAge.z = scale(CAge),
         mean.NAO.z = scale(mean.NAO),
         pop.size.z = scale(pop.size))

###Fit model  ------

#Main model in manuscript
summary(Madult_surv_mod <- glmer(surv_wint ~ scale(delta_age) + scale(CAge) + scale(mean.NAO) + scale(pop.size) + (1|ID) + (1|CapYear), family = binomial, data = male_adult_data))

#Check overdispersion
overdisp_fun(Madult_surv_mod)

#Supplementary model including weight
summary(Madult_surv_mod.supp <- glmer(surv_wint ~ delta_age.z*CAge.z + Weight.z + mean.NAO.z + pop.size.z + (1|ID) + (1|CapYear), family = binomial, data = male_adult_data))

#Check overdispersion
overdisp_fun(Madult_surv_mod.supp)

#Overdispersed so fitting a betabinomial model

library(glmmTMB)

#Main model in manuscript
summary(Madult_surv_mod <- glmmTMB(surv_wint ~ scale(delta_age) + scale(CAge) + scale(mean.NAO) + scale(pop.size) + (1|ID) + (1|CapYear), family = betabinomial, data = male_adult_data))

#Supplementary model including weight
summary(Madult_surv_mod.supp <- glmmTMB(surv_wint ~ delta_age.z + CAge.z + Weight.z + mean.NAO.z + pop.size.z + (1|ID) + (1|CapYear), family = betabinomial, data = male_adult_data))

####Plot this-----

fm.dat <- ggpredict(Madult_surv_mod, terms = "delta_age [all]")

jitter <- position_jitter(width = 0.1, height = 0.02)

Madult.plot <- ggplot() +
  geom_ribbon(data = fm.dat,
              aes(x = x,
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.1)  +
  stat_smooth(data = fm.dat,
              aes(x = x ,
                  y = predicted),
              size = 1.5,
              alpha = .35,
              method="glm",
              method.args = list(family = "binomial"),
              show.legend = NA,
              se = F,
              color = "black") +
  geom_point(data = male_adult_data,
             aes(x = delta_age,
                 y = surv_wint),
             cex = 1,
             shape = 1,
             alpha = 0.3,
             position = jitter) + 
  theme_classic() + 
  xlab("Delta age") +
  ylab("Probability annual survival") +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  theme(axis.text.x  = element_text(vjust=0.5, size=13, colour="black")) + 
  theme(axis.text.y  = element_text(vjust=0.5, size=13, colour="black")) + 
  theme(axis.title = element_text(size=18, vjust = -5))

Madult.plot


#Plot all figures together ----

#For results from clock built from all sheep
ggarrange(Fadult.plot, Madult.plot,
          labels = c("A", "B"),font.label = list(size =12),
          ncol = 2, nrow = 2)

#For results from clock built from adults only
ggarrange(Fadult.plot, Madult.plot,
          labels = c("A", "B"),font.label = list(size =12),
          ncol = 2, nrow = 2)

###Make table with results for each model -------

#Adult female model -----
Fadult_surv_mod.tidy <- broom.mixed::tidy(Fadult_surv_mod)

Fadult_surv_mod.tidy %<>%
  mutate(group = ifelse(group == "CapYear", "Year", group)) %>%
  mutate(group = ifelse(group == "ID", "Individual.ID", group)) %>%
  mutate(effect = ifelse(effect == "fixed", "Fixed Effects", "Random Effects")) %>%
  mutate(term = ifelse(term == "sd__(Intercept)", "sd(intercept)", term)) 

Fadult_surv_mod.tidy  %<>%
  relabel_predictors("(Intercept)" = "Intercept",
                     'scale(delta_age)' = "Delta age",
                     'scale(CAge)' = "Age",
                     'scale(mean.NAO)' = "NAO",
                     'scale(pop.size)' = "Population size")

#get rid of NAs
Fadult_surv_mod.tidy %<>% 
  mutate(group = ifelse(is.na(group), "", group))

dust(Fadult_surv_mod.tidy) %>%
  pixiedust::sprinkle(col = 4:7, 
                      round = 2) %>% 
  sprinkle_colnames(effect = "Effect",
                    group = "Group",
                    term = "Term",
                    estimate = "Estimate", 
                    std.error = "Std error",
                    statistic = "Test-statistic",
                    p.value = "P value") %>%
  kable() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(1, bold = T) %>%
  row_spec(c(2,3,4,5), bold = T) %>%
  collapse_rows(columns = 1:2, valign = "top")


#Adult male model ------
Madult_surv_mod.tidy <- broom.mixed::tidy(Madult_surv_mod)

Madult_surv_mod.tidy %<>%
  mutate(group = ifelse(group == "CapYear", "Year", group)) %>%
  mutate(group = ifelse(group == "ID", "Individual.ID", group)) %>%
  mutate(effect = ifelse(effect == "fixed", "Fixed Effects", "Random Effects")) %>%
  mutate(term = ifelse(term == "sd__(Intercept)", "sd(intercept)", term)) %>%
  dplyr::select(- component)

Madult_surv_mod.tidy  %<>%
  relabel_predictors("(Intercept)" = "Intercept",
                     'scale(delta_age)' = "Delta age",
                     'scale(CAge)' = "Age",
                     'scale(mean.NAO)' = "NAO",
                     'scale(pop.size)' = "Population size")

#get rid of NAs
Madult_surv_mod.tidy %<>% 
  mutate(group = ifelse(is.na(group), "", group))

dust(Madult_surv_mod.tidy) %>%
  pixiedust::sprinkle(col = 4:7, 
                      round = 2) %>% 
  sprinkle_colnames(effect = "Effect",
                    group = "Group",
                    term = "Term",
                    estimate = "Estimate", 
                    std.error = "Std error",
                    statistic = "Test-statistic",
                    p.value = "P value") %>%
  kable() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(1, bold = T) %>%
  row_spec(c(2,4,5), bold = T) %>%
  collapse_rows(columns = 1:2, valign = "top")


###SUPPLEMENTARY TABLES ------- 
#Results from each supplementary analysis in a separate table

#Adult female model -----
Fadult_surv_mod.tidy <- broom.mixed::tidy(Fadult_surv_mod.supp)

Fadult_surv_mod.tidy %<>%
  mutate(group = ifelse(group == "CapYear", "Year", group)) %>%
  mutate(group = ifelse(group == "ID", "Individual.ID", group)) %>%
  mutate(effect = ifelse(effect == "fixed", "Fixed Effects", "Random Effects")) %>%
  mutate(term = ifelse(term == "sd__(Intercept)", "sd(intercept)", term)) 

Fadult_surv_mod.tidy  %<>%
  relabel_predictors("(Intercept)" = "Intercept",
                     delta_age.z = "Delta age",
                     CAge.z = "CAge",
                     Weight.z = "Weight",
                     mean.NAO.z = "NAO",
                     pop.size.z = "Population size",
                     'delta_age.z:CAge.z' = "Delta age:CAge")

#get rid of NAs
Fadult_surv_mod.tidy %<>% 
  mutate(group = ifelse(is.na(group), "", group))

dust(Fadult_surv_mod.tidy) %>%
  pixiedust::sprinkle(col = 4:7, 
                      round = 2) %>% 
  sprinkle_colnames(effect = "Effect",
                    group = "Group",
                    term = "Term",
                    estimate = "Estimate", 
                    std.error = "Std error",
                    statistic = "Test-statistic",
                    p.value = "P value") %>%
  kable() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(1, bold = T) %>%
  row_spec(c(3,4,5,6), bold = T) %>%
  collapse_rows(columns = 1:2, valign = "top")


#Adult male model ------
Madult_surv_mod.tidy <- broom.mixed::tidy(Madult_surv_mod.supp)

Madult_surv_mod.tidy %<>%
  mutate(group = ifelse(group == "CapYear", "Year", group)) %>%
  mutate(group = ifelse(group == "ID", "Individual.ID", group)) %>%
  mutate(effect = ifelse(effect == "fixed", "Fixed Effects", "Random Effects")) %>%
  mutate(term = ifelse(term == "sd__(Intercept)", "sd(intercept)", term)) %>% dplyr::select(- component) 

Madult_surv_mod.tidy  %<>%
  relabel_predictors("(Intercept)" = "Intercept",
                     delta_age.z = "Delta age",
                     CAge.z = "CAge",
                     Weight.z = "Weight",
                     mean.NAO.z = "NAO",
                     pop.size.z = "Population size",
                     'delta_age.z:CAge.z' = "Delta age:CAge")

#get rid of NAs
Madult_surv_mod.tidy %<>% 
  mutate(group = ifelse(is.na(group), "", group))

dust(Madult_surv_mod.tidy) %>%
  pixiedust::sprinkle(col = 4:7, 
                      round = 2) %>% 
  sprinkle_colnames(effect = "Effect",
                    group = "Group",
                    term = "Term",
                    estimate = "Estimate", 
                    std.error = "Std error",
                    statistic = "Test-statistic",
                    p.value = "P value") %>%
  kable() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  column_spec(1, bold = T) %>%
  row_spec(c(3,4,5,6), bold = T) %>%
  collapse_rows(columns = 1:2, valign = "top")
