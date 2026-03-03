#### 8ai. PhenotypeClock - Female #####

## this version contains all the possible phenotype measures from the different data sources
## this does come at the sacrifice of number of samples

## NOTE THIS VERSION HAS THE UPDATED BEHAVIOUR METRICS

#### Set Up ####
library(tidyverse)
library(glmnet)

#### Data ####
BirthPheno_recode <- read_csv("OutputData/TransformedData/BirthPheno_recode.csv")

female_sheet <- read_csv("OutputData/TransformedData/female_sheet_z_NEW.csv")
female_behaviour <- read_csv("OutputData/TransformedData/female_behaviour_zNEW.csv")
female_sample_z <- read_csv("OutputData/TransformedData/female_sample_z_NEW.csv")

# reformatting behaviour
female_behaviour <- female_behaviour %>% 
  mutate(ID = id,
         CapYear = year) %>% 
  select(-id, -year)

#### Combining and dropping incomplete ####
female_data <- full_join(female_sheet, female_behaviour)
female_data <- full_join(female_data, female_sample_z)

female_clockData <- left_join(female_data, BirthPheno_recode, by = "ID")

female_clockData <- female_clockData %>% 
  mutate(AgeY = CapYear - BirthYear) %>% 
  select(ID, AgeY,
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

female_clockData <- na.omit(female_clockData)

#### LOAOCV and Elastic Net Set Up ####

ID_list <- as.vector(unique(female_clockData$ID))
n <- length(ID_list)

nonZeroVariable_names <- NA

variable_names <- c(
  #sheet data
  "UnshedWool_recode", "Scouring_recode", "TeethDeform_recode", "Milk_recode", 
  "Weight.z", "ForeLeg.z", "Teeth.z", "Keds.z", 
  
  #behaviour data
  "home.range.size.z",
  "ff.deg.z", "ff.stre.z", "ff.eigen.z", "ff.clust.z", "ff.betw.z", "ff.close.z", "ff.part.coef.z", "ff.mean.stre.z", "ff.mean.gs.z", "ff.soc.sel.z",
  
  "os.deg.z", "os.stre.z", "os.mean.stre.z", "os.soc.sel.z",
  
  # sample data
  "IgA.z", "IgE.z", "IgG.z",
  "RTL.z",
  "Strongyles.z", "Coccidea.z"
)

set.seed(999)

i <- NA

set.alpha <- 0.5

training <- NA
test <- NA
predictors <- NA
response <- NA

## model set up
TRAIN_glmnet_CV <- NA
coef.lambda.min <- NA
run <- NA
testAge <- NA
testAgePRED <- NA
lambda.min <- NA
coef_min <- NA
nonZero <- NA
testID <- NA
nonZero_names_list <- list(NA)


for (i in 1:n) {
  #1 Setting up the training and test data (one id in test)
  training <- female_clockData %>% filter(ID != ID_list[i]) %>% dplyr::select(-ID)
  test <- female_clockData %>% filter(ID == ID_list[i]) %>% dplyr::select(-ID)
  
  #2 Setting up response and predictors
  predictors <- as.matrix(training %>% dplyr::select(-AgeY))
  response <- as.matrix(training %>% dplyr::select(AgeY))
  
  #3 Fitting the CV model
  TRAIN_glmnet_cv <- cv.glmnet(predictors, response, alpha = 0.5, type.measure = "mse", standardise = TRUE) 
  
  #4 storing the lambda min for the run
  lambda.min[i] <- TRAIN_glmnet_cv$lambda.min
  
  #5 running again for the lambda min
  TRAIN_glmnet <- glmnet(predictors, response, alpha = 0.5, nlambda = 100, standardise = FALSE)
  coef.lambda.min <- coef(TRAIN_glmnet)[, TRAIN_glmnet$lambda == lambda.min[i]]
  
  #6 Predicting the age(s) for the left out individual
  AgePrediction <- predict(TRAIN_glmnet, newx = (as.matrix(test %>% dplyr::select(-AgeY))), type = "response", s = lambda.min[i])
  
  #7 Collating the non zero coefficients
  coef_min <- coef(TRAIN_glmnet, s = lambda.min[i])
  nonZero[i] <- sum(coef_min[-1,] != 0)
  
  nonZero_names <- variable_names[coef_min[-1,] != 0]
  nonZero_names_list[[i]] <- nonZero_names
  
  #8 saving the outputs
  run[i] <- i
  testAge <- append(testAge, test$AgeY)
  testAgePRED <- append(testAgePRED, as.numeric(AgePrediction))
  testID <- c(testID, rep(ID_list[i], times = (length(AgePrediction))))
  
  # update the progress bar
  print(paste0(i, " of ", n, " at ", now()))
}

# collating output
LOAOCV_output <- data.frame(runNumber = run,
                            lambda.min = lambda.min,
                            nonZero = nonZero,
                            testID = ID_list)
LOAOCV_predictions <- data.frame(testID = testID,
                                 testAgeY = testAge,
                                 testAgeYPRED = testAgePRED,
                                 error = testAgePRED - testAge,
                                 sq_error = (testAgePRED - testAge)^2,
                                 ab_error = abs(testAgePRED - testAge)) %>% 
  drop_na()

# pulling together the frequency of each coefficient
variable_freq <- as.data.frame(sort(table(unlist(nonZero_names_list)), decreasing = TRUE))
colnames(variable_freq) <- c("Variable", "Freq")

MeanSE <- mean(LOAOCV_predictions$sq_error)
MedianAbsoluteError <- median(LOAOCV_predictions$ab_error)
LOAOCV_correlation <- cor.test(LOAOCV_predictions$testAgeY, LOAOCV_predictions$testAgeYPRED)


ggplot(LOAOCV_predictions, aes(x = testAgeY, y = testAgeYPRED)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") +
  labs(x = "Age (Years)",
       y = "Predicted Age (Years)",
       title = "Phenotype Clock Females") +
  xlim(-1, 15) +
  ylim(-1, 15) +
  geom_abline(intercept = 0, slope = 1, colour = "pink", linetype = 2) +
  theme_bw()

ggplot(variable_freq, aes(x = Variable, y = Freq)) +
  geom_col() +
  labs(x = "Variable",
       y = "Selection Frequency") +
  geom_text(data = variable_freq, aes(x = Variable, y = (Freq + 2), angle = 45),
            label = paste("n = ", variable_freq$Freq)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(LOAOCV_output, aes(x = nonZero)) +
  geom_bar() +
  labs(x = "Number of Selected Variables") +
  theme_bw()


##### Final Model ####
#setting up predictors and response
predictors <- as.matrix(female_clockData %>% dplyr::select(-AgeY, -ID))
response <- as.matrix(female_clockData %>% dplyr::select(AgeY))

# fitting the model
glmnet_cv <- cv.glmnet(predictors, response, alpha = 0.5, type.measure = "mse", standardise = FALSE)

#storing the lambda min 
full_lambda.min <- glmnet_cv$lambda.min

#running again for the lambda min
full_glmnet <- glmnet(predictors, response, alpha = 0.5, nlambda = 100, standardize = FALSE)

# collecting up the non-zero CpGs and their coefficients
fullModelCoef <- coef(full_glmnet, s = full_lambda.min) %>%
  as.matrix() %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Variables") 

names(fullModelCoef) <- c("Variable", "Coefficient")

fullModelCoef <- fullModelCoef  %>% 
  filter(Coefficient != 0) %>% 
  mutate(direction = if_else(Coefficient > 0, "positive", "negative"))

ggplot(fullModelCoef %>% 
         filter(Variable != "(Intercept)"),
       aes(x = Variable, y = Coefficient, fill = direction)) +
  geom_bar(stat = "identity") +
  theme(axis.ticks.y = element_blank()) +
  coord_flip()


#### Plotting error by age ####

ggplot(data = LOAOCV_predictions, aes(x = testAgeY, y = error)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(x = "Chronological Age (years)",
       y = "Prediction Error (years)",
       title = "Female Phenotype Clock Error By Age") +
  theme_bw()
  
#### Writing Outputs ####
#write_csv(fullModelCoef, "OutputData/FinalModelData/PhenotypeClock/f_fullModelCoef.csv")

#write_csv(LOAOCV_predictions, "OutputData/FinalModelData/PhenotypeClock/f_predictions.csv")

plot(full_glmnet, label = TRUE)
abline(v = -log(full_lambda.min), col = "red", lty = 2)
abline(v = -log(glmnet_cv$lambda.1se), col = "blue", lty = 2)
