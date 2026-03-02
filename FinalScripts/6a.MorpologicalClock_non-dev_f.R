#### 30.a Sheet Data - Female ####

## FILTERED to only have individuals that are 2 and over

#### Set Up ####
library(tidyverse)
library(glmnet)

#### Data ####
BirthPheno_recode <- read_csv("OutputData/TransformedData/BirthPheno_recode.csv")
female_sheet <- read_csv("OutputData/TransformedData/female_sheet_z_NEW.csv")

#### Dropping NAs For Now ####
femaleData <- left_join(female_sheet, BirthPheno_recode, by = "ID")

femaleData <- femaleData %>% 
  mutate(AgeY = CapYear - BirthYear)

female_clockData_UNCLEAN <- femaleData %>% 
  select(AgeY, ID, UnshedWool_recode, Scouring_recode, TeethDeform_recode, Milk_recode, Weight.z, ForeLeg.z, Teeth.z, Keds.z) 


female_clockData <- na.omit(female_clockData_UNCLEAN)

rm(female_clockData_UNCLEAN)


female_clockData <- female_clockData %>%  filter(AgeY >= 2)
#### LOAOCV and Elastic Net Set Up ####

ID_list <- as.vector(unique(female_clockData$ID))
n <- length(ID_list)

nonZeroVariable_names <- NA
variable_names <- c("UnshedWool_recode", "Scouring_recode", "TeethDeform_recode", "Milk_recode", "Weight.z", "ForeLeg.z", "Teeth.z", "Keds.z")

set.seed(123)

i <- NA

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

#### The LOOACV Loop ####
for (i in 1:n) {
  #1 Setting up the training and test data (one id in test)
  training <- female_clockData %>% filter(ID != ID_list[i]) %>% dplyr::select(-ID)
  test <- female_clockData %>% filter(ID == ID_list[i]) %>% dplyr::select(-ID)
  
  #2 Setting up response and predictors
  predictors <- as.matrix(training %>% dplyr::select(-AgeY))
  response <- as.matrix(training %>% dplyr::select(AgeY))
  
  #3 Fitting the CV model
  TRAIN_glmnet_cv <- cv.glmnet(predictors, response, alpha = 0.5, type.measure = "mse", standardise = FALSE) 
  
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
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "Age (years)",
       y = "Predicted Age (years)",
       title = "Sheet Clock Females Adults") +
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


#### Plotting the coefficient selection ####
plot(full_glmnet, label = TRUE)
abline(v = -log(glmnet_cv$lambda.min), col = "red", lty = 2)
abline(v = -log(glmnet_cv$lambda.1se), col = "blue", lty = 2)

#### Writing Outputs ####
#write_csv(fullModelCoef, "OutputData/FinalModelData/SheetClock_Adults/f_fullModelCoef.csv")

#write_csv(LOAOCV_predictions, "OutputData/FinalModelData/SheetClock_Adults/f_predictions.csv")
