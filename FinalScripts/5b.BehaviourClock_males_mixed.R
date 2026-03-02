#### 5.a Behaviour Data - male - MIXED ####
## NOTE: this has been updated to have the new behaviour metrics 
## 7/11/25

#### Set Up ####
library(tidyverse)
library(glmnet)

#### Data ####
BirthPheno_recode <- read_csv("OutputData/TransformedData/BirthPheno_recode.csv")
male_behaviour <- read_csv("OutputData/TransformedData/male_behaviour_zNEW.csv")

## Reformatting to match
male_behaviour <- male_behaviour %>% 
  mutate(ID = id,
         CapYear = year) %>% 
  select(-id, -year)

#### Dropping NAs For Now ####
maleData <- left_join(male_behaviour, BirthPheno_recode, by = "ID")

maleData <- maleData %>% 
  mutate(AgeY = CapYear - BirthYear)

male_clockData_UNCLEAN <- maleData %>% 
  select(ID, AgeY, 
         home.range.size.z,
         mm.deg.z, mm.stre.z, mm.eigen.z, mm.clust.z, mm.betw.z, mm.close.z, mm.part.coef.z, mm.mean.stre.z, mm.mean.gs.z, mm.soc.sel.z, 
         
         os.deg.z, os.stre.z, os.mean.stre.z, os.soc.sel.z
  ) 


male_clockData <- na.omit(male_clockData_UNCLEAN)

rm(male_clockData_UNCLEAN)


#### LOAOCV and Elastic Net Set Up ####

ID_list <- as.vector(unique(male_clockData$ID))
n <- length(ID_list)

nonZeroVariable_names <- NA

# note that this needs to be in the same order as the columns going into the elastic net otherwise there will be mismatch
variable_names <- c("home.range.size.z",
                    "mm.deg.z", "mm.stre.z", "mm.eigen.z", "mm.clust.z", "mm.betw.z", "mm.close.z", "mm.part.coef.z", "mm.mean.stre.z", "mm.mean.gs.z", "mm.soc.sel.z", 
                    "os.deg.z", "os.stre.z", "os.mean.stre.z", "os.soc.sel.z"
)

set.seed(123)

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

#### The LOOCV Loop ####
for (i in 1:n) {
  #1 Setting up the training and test data (one id in test)
  training <- male_clockData %>% filter(ID != ID_list[i]) %>% dplyr::select(-ID)
  test <- male_clockData %>% filter(ID == ID_list[i]) %>% dplyr::select(-ID)
  
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
  
  #7 Collating the non zero coemmicients
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
                                 error = testAge - testAgePRED,
                                 sq_error = (testAge - testAgePRED)^2,
                                 ab_error = abs(testAge - testAgePRED)) %>% 
  drop_na()

# pulling together the frequency of each coemmicient
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
       title = "Behaviour Clock males (mixed interactions)") +
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
predictors <- as.matrix(male_clockData %>% dplyr::select(-AgeY, -ID))
response <- as.matrix(male_clockData %>% dplyr::select(AgeY))

# fitting the model
glmnet_cv <- cv.glmnet(predictors, response, alpha = 0.5, type.measure = "mse", standardise = FALSE)

#storing the lambda min 
full_lambda.min <- glmnet_cv$lambda.min

#running again for the lambda min
full_glmnet <- glmnet(predictors, response, alpha = 0.5, nlambda = 100, standardize = FALSE)

# collecting up the non-zero CpGs and their coemmicients
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

#### Writing Outputs ####
 #  write_csv(fullModelCoef, "OutputData/FinalModelData/BehaviourClock/m_fullModelCoef.csv")
 # # 
 #  write_csv(LOAOCV_predictions, "OutputData/FinalModelData/BehaviourClock/m_predictions.csv")
 # 