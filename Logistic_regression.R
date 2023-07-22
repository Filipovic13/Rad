source("Util.r")


data <- readRDS(file = "final_data_binary.RDS")

################################## Train and Test #########################
library(caret)

set.seed(33)
train_indices <- createDataPartition(data$Diabetes, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

############################# 1st default Imbalanced ###########################



lr1 <-glm(Diabetes ~ ., data = train_data, family = 'binomial')
summary(lr1)

lr1_prob <- predict(lr1, test_data, type = "response")
head(lr1_prob)

lr1_pred <- ifelse(test = lr1_prob > 0.5, yes = "Yes", no = "No")
lr1_pred <- as.factor(lr1_pred)

cm1 <-  table(acutaul = test_data$Diabetes,
              predicted = lr1_pred)
cm1
#           predicted
# acutaul    No   Yes
# No      41616  1124
# Yes     6553  1442

eval1 <- compute_eval_metrics(cm1, test_data$Diabetes, lr1_prob)
eval1
# precision    recall        F1     prauc 
# 0.5619641 0.1803627 0.2730802 0.4408816 


#################################### CV #############################################
library(ROSE)

################### sampling: down ####################
library(glmnet)


lasso <- glmnet(x = model.matrix(data = train_data[,-19], ~ .), 
                y = train_data$Diabetes,
                family = "binomial")
plot(lasso, xvar = "lambda")

tuning_grid  <- expand.grid(alpha = 1, lambda = lasso$lambda)

tr_ctrl <- trainControl(method = "repeatedcv",
                        repeats = 5,
                        classProbs = TRUE,
                        summaryFunction = my_summary,
                        sampling = "down")

set.seed(33)
down <- train(x = model.matrix(data = train_data[,-19], ~ .),
              y = train_data$Diabetes,
              method = "glmnet", 
              family = "binomial",
              metric = "AUC",
              trControl = tr_ctrl, 
              tuneGrid = tuning_grid)

down



# ################# sampling: up ####################

tr_ctrl$sampling <- "up"

set.seed(33)
up <-  train(x = model.matrix(data = train_data[,-19], ~ .),
             y = train_data$Diabetes,
             method = "glmnet", 
             family = "binomial",
             metric = "AUC",
             trControl = tr_ctrl, 
             tuneGrid = tuning_grid)

up

# ################# sampling: ROSE ####################

# tr_ctrl$sampling <- "rose"
# 
# set.seed(33)
# rose <- train(x = model.matrix(data = train_data[,-19], ~ .),
#               y = train_data$Diabetes,
#               method = "glmnet", 
#               family = "binomial",
#               metric = "AUC",
#               trControl = tr_ctrl, 
#               tuneGrid = tuning_grid)
# 
# rose



# Something is wrong; all the AUC metric values are missing:
#   AUC        Precision       Recall          F      
# Min.   : NA   Min.   : NA   Min.   : NA   Min.   : NA  
# 1st Qu.: NA   1st Qu.: NA   1st Qu.: NA   1st Qu.: NA  
# Median : NA   Median : NA   Median : NA   Median : NA  
# Mean   :NaN   Mean   :NaN   Mean   :NaN   Mean   :NaN  
# 3rd Qu.: NA   3rd Qu.: NA   3rd Qu.: NA   3rd Qu.: NA  
# Max.   : NA   Max.   : NA   Max.   : NA   Max.   : NA  
# NA's   :67    NA's   :67    NA's   :67    NA's   :67 
# 
# Warning messages:
#   1: In train.default(x = model.matrix(data = train_data[,  ... :
#    The metric "Accuracy" was not in the result set. AUC will be used instead.
#   2: model fit failed for Fold01.Rep1: alpha=1, lambda=0.09789 Error in str2lang(x) : <text>:1:168: unexpected symbol
#   1: .y ~ HighBPNo+HighBPYes+HighCholYes+CholCheckYes+BMI+SmokerYes+StrokeYes+HeartDiseaseorAttackYes+PhysActivityYes+FruitsYes+VeggiesYes+HvyAlcoholConsumpYes+GenHlthvery good

################# sampling: Original ####################


tr_ctrl$sampling <- NULL

set.seed(33)
original <-  train(x = model.matrix(data = train_data[,-19], ~ .),
                   y = train_data$Diabetes,
                   method = "glmnet", 
                   family = "binomial",
                   metric = "AUC",
                   trControl = tr_ctrl, 
                   tuneGrid = tuning_grid)

original


models <- list(original = original,
               down = down,
               up = up #,
               #rose = rose
               )

inside_resampling <- resamples(models)

summary(inside_resampling, metric = "AUC")
###############################################################################
# Call:
#   summary.resamples(object = inside_resampling, metric = "AUC")
# 
# Models: original, down, up 
# Number of resamples: 50 
# 
# AUC 
#               Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# original 0.4139707 0.4272968 0.4312538 0.4322533 0.4382490 0.4553223    0
# down     0.4123675 0.4247341 0.4299998 0.4305502 0.4358437 0.4544349    0
# up       0.4126613 0.4249350 0.4298241 0.4306991 0.4359846 0.4538264    0
###############################################################################

summary(inside_resampling, metric = "F")
###############################################################################
# Call:
#   summary.resamples(object = inside_resampling, metric = "F")
# 
# Models: original, down, up 
# Number of resamples: 50 
# 
# F 
#               Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# original 0.2555819 0.2640134 0.2693197 0.2697260 0.2743837 0.2905054    0
# down     0.4519377 0.4630834 0.4663134 0.4661807 0.4702974 0.4786078    0
# up       0.4517786 0.4633074 0.4657254 0.4661901 0.4695074 0.4787244    0
###############################################################################

#################################### 2nd default balanced #################################


#### DOWNSAMPLE ###
set.seed(33)
down_balanced_train_data <- downSample(x = train_data[, -19],
                                       y = train_data$Diabetes,
                                       yname = "Diabetes")

table(down_balanced_train_data$Diabetes)
# No   Yes 
# 31982 31982 


### LG2: downSample - default ###

lr2 <- glm(Diabetes ~ ., data = down_balanced_train_data, family = "binomial")
summary(lr2)


lr2_prob <- predict(lr2, newdata = test_data, type = "response")
head(lr2_prob)

lr2_pred <-  ifelse(test = lr2_prob > 0.5 , yes = "Yes", no = "No")
lr2_pred <- as.factor(lr2_pred)

cm2 <- table(actual = test_data$Diabetes,
             predicted = lr2_pred)
cm2
#           predicted
# actual    No   Yes
# No      30817 11923
# Yes     1864  6131


eval2 <- compute_eval_metrics(cm2, test_data$Diabetes, lr2_prob)
eval2
# precision    recall        F1     prauc 
# 0.3395923 0.7668543 0.4707282 0.4382985 



#################################### 3rd tuned balanced #################################

best_lambda <- down$bestTune$lambda
best_lambda


lr3 <- glmnet(x = model.matrix(data = down_balanced_train_data[,-19], ~ .),
              y = down_balanced_train_data$Diabetes,
              family = "binomial",
              lambda = best_lambda)

lr3_prob <- predict(lr3, newx = model.matrix(Diabetes ~ ., data = test_data), s = best_lambda, type = "response")

lr3_pred <- ifelse(lr3_prob > 0.5, "Yes", "No")
lr3_pred <- as.factor(lr3_pred)

cm3 <- table(actual = test_data$Diabetes, predicted = lr3_pred)
cm3
#           predicted
# actual    No   Yes
# No      30833 11907
# Yes     1868  6127

# 
eval3 <- compute_eval_metrics(cm3, test_data$Diabetes, lr3_prob)
eval3
# precision    recall        F1     prauc 
# 0.3397471 0.7663540 0.4707826 0.4382511 


data.frame(rbind(eval1,eval2,eval3),row.names = paste0("lr",1:3))
#     precision    recall        F1     prauc
# lr1 0.5619641 0.1803627 0.2730802 0.4408816
# lr2 0.3395923 0.7668543 0.4707282 0.4382985
# lr3 0.3397471 0.7663540 0.4707826 0.4382511
