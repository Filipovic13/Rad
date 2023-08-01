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

cm1 <-  table(actual = test_data$Diabetes,
              predicted = lr1_pred)
cm1
#           predicted
# actual    No   Yes
# No      41616  1124
# Yes     6553  1442

eval1 <- compute_eval_metrics(cm1, test_data$Diabetes, lr1_prob)
eval1
# precision    recall        F1     prauc 
# 0.5619641 0.1803627 0.2730802 0.4408816 


######################## CV: lasso regression - alpha = 1 ######################


################### sampling: down ####################
library(glmnet)


x <- model.matrix(data = train_data[,-19], ~ .)
y <- train_data$Diabetes

set.seed(33)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)

tuning_grid  <- expand.grid(alpha = 1, lambda = c(cv.lasso$lambda.min, cv.lasso$lambda.1se))

tr_ctrl <- trainControl(method = "repeatedcv",
                        repeats = 5,
                        classProbs = TRUE,
                        summaryFunction = my_summary,
                        sampling = "down")

set.seed(33)
down <- train(x = x,
              y = y, 
              method = "glmnet", 
              family = "binomial",
              metric =  "F", 
              trControl = tr_ctrl, 
              tuneGrid = tuning_grid)

down



# ################# sampling: up ####################

tr_ctrl$sampling <- "up"

set.seed(33)
up <-  train(x = x, 
             y = y, 
             method = "glmnet", 
             family = "binomial",
             metric = "F", 
             trControl = tr_ctrl, 
             tuneGrid = tuning_grid)

up

# ################# sampling: ROSE ####################
#library(ROSE)
#
# tr_ctrl$sampling <- "rose"
# 
# set.seed(33)
# rose <- train(x = x, 
#               y = y, 
#               method = "glmnet",
#               family = "binomial",
#               metric = "F",
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
original <-  train(x = x,
                   y = y, 
                   method = "glmnet", 
                   family = "binomial",
                   metric = "F", 
                   trControl = tr_ctrl, 
                   tuneGrid = tuning_grid)

original

################# sampling: Original Weighted ####################

fraction_No <- rep(1 - sum(y == "No") / length(y), sum(y == "No"))
fraction_Yes <- rep(1 - sum(y == "Yes") / length(y), sum(y == "Yes"))
weights <- numeric(length(y))
weights[y == "No"] <- fraction_No
weights[y == "Yes"] <- fraction_Yes

set.seed(33)
original_weighted <-  train(x = x,
                            y = y, 
                            method = "glmnet", 
                            family = "binomial",
                            weights = weights,
                            metric = "F",
                            trControl = tr_ctrl, 
                            tuneGrid = tuning_grid)

original_weighted

#############################################################################

models <- list(original = original,
               original_w = original_weighted,
               down = down,
               up = up)

inside_resampling <- resamples(models)


#  
###############################################################################







######################## CV: ridge regression - alpha = 0 ######################


################### sampling: down ####################

set.seed(33)
cv.ridge  <- cv.glmnet(x, y, alpha = 0, family = "binomial")
plot(cv.ridge)

tuning_grid_0  <- expand.grid(alpha = 0, lambda = c(cv.ridge$lambda.min, cv.ridge$lambda.1se))

tr_ctrl$sampling <- "down"

set.seed(33)
down_0 <- train(x = x,
                y = y, 
                method = "glmnet", 
                family = "binomial",
                metric =  "F", 
                trControl = tr_ctrl, 
                tuneGrid = tuning_grid_0)

down_0



# ################# sampling: up ####################

tr_ctrl$sampling <- "up"

set.seed(33)
up_0 <-  train(x = x, 
               y = y, 
               method = "glmnet", 
               family = "binomial",
               metric = "F", 
               trControl = tr_ctrl, 
               tuneGrid = tuning_grid_0)

up_0


################# sampling: Original ####################

tr_ctrl$sampling <- NULL

set.seed(33)
original_0 <-  train(x = x,
                   y = y, 
                   method = "glmnet", 
                   family = "binomial",
                   metric = "F", 
                   trControl = tr_ctrl, 
                   tuneGrid = tuning_grid_0)

original_0

################# sampling: Original Weighted ####################

set.seed(33)
original_weighted_0 <-  train(x = x,
                            y = y, 
                            method = "glmnet", 
                            family = "binomial",
                            weights = weights,
                            metric = "F",
                            trControl = tr_ctrl, 
                            tuneGrid = tuning_grid_0)

original_weighted_0

#############################################################################

models_0 <- list(original_0 = original_0,
                 original_w_0 = original_weighted_0,
                 down_0 = down_0,
                 up_0 = up_0)

inside_resampling_0 <- resamples(models_0)



######################## CV: elastic net regression: - alpha = 0.5 ######################


################### sampling: down ####################

set.seed(33)
cv.elastic.net  <- cv.glmnet(x, y, alpha = 0.5, family = "binomial")
plot(cv.elastic.net)

tuning_grid_en  <- expand.grid(alpha = 0.5, lambda = c(cv.elastic.net$lambda.min, cv.elastic.net$lambda.1se))

tr_ctrl$sampling <- "down"

set.seed(33)
down_en <- train(x = x,
                y = y, 
                method = "glmnet", 
                family = "binomial",
                metric =  "F", 
                trControl = tr_ctrl, 
                tuneGrid = tuning_grid_en)

down_en



# ################# sampling: up ####################

tr_ctrl$sampling <- "up"

set.seed(33)
up_en <-  train(x = x, 
               y = y, 
               method = "glmnet", 
               family = "binomial",
               metric = "F", 
               trControl = tr_ctrl, 
               tuneGrid = tuning_grid_en)

up_en


################# sampling: Original ####################

tr_ctrl$sampling <- NULL

set.seed(33)
original_en <-  train(x = x,
                     y = y, 
                     method = "glmnet", 
                     family = "binomial",
                     metric = "F", 
                     trControl = tr_ctrl, 
                     tuneGrid = tuning_grid_en)

original_en

################# sampling: Original Weighted ####################

set.seed(33)
original_weighted_en <-  train(x = x,
                              y = y, 
                              method = "glmnet", 
                              family = "binomial",
                              weights = weights,
                              metric = "F",
                              trControl = tr_ctrl, 
                              tuneGrid = tuning_grid_en)

original_weighted_en

#############################################################################

models_en <- list(original_en = original_en,
                 original_w_en = original_weighted_en,
                 down_en = down_en,
                 up_en = up_en)

inside_resampling_en <- resamples(models_en)








##################### Res: Lasso, Ridge, Elastic Net #######################################

#Lasso
summary(inside_resampling, metric = "F")
###############################################################################
# Call:
#   summary.resamples(object = inside_resampling, metric = "F")
# 
# Models: original, original_w, down, up 
# Number of resamples: 50 
# 
# F 
#                   Min.    1st Qu.    Median      Mean    3rd Qu.      Max.     NA's
# original    0.2559962  0.2641691  0.2694615  0.2699257  0.2746592  0.2908404    0  
# original_w  0.4529457 +0.4636828  0.4662084  0.4667243  0.4703126  0.4768889    0   
# down       +0.4534197  0.4636276 +0.4665151  0.4665812 +0.4710319 +0.4783740    0   
# up          0.4527401  0.4636457  0.4659518 +0.4667377  0.4707652  0.4776781    0   
# 


#Ridge
summary(inside_resampling_0, metric = "F")
# Call:
#   summary.resamples(object = inside_resampling_0, metric = "F")
# 
# Models: original_0, original_w_0, down_0, up_0 
# Number of resamples: 50 
# 
# F 
#                     Min.    1st Qu.    Median      Mean    3rd Qu.     Max.     NA's
# original_0    0.2299976  0.2408989  0.2460215  0.2458969  0.2516872  0.2631068    0
# original_w_0  0.4541063  0.4644100  0.4675712  0.4676178  0.4703802  0.4797727    0
# down_0        0.4534951  0.4642218  0.4673703  0.4675133  0.4711691  0.4795218    0
# up_0         +0.4541156 +0.4644161 +0.4676762 +0.4677178 +0.4712128 +0.4798980    0  <-----

#Elasttic net
summary(inside_resampling_en, metric = "F")
# Call:
#   summary.resamples(object = inside_resampling_en, metric = "F")
# 
# Models: original_en, original_w_en, down_en, up_en 
# Number of resamples: 50 
# 
# F 
#                     Min.   1st Qu.    Median      Mean      3rd Qu.      Max.    NA's
# original_en    0.2552281  0.2638778  0.2702791  0.2698693  0.2746101  0.2907032    0
# original_w_en +0.4538233 +0.4643709  0.4668969 +0.4671240  0.4707900  0.4782315    0
# down_en        0.4532799  0.4641330 +0.4673123  0.4669144 +0.4712410 +0.4790662    0
# up_en          0.4531474  0.4641203  0.4665874  0.4671043  0.4706447  0.4778454    0





#################################### 2nd balanced tuned #################################


#### UPSAMPLE ###
set.seed(33)
up_balanced_train_data <- upSample(x = train_data[, -19],
                                      y = train_data$Diabetes,
                                      yname = "Diabetes")

table(up_balanced_train_data$Diabetes)
# No    Yes 
# 170963 170963 


### LR2: upSample - tuned ###

best_lambda <- down_0$bestTune$lambda
best_lambda

lr2 <- down_0$finalModel

lr2_prob <- predict(lr2, newx = model.matrix(data = test_data, Diabetes ~ .), s = best_lambda, type = 'response')

lr2_pred <- ifelse(lr2_prob > 0.5, "Yes", "No")
lr2_pred <- as.factor(lr2_pred)



cm2 <- table(actual = test_data$Diabetes,
             predicted = lr2_pred)
cm2
#           predicted
# actual    No   Yes
# No      31077 11663
# Yes     1942  6053


eval2 <- compute_eval_metrics(cm2, test_data$Diabetes, lr2_prob)
eval2
# precision    recall        F1     prauc 
# 0.3416685 0.7570982 0.4708491 0.4386014  


#################################### 3rd threshold changed #################################

library(pROC)


predicted_probabilities <- cbind(1 - lr2_prob, lr2_prob)
colnames(predicted_probabilities) <- c("No", "Yes")
head(predicted_probabilities)

length(predicted_probabilities[,2])
length(test_data$Diabetes)



lr2_roc <- roc(response = as.integer(test_data$Diabetes),predictor = predicted_probabilities[ ,2])
lr2_roc$auc



plot.roc(lr2_roc,
         print.thres = 'best',
         print.thres.best.method = "closest.topleft")


closest_topleft_coords <- coords(lr2_roc,
                                 x='best',
                                 best.method = "closest.topleft",
                                 ret=c("threshold","sensitivity"),
                                 transpose=FALSE)

closest_topleft_threshold <- closest_topleft_coords[1,1]
closest_topleft_threshold


lr2_pred2 <- ifelse(test=predicted_probabilities[ , 2] > closest_topleft_threshold, yes="Yes",no="No")
lr2_pred2 <- as.factor(lr2_pred2)


cm3 <- table(actual = test_data$Diabetes, 
             predicted = lr2_pred2)
cm3
# actual    No   Yes
# No     31043 11697
# Yes    1927  6068

eval3 <-  compute_eval_metrics(cm3, test_data$Diabetes, lr2_prob)
eval3
# precision    recall        F1     prauc 
# 0.3415705 0.7589744 0.4711180 0.4386014 

##############################################

plot.roc(lr2_roc,
         print.thres = 'best',
         print.thres.best.method = "youden")


youden_coords <- coords(lr2_roc,
                        x='best',
                        best.method = "youden",
                        ret=c("threshold","sensitivity"),
                        transpose=FALSE)

youdent_threshold <- youden_coords[1,1]
youdent_threshold


lr2_pred3 <- ifelse(test=predicted_probabilities[ , 2] > youdent_threshold, yes="Yes",no="No")
lr2_pred3 <- as.factor(lr2_pred3)


cm4 <- table(actual = test_data$Diabetes, 
             predicted = lr2_pred3)
cm4
# actual    No   Yes
# No  28526 14214
# Yes  1440  6555


eval4 <-  compute_eval_metrics(cm4, test_data$Diabetes, lr2_prob)
eval4
# precision    recall        F1     prauc 
# 0.3156146 0.8198874 0.4557781 0.4386014 

###################################################################




data.frame(rbind(eval1,eval2,eval3, eval4),row.names = paste0("lr",1:4))
#     precision    recall        F1     prauc
# lr1 0.5619641 0.1803627 0.2730802 0.4408816   default imblanced
# lr2 0.3416685 0.7570982 0.4708491 0.4386014   cv ridge
# lr3 0.3415705 0.7589744 0.4711180 0.4386014   closest_topleft_threshold
# lr4 0.3156146 0.8198874 0.4557781 0.4386014   youdent_threshold

