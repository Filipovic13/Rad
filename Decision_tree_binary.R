library(pROC)
library(MLmetrics)

data <- readRDS(file = "final_data.RDS")

table(data$Diabetes)
# No    Pre    Yes 
# 213703   4631  35346 

levels(data$Diabetes) <- c("No",  "Yes", "Yes")
table(data$Diabetes)
# No    Yes 
# 213703  39977 

library(ggplot2)
ggplot(data = data,
       mapping = aes(x= Diabetes)) +
       geom_bar(position = "dodge")+
       theme_light()





################################## Train and Test #########################
library(caret)

set.seed(33)
train_indices <- createDataPartition(data$Diabetes, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

################################  1st default Imbalanced   ###########################

########### Tree1 ######   default Imbalanced
library(rpart)
library(rpart.plot)

set.seed(33)
tree1 <- rpart(Diabetes ~ ., data = train_data)
tree1$control$cp
# 0.01


tree1_pred <- predict(tree1, newdata = test_data, type = "class")


cm1 <- table(actual=test_data$Diabetes,
             predict=tree1_pred)
cm1
#           predict
# actual    No   Yes
# No      42740     0
# Yes      7995     0


tree1_prob <- predict(tree1, newdata = test_data, type = "prob")


compute_eval_metrics <- function(cm, y_true, y_pred_prob){

  prAUC = PRAUC(y_pred_prob, y_true)[1]
  
  TP <- cm[2,2] 
  TN <- cm[1,1] 
  FP <- cm[1,2] 
  FN <- cm[2,1]
  
  a <- (TP + TN)/sum(cm)
  p <- TP/(TP + FP)
  r <- TP/(TP + FN)
  f1 <- 2*p*r/(r+p)
  
  c(precision=p, recall=r, F1=f1, prauc = prAUC)
}

eval1 <- compute_eval_metrics(cm1, test_data$Diabetes, tree1_prob[,2])
eval1
# precision    recall        F1     prauc 
# NaN             0         NaN        0 


#################################### CV #############################################
#install.packages("ROSE")
library(ROSE)

################# sampling: down ####################
tr_ctrl <- trainControl(method = "repeatedcv", repeats = 5, 
                        classProbs = TRUE,
                        summaryFunction = prSummary,
                        sampling = "down")

cp_grid <- expand.grid(.cp = seq(0.001, 0.01, 0.0025))

set.seed(33)
down <- train(x = train_data[,-19],
              y = train_data$Diabetes,
              method = "rpart",
              metric = "AUC",
              trControl = tr_ctrl,
              tuneGrid = cp_grid) 
down$bestTune$cp
# 0.001


################# sampling: up ####################

tr_ctrl$sampling <- "up"

set.seed(33)
up <- train(x = train_data[,-19],
            y = train_data$Diabetes,
            method = "rpart",
            metric = "AUC",
            trControl = tr_ctrl,
            tuneGrid = cp_grid)
# Warning message:
#   In UseMethod("depth") :
#   no applicable method for 'depth' applied to an object of class "NULL"
up$bestTune$cp
# 0.001

################# sampling: ROSE ####################

# tr_ctrl$sampling <- "rose"
# 
# set.seed(33)
# rose <- train(x = train_data[,-19],
#             y = train_data$Diabetes,
#             method = "rpart",
#             metric = "AUC",
#             trControl = tr_ctrl,
#             tuneGrid = cp_grid)
# rose$bestTune$cp
# 0.001



################# sampling: Original ####################

# 
# tr_ctrl$sampling <- NULL
# 
# set.seed(33)
# original <- train(x = train_data[,-19], 
#                   y = train_data$Diabetes,
#                   method = "rpart",
#                   metric = "AUC",
#                   trControl = tr_ctrl,
#                   tuneGrid = cp_grid)
# original$bestTune$cp
# 0.001

best_cp <- 0.001

# models <- list(original = original,
#                down = down,
#                up = up,
#                rose = rose)
# 
# inside_resampling <- resamples(models)
# summary(inside_resampling, metric = "AUC")
#################################################################################
# Call:
#   summary.resamples(object = inside_resampling, metric = "AUC")
# 
# Models: original, down, up, rose 
# Number of resamples: 50 
# 
# AUC 
#               Min.   1st Qu.    Median      Mean   3rd Qu.      Max.   NA's
# original 0.3248479 0.3313818 0.3335803 0.3333812 0.3352307 0.3421177    0
# down     0.5283582 0.5336645 0.5354586 0.5588453 0.5378858 0.9296631    0      <----- down
# up       0.5284311 0.5333845 0.5345331 0.5582875 0.5372531 0.9351010    0      <--    up
# rose     0.5277206 0.5330426 0.5351985 0.5401585 0.5370996 0.6255246    0
################################################################################

#################################### 2nd default balanced #################################


#### DOWNSAMPLE ###
set.seed(33)
down_balanced_train_data <- downSample(x = train_data[, -19],
                                       y = train_data$Diabetes)
colnames(down_balanced_train_data)[19] <- "Diabetes"
table(down_balanced_train_data$Diabetes)
# No   Yes 
# 31982 31982 


########################### Tree2: downSample - default ##############################
set.seed(33)
tree2 <- rpart(formula = Diabetes ~ ., 
               data = down_balanced_train_data)
rpart.plot(tree2, extra = 104)

tree2$control$cp
# 0.01

tree2_pred <- predict(tree2, newdata = test_data, type = 'class')

cm2 <- table(actual=test_data$Diabetes,
             prediceted= tree2_pred)
cm2
#           prediceted
# actual    No   Yes
# No      29046 13694
# Yes     1834  6161


tree2_prob <-  predict(tree2, newdata = test_data, type = 'prob')
# 
eval2 <- compute_eval_metrics(cm2, test_data$Diabetes, tree2_prob[,2])
eval2
# precision    recall        F1     prauc 
# 0.3102997 0.7706066 0.4424417 0.1132496  





### UPSAMPLE ###
up_balanced_train_data <- upSample(x = train_data[, -19],
                                   y = train_data$Diabetes,
                                   yname = "Diabetes")

table(up_balanced_train_data$Diabetes)
# No    Yes 
# 170963 170963


########################### Tree3: UPSAMPLE - default ##############################


tree3 <- rpart(Diabetes ~ .,
               data =  up_balanced_train_data)

rpart.plot(tree3, extra = 104)

tree3_pred <- predict(tree3, newdata = test_data, type = "class")
cm3 <- table(actual = test_data$Diabetes,
      predicted = tree3_pred)
cm3
#           predicted
# actual    No   Yes
# No      28769 13971
# Yes     1877  6118


tree3_prob <- predict(tree3, newdata = test_data, type = "prob")

eval3 <- compute_eval_metrics(cm3, test_data$Diabetes, tree3_prob[,2])
eval3
# precision    recall        F1     prauc 
# 0.3045448 0.7652283 0.4356929 0.1111096


################################  3rd - Tuned Balanced   ###########################

######## Tree4: Balanced Downsample  #########

tree4 <-down$finalModel

tree4$tuneValue
# 0.001

rpart.plot(tree4, extra = 104)

tree4_pred <- predict(tree4,newdata = test_data, type = "class")

cm4 <- table(actual = test_data$Diabetes,
             predicted = tree4_pred)
cm4
#           predicted
# actual    No   Yes
# No        30131 12609
# Yes       1930  6065


tree4_prob <- predict(tree4, newdata = test_data, type = "prob")

eval4 <- compute_eval_metrics(cm4, test_data$Diabetes, tree4_prob[,2])
eval4
# precision    recall        F1     prauc 
# 0.3247831 0.7585991 0.4548352 0.1149385 



################################  Balanced Rose   ###########################


########### Tree5: Balanced upSample ######


tree5 <- up$finalModel

rpart.plot(tree5, extra = 104)

tree5_pred <- predict(tree5, newdata = test_data, type = "class")

cm5 <- table(actual = test_data$Diabetes,
             predicted = tree5_pred)
cm5
#           predicted
# actual    No   Yes
# No        28811 13929
# Yes       1649  6346


tree5_prob <- predict(tree5, newdata = test_data, type = "prob")

eval5 <- compute_eval_metrics(cm5, test_data$Diabetes, tree5_prob[,2])
eval5
# precision    recall        F1     prauc 
# 0.3129963 0.7937461 0.4489565 0.1164702 


####################################################################

data.frame(rbind(eval2,eval4),row.names = c("down default","up default"))
# precision    recall        F1     prauc
# down default 0.3102997 0.7706066 0.4424417 0.1132496
# up default   0.3247831 0.7585991 0.4548352 0.1149385


data.frame(rbind(eval3,eval5),row.names = c("down tuned","up tuned"))
# precision    recall        F1     prauc
# down tuned 0.3045448 0.7652283 0.4356929 0.1111096
# up tuned   0.3129963 0.7937461 0.4489565 0.1164702




