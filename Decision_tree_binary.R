source("Util.R")
library(MLmetrics)

data <- readRDS(file = "final_data_binary.RDS")

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

eval1 <- compute_eval_metrics(cm1, test_data$Diabetes, tree1_prob[,2])
eval1
# precision    recall        F1     prauc 
# NaN             0         NaN        0 


#################################### CV #############################################



################# sampling: down ####################

tr_ctrl <- trainControl(method = "repeatedcv", repeats = 5, 
                        classProbs = TRUE,
                        summaryFunction = my_summary,
                        sampling = "down")

cp_grid <- expand.grid(.cp = seq(0.001, 0.01, 0.0025))

set.seed(33)
down <- train(x = train_data[,-19],
              y = train_data$Diabetes,
              method = "rpart",
              metric = "F",
              trControl = tr_ctrl,
              tuneGrid = cp_grid)

down 

down$bestTune$cp
# 0.001


################# sampling: up ####################

tr_ctrl$sampling <- "up"

set.seed(33)
up <- train(x = train_data[,-19],
            y = train_data$Diabetes,
            method = "rpart",
            metric = "F",
            trControl = tr_ctrl,
            tuneGrid = cp_grid)
up 

up$bestTune$cp
# 0.001

################# sampling: ROSE ####################
library(ROSE)

tr_ctrl$sampling <- "rose"

set.seed(33)
rose <- train(x = train_data[,-19],
            y = train_data$Diabetes,
            method = "rpart",
            metric = "F",
            trControl = tr_ctrl,
            tuneGrid = cp_grid)

rose

rose$bestTune$cp
#0.001



################# sampling: Original ####################


tr_ctrl$sampling <- NULL

set.seed(33)
original <- train(x = train_data[,-19],
                  y = train_data$Diabetes,
                  method = "rpart",
                  metric = "F",
                  trControl = tr_ctrl,
                  tuneGrid = cp_grid)

original

original$bestTune$cp
# 0.001

best_cp <- 0.001

models <- list(original = original,
               down = down,
               up = up,
               rose = rose)

inside_resampling <- resamples(models)
summary(inside_resampling, metric = "F")
#################################################################################
# Call:
#   summary.resamples(object = inside_resampling, metric = "F")
# 
# Models: original, down, up, rose 
# Number of resamples: 50 
# 
# F 
#               Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# original 0.1993834 0.2396913 0.2613308 0.2491320 0.2662214 0.2857810   17
# down     0.4372370 0.4465046 0.4518636 0.4507007 0.4543538 0.4678397    0     <---
# up       0.4329628 0.4446448 0.4498972 0.4490391 0.4531891 0.4618847    0
# rose     0.4356583 0.4483498 0.4509018 0.4518355 0.4573591 0.4669839    0     <---
# ################################################################################

#################################### 2nd default balanced #################################


#### DOWNSAMPLE ###
set.seed(33)
down_balanced_train_data <- downSample(x = train_data[, -19],
                                       y = train_data$Diabetes)
colnames(down_balanced_train_data)[19] <- "Diabetes"
table(down_balanced_train_data$Diabetes)
# No   Yes 
# 31982 31982 

#### Tree2: downSample - default ###
set.seed(33)
tree2 <- rpart(formula = Diabetes ~ ., data = down_balanced_train_data)
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

eval2 <- compute_eval_metrics(cm2, test_data$Diabetes, tree2_prob[,2])
eval2
# precision    recall        F1     prauc 
# 0.3102997 0.7706066 0.4424417 0.1132496  





### ROSE  ###
rose_balanced_train_data <-  ROSE(Diabetes ~ ., data = train_data, seed = 33)$data

table(rose_balanced_train_data$Diabetes)
# No    Yes 
# 101655 101290 


########################### Tree3: ROSE (Random Over Sampling Examples) - default ##############################


tree3 <- rpart(Diabetes ~ ., data =  rose_balanced_train_data)

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



########### Tree5: Balanced ROSE ######


tree5 <- rose$finalModel

rpart.plot(tree5, extra = 104)

tree5_pred <- predict(tree5, newdata = test_data, type = "class")

cm5 <- table(actual = test_data$Diabetes,
             predicted = tree5_pred)
cm5
#         predicted
# actual    No   Yes
# No      31795 10945
# Yes     2275  5720


tree5_prob <- predict(tree5, newdata = test_data, type = "prob")

eval5 <- compute_eval_metrics(cm5, test_data$Diabetes, tree5_prob[,2])
eval5
# precision    recall        F1     prauc 
# 0.3432343 0.7154472 0.4639092 0.1903192 


####################################################################

data.frame(rbind(eval2,eval3),row.names = c("down default","rose default"))
#             precision    recall        F1     prauc
# down default 0.3102997 0.7706066 0.4424417 0.1132496
# rose default 0.3045448 0.7652283 0.4356929 0.1111096


data.frame(rbind(eval4,eval5),row.names = c("down tuned","rose tuned"))
#             precision    recall        F1     prauc
# down tuned 0.3247831 0.7585991 0.4548352 0.1149385
# rose tuned 0.3432343 0.7154472 0.4639092 0.1903192




