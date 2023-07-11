library(pROC)

data <- readRDS(file = "final_data.RDS")

table(data$Diabetes)
# No    Pre    Yes
# 213703   4631  35346

levels(data$Diabetes) <- c("No",  "Yes", "Yes")
table(data$Diabetes)
# No    Yes
# 213703  39977



####################  Numerical columns distibution  ###########################

summary(data)
numerical_columns <- c(4, 13, 14)


apply(data[sample(nrow(data), 5000), numerical_columns],
      MARGIN = 2 ,
      FUN = shapiro.test)
# BMI p < 0.05
# MentHlth p < 0.05
# PhysHlth p < 0.05

#############################  Discretize  #####################################
library(ggplot2)

ggplot(data = data,
       aes(x = BMI)) +
  geom_histogram() +
  theme_minimal()

ggplot(data = data,
       aes(x = MentHlth)) +
  geom_histogram() +
  theme_minimal()

ggplot(data = data,
       aes(x = PhysHlth)) +
  geom_histogram() +
  theme_minimal()


library(bnlearn)

discretized_data <- discretize(data = data[, numerical_columns],
                               method = "interval",
                               breaks = 2)

summary(discretized_data)


columns_to_add <-
  setdiff(colnames(data), colnames(discretized_data))

transformed_data <- cbind(discretized_data, data[, columns_to_add])

transformed_data <- transformed_data[, colnames(transformed_data)]

################################## Train and Test #########################
library(caret)

set.seed(33)
train_indices <-
  createDataPartition(transformed_data$Diabetes, p = 0.8, list = FALSE)
train_data <- data[train_indices,]
test_data <- data[-train_indices,]

################################  1st default Imbalanced   ###########################


########## NB1 : default Imbalanced #######
library(e1071)

nb1 <- naiveBayes(Diabetes ~ ., data = train_data)

nb1_pred <- predict(nb1, newdata = test_data, type = 'class')

head(nb1_pred)

cm1 <- table(actual = test_data$Diabetes,
             predicted = nb1_pred)
cm1
#           predicted
# actual    No   Yes
# No      36467  6273
# Yes     4160  3835

nb1_prob <- predict(nb1, newdata = test_data, type = "raw")
head(nb1_prob)

compute_eval_metrics <- function(cm, y_true, y_pred_prob){
  
  tree1_roc = roc(y_true, y_pred_prob)
  auc = auc(tree1_roc)
  
  TP <- cm[2,2] 
  TN <- cm[1,1] 
  FP <- cm[1,2] 
  FN <- cm[2,1]
  
  a <- (TP + TN)/sum(cm)
  p <- TP/(TP + FP)
  r <- TP/(TP + FN)
  f1 <- 2*p*r/(r+p)
  
  c(precision=p, recall=r, F1=f1, AUC = auc)
}

eval1 <-compute_eval_metrics(cm1, test_data$Diabetes, nb1_prob[, 2])
eval1
# precision    recall        F1       AUC 
# 0.3794025 0.4796748 0.4236867 0.7934715 

#################################### CV #############################################
# library(ROSE)
# 
# ################# sampling: down ####################
# tr_ctrl <- trainControl(method = "repeatedcv",
#                         repeats = 5,
#                         classProbs = TRUE,
#                         summaryFunction = prSummary,
#                         sampling = "down")
# 
# set.seed(33)
# down <- train(x = train_data[, -19],
#                 y = train_data$Diabetes,
#                 method = "nb",
#                 metric = "AUC",
#                 trControl = tr_ctrl)
# 
# 
# 
# ################# sampling: up ####################
# 
# tr_ctrl$sampling <- "up"
# 
# set.seed(33)
# up <- train(x = train_data[, -19],
#             y = train_data$Diabetes,
#             method = "nb",
#             metric = "AUC",
#             trControl = tr_ctrl)
# 
# ################# sampling: ROSE ####################
# 
# tr_ctrl$sampling <- "rose"
# ? caret::train()
# set.seed(33)
# rose <- train(x = train_data[, -19],
#               y = train_data$Diabetes,
#               method = "nb",
#               metric = "AUC",
#               trControl = tr_ctrl)
# 
# 
# 
# 
# ################# sampling: Original ####################
# 
# 
# tr_ctrl$sampling <- NULL
# 
# set.seed(33)
# original <- train(x = train_data[, -19],
#                   y = train_data$Diabetes,
#                   method = "nb",
#                   metric = "AUC",
#                   trControl = tr_ctrl)
# 
# 
# 
# 
# models <- list(original = original,
#                 down = down,
#                 up = up,
#                 rose = rose)
# 
# inside_resampling <- resamples(models)
# summary(inside_resampling, metric = "AUC")

################################################################################
# Call:
#   summary.resamples(object = inside_resampling, metric = "AUC")
#
# Models: original, down, up, rose
# Number of resamples: 50
#
# AUC
#               Min.   1st Qu.    Median      Mean   3rd Qu.      Max.   NA's
# original 0.9500466 0.9529065 0.9535987 0.9534848 0.9542256 0.9564392    0
# down     0.9499679 0.9529450 0.9536358 0.9535132 0.9542540 0.9565705    0
# up       0.9507336 0.9535391 0.9542217 0.9541446 0.9549626 0.9571069    0     <----- up
# rose     0.9507305 0.9532142 0.9539003 0.9539190 0.9547318 0.9571405    0     <-
################################################################################


#################################### 2nd default balanced #################################

### UPSAMPLE ###
up_balanced_train_data <- upSample(x = train_data[,-19],
                                   y = train_data$Diabetes,
                                   yname = "Diabetes")

table(up_balanced_train_data$Diabetes)
# No    Yes
# 170963 170963

########################### NB2: UPSAMPLE - default ############################

nb2 <- naiveBayes(Diabetes ~ ., data = up_balanced_train_data)

nb2_pred <- predict(nb2, newdata = test_data, type = 'class')

cm2 <- table(actual = test_data$Diabetes,
             predicted = nb2_pred)
cm2
#           predicted
# actual    No   Yes
# No        32318 10422
# Yes       2620  5375


nb2_prob <- predict(nb2, newdata = test_data, type = "raw")

eval2 <- compute_eval_metrics(cm2, test_data$Diabetes, nb2_prob[, 2])
eval2
# precision    recall        F1       AUC 
# 0.3402545 0.6722952 0.4518325 0.7933573 



########################### NB3: UPSAMPLE  ####################################

nb2_roc <- roc(response = as.integer(test_data$Diabetes),
               predictor = nb2_prob[, 2])

plot.roc(nb2_roc,
         print.thres = 'best',
         print.thres.best.method = "youden")

threshold_youden <- 0.209

nb2_pred2 <- ifelse(test = nb2_prob[, 2] > threshold_youden,
                     yes = "Yes",
                       no = "No")
nb2_pred2 <- as.factor(nb2_pred2)


cm3 <- table(actual = test_data$Diabetes,
             predicted = nb2_pred2)
cm3
#             predicted
# actual    No   Yes
# No      27355 15385
# Yes     1475  6520


eval3 <- compute_eval_metrics(cm3, y_true = test_data$Diabetes, y_pred_prob = nb2_prob[,2])
eval3
# precision    recall        F1       AUC
# 0.2976489 0.8155097 0.4361204 0.7933573 


data.frame(rbind(eval1,eval2,eval3),row.names = paste("NB",1:3))
#     precision    recall        F1       AUC
# NB 1 0.3794025 0.4796748 0.4236867 0.7934715
# NB 2 0.3402545 0.6722952 0.4518325 0.7933573
# NB 3 0.2976489 0.8155097 0.4361204 0.7933573

