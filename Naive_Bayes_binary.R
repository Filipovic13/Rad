source("Util.R")
library(MLmetrics)


data <- readRDS(file = "final_data_binary.RDS")

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


columns_to_add <- setdiff(colnames(data), colnames(discretized_data))

transformed_data <- cbind(discretized_data, data[, columns_to_add])

transformed_data <- transformed_data[, colnames(transformed_data)]

################################## Train and Test #########################
library(caret)

set.seed(33)
train_indices <- createDataPartition(transformed_data$Diabetes, p = 0.8, list = FALSE)
train_data <- data[train_indices,]
test_data <- data[-train_indices,]

################################  1st default Imbalanced   ###########################
library(pROC)

########## NB1 : default Imbalanced #######
library(naivebayes)

nb1 <- naive_bayes(Diabetes ~ ., data = train_data)

nb1_pred <- predict(nb1, newdata = test_data, type = 'class')

head(nb1_pred)

cm1 <- table(actual = test_data$Diabetes,
             predicted = nb1_pred)
cm1
#           predicted
# actual    No   Yes
# No      36467  6273
# Yes     4160  3835

nb1_prob <- predict(nb1, newdata = test_data, type = "prob")
head(nb1_prob)

eval1 <-compute_eval_metrics(cm1, test_data$Diabetes, nb1_prob[, 2])
eval1
# precision    recall        F1     prauc 
# 0.3794025 0.4796748 0.4236867 0.3746899 


#################################### CV #############################################
 library(ROSE)
 
################### sampling: down ####################
tr_ctrl <- trainControl(method = "repeatedcv",
                        repeats = 5,
                        classProbs = TRUE,
                        summaryFunction = my_summary,
                        sampling = "down")

set.seed(33)
down <- train(x = train_data[, -19],
                y = train_data$Diabetes,
                method = "naive_bayes",
                metric = "AUC",
                trControl = tr_ctrl)

down

# ################# sampling: up ####################

tr_ctrl$sampling <- "up"

set.seed(33)
up <- train(x = train_data[, -19],
            y = train_data$Diabetes,
            method = "naive_bayes",
            metric = "AUC",
            trControl = tr_ctrl)

up

# ################# sampling: ROSE ####################

tr_ctrl$sampling <- "rose"

set.seed(33)
rose <- train(x = train_data[, -19],
              y = train_data$Diabetes,
              method = "naive_bayes",
              metric = "AUC",
              trControl = tr_ctrl)

rose


################# sampling: Original ####################


tr_ctrl$sampling <- NULL

set.seed(33)
original <- train(x = train_data[, -19],
                  y = train_data$Diabetes,
                  method = "naive_bayes",
                  metric = "AUC",
                  trControl = tr_ctrl)




models <- list(original = original,
                down = down,
                up = up,
                rose = rose)

inside_resampling <- resamples(models)
summary(inside_resampling, metric = "AUC")
###############################################################################
# Call:
#   summary.resamples(object = inside_resampling, metric = "AUC")
# 
# Models: original, down, up, rose 
# Number of resamples: 50 
# 
# AUC 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# original 0.3918966 0.3991806 0.4058161 0.4055504 0.4111961 0.4326396    0
# down     0.3921116 0.3986739 0.4056563 0.4052306 0.4110744 0.4327341    0
# up       0.3948157 0.4018808 0.4088574 0.4086354 0.4137861 0.4356612    0
# rose     0.3934671 0.4017609 0.4089385 0.4083572 0.4136724 0.4311674    0
###############################################################################


summary(inside_resampling, metric = "F")
################################################################################
# Call:
#   summary.resamples(object = inside_resampling, metric = "F")
# 
# Models: original, down, up, rose 
# Number of resamples: 50 
# 
# F 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# original 0.3109897 0.3186279 0.3243370 0.3248112 0.3300521 0.3466611    0
# down     0.4293356 0.4406327 0.4452272 0.4450023 0.4494114 0.4670407    0
# up       0.4225103 0.4352816 0.4408110 0.4406322 0.4456034 0.4658892    0
# rose     0.4440124 0.4538093 0.4574747 0.4573747 0.4603730 0.4746528    0
#################################################################################


#################################### 2nd default balanced #################################

### UPSAMPLE ###
up_balanced_train_data <- upSample(x = train_data[,-19],
                                   y = train_data$Diabetes,
                                   yname = "Diabetes")

table(up_balanced_train_data$Diabetes)
# No    Yes
# 170963 170963

########################### NB2: UPSAMPLE - default ############################

nb2 <- naive_bayes(Diabetes ~ ., data = up_balanced_train_data)

nb2_pred <- predict(nb2, newdata = test_data, type = 'class')

cm2 <- table(actual = test_data$Diabetes,
             predicted = nb2_pred)
cm2
#           predicted
# actual    No   Yes
# No      32328 10412
# Yes     2623  5372


nb2_prob <- predict(nb2, newdata = test_data, type = "prob")

eval2 <- compute_eval_metrics(cm2, test_data$Diabetes, nb2_prob[, 2])
eval2
# precision    recall        F1     prauc 
# 0.3403447 0.6719199 0.4518272 0.3748528 
 



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
#           predicted
# actual    No   Yes
# No      27424 15316
# Yes     1483  6512


eval3 <- compute_eval_metrics(cm3, y_true = test_data$Diabetes, y_pred_prob = nb2_prob[,2])
eval3
# precision    recall        F1     prauc 
# 0.2983324 0.8145091 0.4367099 0.3748528 



data.frame(rbind(eval1,eval2,eval3),row.names = paste("NB",1:3))
# precision    recall        F1     prauc
# NB 1 0.3794025 0.4796748 0.4236867 0.3746899
# NB 2 0.3403447 0.6719199 0.4518272 0.3748528
# NB 3 0.2983324 0.8145091 0.4367099 0.3748528

