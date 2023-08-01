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
                metric = "F",
                trControl = tr_ctrl)

down

# ################# sampling: up ####################

tr_ctrl$sampling <- "up"

set.seed(33)
up <- train(x = train_data[, -19],
            y = train_data$Diabetes,
            method = "naive_bayes",
            metric = "F",
            trControl = tr_ctrl)

up

# ################# sampling: ROSE ####################
library(ROSE)

tr_ctrl$sampling <- "rose"

set.seed(33)
rose <- train(x = train_data[, -19],
              y = train_data$Diabetes,
              method = "naive_bayes",
              metric = "F",
              trControl = tr_ctrl)

rose


################# sampling: Original ####################


tr_ctrl$sampling <- NULL

set.seed(33)
original <- train(x = train_data[, -19],
                  y = train_data$Diabetes,
                  method = "naive_bayes",
                  metric = "F",
                  trControl = tr_ctrl)




models <- list(original = original,
                down = down,
                up = up,
                rose = rose)

inside_resampling <- resamples(models)

summary(inside_resampling, metric = "F")
################################################################################
# Call:
# summary.resamples(object = inside_resampling, metric = "F")
# 
# Models: original, down, up, rose 
# Number of resamples: 50 
# 
# F 
#                Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# original 0.4035235 0.4203839 0.4255562 0.4247110 0.4288846 0.4499724    0     
# down     0.4339402 0.4435962 0.4479491 0.4470015 0.4502284 0.4615708    0
# up       0.4334431 0.4439844 0.4479506 0.4469056 0.4502843 0.4601770    0
# rose     0.4440124 0.4538093 0.4574747 0.4573747 0.4603730 0.4746528    0     <------
#################################################################################


#################################### 2nd default balanced #################################

### ROSE  (Random Over Sampling Examples) ###
rose_balanced_train_data <- ROSE(Diabetes ~ ., data = train_data, seed = 33)$data

table(rose_balanced_train_data$Diabetes)
# No    Yes 
# 101655 101290 

########################### NB2: ROSE - default ############################

nb2 <- naive_bayes(Diabetes ~ ., data = rose_balanced_train_data)

nb2_pred <- predict(nb2, newdata = test_data, type = 'class')

cm2 <- table(actual = test_data$Diabetes,
             predicted = nb2_pred)
cm2
#           predicted
# actual    No   Yes
# No      32386 10354
# Yes     2636  5359


nb2_prob <- predict(nb2, newdata = test_data, type = "prob")

eval2 <- compute_eval_metrics(cm2, test_data$Diabetes, nb2_prob[, 2])
eval2
# precision    recall        F1     prauc 
# 0.3410552 0.6702939 0.4520837 0.3746172 
 



########################### NB3: ROSE threshold changed  ####################################

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
# No      27420 15320
# Yes     1484  6511


eval3 <- compute_eval_metrics(cm3, y_true = test_data$Diabetes, y_pred_prob = nb2_prob[,2])
eval3
# precision    recall        F1     prauc 
# 0.2982456 0.8143840 0.4365989 0.3746172  



data.frame(rbind(eval1,eval2,eval3),row.names = paste("NB",1:3))
#     precision    recall        F1     prauc
# NB 1 0.3794025 0.4796748 0.4236867 0.3746899
# NB 2 0.3410552 0.6702939 0.4520837 0.3746172
# NB 3 0.2982456 0.8143840 0.4365989 0.3746172

