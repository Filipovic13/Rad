
data <- readRDS(file = "final_data.RDS")

table(data$Diabetes)
levels(data$Diabetes) <- c("No",  "Yes", "Yes")
table(data$Diabetes)

ggplot(data = data,
       mapping = aes(x= Diabetes)) +
       geom_bar(position = "dodge")+
       theme_light()



################################  1st default Imbalanced   ###########################

######## Train and Test #######
library(caret)

set.seed(33)
train_indices <- createDataPartition(data$Diabetes, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


### Tree1 
library(rpart)
library(rpart.plot)

set.seed(33)
tree1 <- rpart(Diabetes ~ ., data = train_data)

tree1_pred <- predict(tree1, newdata = test_data, type = "class")

cm1 <- table(actual=test_data$Diabetes,
             predict=tree1_pred)
cm1

compute_eval_measures <- function(cm){
  TP <- cm[1,1]
  TN <- cm[2,2]
  FP <- cm[2,1]
  FN <- cm[1,2]
  a <- (TP + TN)/sum(cm)
  p <- TP/(TP + FP)
  r <- TP/(TP + FN)
  f1 <- 2*p*r/(r+p)
  c(accuaracy=a, precision=p, recall=r, F1=f1)
}

eval1 <- compute_eval_measures(cm1)

#################################################################################
#install.packages("ROSE")
library(ROSE)

tr_ctrl <- trainControl(method = "repeatedcv", repeats = 5, 
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        sampling = "down")

cp_grid <- expand.grid(.cp = seq(0.001, 0.01, 0.0025))

set.seed(33)
down <- train(x = train_data[,-19],
              y = train_data$Diabetes,
              method = "rpart",
              metric = "ROC",
              trControl = tr_ctrl,
              tuneGrid = cp_grid) 
down$bestTune$cp



tr_ctrl$sampling <- "up"

set.seed(33)
up <- train(x = train_data[,-19],
            y = train_data$Diabetes,
            method = "rpart",
            metric = "ROC",
            trControl = tr_ctrl,
            tuneGrid = cp_grid)
up$bestTune$cp


tr_ctrl$sampling <- "rose"

set.seed(33)
rose <- train(x = train_data[,-19],
            y = train_data$Diabetes,
            method = "rpart",
            metric = "ROC",
            trControl = tr_ctrl,
            tuneGrid = cp_grid)




tr_ctrl$sampling <- NULL

set.seed(33)
original <- train(x = train_data[,-19], 
                  y = train_data$Diabetes,
                  method = "rpart",
                  metric = "ROC",
                  trControl = tr_ctrl,
                  tuneGrid = cp_grid)
original$bestTune$cp

best_cp <- 0.001

models <- list(original = original,
               down = down,
               up = up,
               rose = rose)

inside_resampling <- resamples(models)
summary(inside_resampling, metric = "ROC")

# Call:
#   summary.resamples(object = inside_resampling, metric = "ROC")
# 
# Models: original, down, up, rose 
# Number of resamples: 50 
# 
# ROC 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# original 0.6969897 0.7148407 0.7196613 0.7211747 0.7296718 0.7374346    0
# down     0.7549067 0.7693298 0.7785918 0.7757327 0.7817193 0.7909056    0
# up       0.7549618 0.7646936 0.7675204 0.7697824 0.7767960 0.7874405    0
# rose     0.7549285 0.7663463 0.7726934 0.7736282 0.7826455 0.7903989    0
##################################################################################


### downsample
set.seed(33)
down_balanced_train_data <- downSample(x = train_data[, -19],
                                       y = train_data$Diabetes)

colnames(down_balanced_train_data)[19] <- "Diabetes"
down_balanced_train_data = down_balanced_train_data[sample(1:nrow(down_balanced_train_data)), ]

table(down_balanced_train_data$Diabetes)


### rose

rose_balanced_train_data <- ROSE(Diabetes~., data=train_data, seed=33)$data

table(rose_balanced_train_data$Diabetes)
ggplot(data = rose_balanced_train_data,
       mapping = aes(x= Diabetes)) +
  geom_bar(position = "dodge")+
  theme_light()



################################   2nd default balanced  ####################################

###Tree2: downSample - default
set.seed(33)
tree2 <- rpart(formula = Diabetes ~ ., 
               data = down_balanced_train_data)
rpart.plot(tree2, extra = 104)

tree2$control$cp

tree2_pred <- predict(tree2, newdata = test_data, type = 'class')

cm2 <- table(actual=test_data$Diabetes,
             prediceted= tree2_pred)
cm2


eval2 <- compute_eval_measures(cm2)

################################################################################
#Tree3: rose - default
set.seed(33)
tree3 <- rpart(formula = Diabetes ~ ., 
               data = rose_balanced_train_data)
rpart.plot(tree3, extra = 104)

tree3$control$cp

tree3_pred <- predict(tree3, newdata = test_data, type = 'class')

cm3 <- table(actual=test_data$Diabetes,
             prediceted= tree3_pred)
cm3


eval3 <- compute_eval_measures(cm3)



######################## 3rd - Tuned Balanced #######################


tr_ctrl <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 5)

cp_grid <- expand.grid(.cp = seq(0.001, 0.1, 0.0005))



####Tree4: downsample - tuned
set.seed(33)
tree4_cv <- train(x = down_balanced_train_data[,-19],
                  y = down_balanced_train_data$Diabetes,
                  method = 'rpart',
                  trControl = tr_ctrl,
                  tuneGrid = cp_grid)
tree4_cv$bestTune$cp


set.seed(33)
tree4 <- rpart(formula = Diabetes ~ ., 
               data = down_balanced_train_data,
               control = rpart.control(cp =best_cp))
rpart.plot(tree4, extra = 104)

tree4_pred <- predict(tree4, newdata = test_data, type = 'class')

cm4 <- table(actual=test_data$Diabetes,
             prediceted= tree4_pred)
cm4


eval4 <- compute_eval_measures(cm4)
################################################################



####Tree5: rose - tuned
set.seed(33)
tree5_cv <- train(x = rose_balanced_train_data[,-19],
                  y = rose_balanced_train_data$Diabetes,
                  method = 'rpart',
                  trControl = tr_ctrl,
                  tuneGrid = cp_grid)
tree5_cv$bestTune$cp



####Tree5: rose - tuned
set.seed(33)
tree5 <- rpart(formula = Diabetes ~ ., 
               data = down_balanced_train_data,
               control = rpart.control(cp =best_cp))
rpart.plot(tree5, extra = 104)

tree5_pred <- predict(tree5, newdata = test_data, type = 'class')

cm5 <- table(actual=test_data$Diabetes,
             prediceted= tree5_pred)
cm5


eval5 <- compute_eval_measures(cm5)



###############################################
data.frame(rbind(eval1,eval2,eval4,eval3,eval5),row.names = c("imbalanced","down default","down tuned","rose default","rose tuned"))


