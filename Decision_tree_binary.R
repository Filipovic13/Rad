
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

################################   2nd default balanced  ####################################


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
               up = up)

inside_resampling <- resamples(models)
summary(inside_resampling, metric = "ROC")

# Call:
#   summary.resamples(object = inside_resampling, metric = "ROC")
# 
# Models: original, down, up 
# Number of resamples: 50 
# 
# ROC 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# original 0.6969897 0.7148407 0.7196613 0.7211747 0.7296718 0.7374346    0
# down     0.7549067 0.7693298 0.7785918 0.7757327 0.7817193 0.7909056    0
# up       0.7549618 0.7646936 0.7675204 0.7697824 0.7767960 0.7874405    0


set.seed(33)
down_balanced_train_data <- downSample(x = train_data[, -19],
                                       y = train_data$Diabetes)

colnames(down_balanced_train_data)[19] <- "Diabetes"
down_balanced_train_data = down_balanced_train_data[sample(1:nrow(down_balanced_train_data)), ]

table(down_balanced_train_data$Diabetes)



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



######################## 3rd - Tuned Balanced #######################

tr_ctrl <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 5)

cp_grid <- expand.grid(.cp = seq(0.001, 0.1, 0.0005))

set.seed(33)
tree3_cv <- train(x = down_balanced_train_data[,-19],
                  y = down_balanced_train_data$Diabetes,
                  method = 'rpart',
                  trControl = tr_ctrl,
                  tuneGrid = cp_grid)
tree3_cv$bestTune$cp



####Tree3: downsample - tuned
set.seed(33)
tree3 <- rpart(formula = Diabetes ~ ., 
               data = down_balanced_train_data,
               control = rpart.control(cp =best_cp))
rpart.plot(tree3, extra = 104)

tree3_pred <- predict(tree3, newdata = test_data, type = 'class')

cm3 <- table(actual=test_data$Diabetes,
             prediceted= tree3_pred)
cm3


eval3 <- compute_eval_measures(cm3)


data.frame(rbind(eval1,eval2,eval3),row.names = c("Imbalanced","Balanced default","Balanced tuned"))


