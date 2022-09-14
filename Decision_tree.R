
data <- readRDS(file = "final_data.RDS")

summary(data)

library(rpart)
library(rpart.plot)


############################## 1st default Imbalanced #####################################



######## Train and Test #######

library(caret)

set.seed(33)
train_indices <- createDataPartition(data$Diabetes, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


### Tree1 
set.seed(33)
tree1 <- rpart(Diabetes ~ ., data = train_data)

tree1_pred <- predict(tree1, newdata = test_data, type = "class")

cm1 <- table(actual=test_data$Diabetes,
             predict=tree1_pred)
cm1

compute_eval_measures <- function(cm){
  
  total_TP <- 0
  total_FP <- 0
  total_FN <- 0
  micro_F1 <- 0
  macro_F1 <- 0
  weighted_F1 <- 0
  sum_ <- 0
  
  df <- data.frame()
  
  for (class_ in 1:3)
  {
    TP <- 0
    FN <- 0
    FP <- 0
    
    for (i in 1:3)
    {
      for (j in 1:3)
      {
        
        if (class_ == i & i == j ) {
          TP <-  cm[i,j]
        }
        
        if (j == class_ & i != j) {
          FN <-  FN + cm[i,j]
        }
        
        if (i == class_ & i != j) {
          FP <- FP + cm[i,j]
        }
        
      }
      
    }
    
    ### precsion , recall, f1
    a <- round(sum(diag(cm)/sum(cm)),2)
    p <- round(TP/(TP + FP),2)
    r <- round(TP/(TP + FN),2)
    f1 <- round(2*p*r/(r+p),2)
    
    v <- c(a, p, r, f1)
    
    
    df <- rbind(df,v)
    
    
    
    
    #### micro f1
    total_TP <- total_TP + TP
    total_FP <- total_FP + FP
    total_FN <- total_FN + FN
    
    tot_P <- total_TP/(total_TP + total_FP)
    tot_R <- total_TP/(total_TP + total_FN)
    micro_F1 <- round(2 * tot_P * tot_R/(tot_P+tot_R),2)
    
    
    
    ### Macro F1
    macro_F1 <- macro_F1 + f1
    
    
    ### Weighted F1
    
    weighted_F1 <- weighted_F1 + (f1*sum(cm[,class_]))
    
  }
  
  ### Weighted F1
  sum_  <- sum(cm[,1]) + sum(cm[,2]) + sum(cm[,3])
  weighted_F1 <- round(weighted_F1/sum_,2)
  
  
  macro_F1 <-round(macro_F1/3,2) 
  
  df[nrow(df) + 1,] <- c("###", "###", "###", "###")
  df[nrow(df) + 1,] <- c("", "", "", micro_F1)
  df[nrow(df) + 1,] <- c("", "", "", macro_F1)
  df[nrow(df) + 1,] <- c("", "", "", weighted_F1)
  
  rownames(df) <- c("No", "Pre", "Yes","###", "Micro_F1", "Macro_F1","Weighted_F1")
  colnames(df) <- c("Accuracy", "Precision", "Recall", "F1")
  return(df)
  
}


eval1 <- compute_eval_measures(cm1)



#################################### 2nd default balanced #################################

tr_ctrl <- trainControl(method = "repeatedcv", repeats = 5, 
                        classProbs = TRUE,
                        summaryFunction = multiClassSummary,
                        sampling = "down")

cp_grid <- expand.grid(.cp = seq(0.001, 0.01, 0.0025))

set.seed(33)
down <- train(x = train_data[,-19],
              y = train_data$Diabetes,
              method = "rpart",
              metric = "prAUC",
              trControl = tr_ctrl,
              tuneGrid = cp_grid) 
down$bestTune$cp



tr_ctrl$sampling <- "up"

set.seed(33)
up <- train(x = train_data[,-19],
            y = train_data$Diabetes,
            method = "rpart",
            metric = "prAUC",
            trControl = tr_ctrl,
            tuneGrid = cp_grid)
up$bestTune$cp



tr_ctrl$sampling <- NULL

set.seed(33)
original <- train(x = train_data[,-19], 
                  y = train_data$Diabetes,
                  method = "rpart",
                  metric = "prAUC",
                  trControl = tr_ctrl,
                  tuneGrid = cp_grid)
original$bestTune$cp

best_cp <- 0.001

models <- list(original = original,
               down = down,
               up = up)

inside_resampling <- resamples(models)
summary(inside_resampling, metric = "prAUC")
# 
# Call:
#   summary.resamples(object = inside_resampling, metric = "prAUC")
# 
# Models: original, down, up 
# Number of resamples: 50 
# 
# prAUC 
#                 Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# original 0.2139329 0.2182342 0.2208237 0.2203819 0.2229718 0.2256323    0
# down     0.2649725 0.3159778 0.3229105 0.3413587 0.3333754 0.4382137    0
# up       0.2849296 0.3135349 0.3185886 0.3205220 0.3261735 0.4304269    0



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



eval1 #tree1: default imbalanced
eval2 #tree2: default balanced-downSample cp=0.01
eval3 #tree3: tuned   balanced-downSample cp=0.001
