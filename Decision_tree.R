
data <- readRDS(file = "final_data.RDS")

summary(data)

library(rpart)
library(rpart.plot)


                            ### 1st default unbalanced ###



######## Train and Test #######

library(caret)
set.seed(33)
train_indices <- createDataPartition(data$Diabetes, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]


### Tree1

set.seed(33)
tree1 <- rpart(Diabetes ~ ., data = train_data)
tree1

rpart.plot(tree1, extra =104)



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
  
  rownames(df) <- c("No", "Pre", "Yes","###", "MicroF1", "MacroF1","WeightedF1")
  colnames(df) <- c("Accuracy", "Precision", "Recall", "F1")
  return(df)
  
}


eval1 <- compute_eval_measures(cm1)
eval1




                           ### 2nd default balanced ###

library(caret)
#remotes::install_github("cran/DMwR")
library(DMwR)
#install.packages("ROSE")
library(ROSE)



tr_ctrl <- trainControl(method = "repeatedcv", repeats = 5, 
                        classProbs = TRUE,
                        summaryFunction = multiClassSummary,
                       sampling = "down")

set.seed(33)
down <- train(Diabetes ~ ., data = train_data,
                     method = "rpart",
                     metric = "Mean_F1",
                     trControl = tr_ctrl)



tr_ctrl$sampling <- "up"

set.seed(33)
up <- train(Diabetes ~ ., data = train_data,
                   method = "rpart",
                   metric = "Mean_F1",
                   trControl = tr_ctrl)



tr_ctrl$sampling <- "rose"

set.seed(33)
rose <- train(Diabetes ~ ., data = train_data,
                     method = "rpart",
                     metric = "Mean_F1",
                     trControl = tr_ctrl)



tr_ctrl$sampling <- "smote"

set.seed(33)
smote <- train(Diabetes ~ ., data = train_data,
                      method = "rpart",
                      metric = "Mean_F1",
                      trControl = tr_ctrl)



