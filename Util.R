library(MLmetrics)

my_summary <- function(data, lev = NULL, model = NULL) {
  classes <- c("Yes", "No")
  prSummary(data, lev = classes)
}


compute_eval_metrics <- function(cm, y_true, y_pred_prob){
  
  prAUC = PRAUC(y_pred_prob, y_true) 
  
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