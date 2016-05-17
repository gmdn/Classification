## Compute classification performance based on the 
## coordinates of the objects and the decision line.
## Return ...
nbClassify <- function(coordinates, labels, line) {
  
  ## Objects are classified as "positive" if the inequality
  ## y < m * x + q
  ## is satisfied.
  
  ## Compute coordinates of positive and negative objects.
  yPositive <- coordinates$y[labels == 1]
  xPositive <- coordinates$x[labels == 1]
  yNegative <- coordinates$y[labels == 0]
  xNegative <- coordinates$x[labels == 0]
  
  ## Compute confusion matrix.
  truePositive  <- sum(yPositive < line$m * xPositive + line$q)
  falseNegative <- length(xPositive) - truePositive
  falsePositive <- sum(yNegative < line$m * xNegative + line$q)
  trueNegative  <- length(xNegative) - falsePositive
  
  ## Compute recall, precision and F1 measure.
  rec <- 0
  pre <- 0
  f1  <- 0
  
  ## If true postives are zero, avoid 0/0.
  if(truePositive > 0) {
    rec <- truePositive / (truePositive + falseNegative)
    pre <- truePositive / (truePositive + falsePositive)
    f1 <- 2 * rec * pre / (rec + pre)
  }
  else {
    if(falsePositive == 0) {
      pre <- 1.0
    }
  }
  
  ## Return measures.
  return(list(recall = rec, precision = pre, f1 = f1))
}