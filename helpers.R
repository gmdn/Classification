## Compute a coordinate, either x (positive) or y (negative).
## Return a vector of coordinates.
computeCoordinate <- function(dataset, theta, thetaClass) {
  
  ## Formula to compute a coordinate of an object:
  ## (Bernoulli version)
  ## \sum_i (x_i * (log(theta_i) - log(1 - theta_i))) + 
  ##      + sum_i (log(1 - theta_i))
  ##      + log(theta_class)
  
  ## Compute coordinates of the positive class.
  logTheta <- log(theta)
  logOneMinusTheta <- log(1 - theta)
  ## Use proportion of positive objects to compute
  ## the probability of the class.
  logThetaClass <- log(thetaClass)
  
  coordinate <- dataset %*% (logTheta - logOneMinusTheta) +
    sum(logOneMinusTheta) +
    logThetaClass
  
  return(coordinate)
  
}

## Compute new samples.
