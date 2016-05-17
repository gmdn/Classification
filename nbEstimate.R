nbEstimate <- function(training, trainingLabels, prior = c(1, 1)) {
  
  ## Take positive and negative examples.
  pos <- training[trainingLabels == 1, ]
  neg <- training[trainingLabels == 0, ]
  
  ## If there is only one positive documents, transpose
  ## vector to obtain a matrix 1 x n.
  if(sum(trainingLabels == 1) == 1) {
    pos <- t(pos)
  }

  ## Beta prior (hyper-)parameters.
  a <- prior[1]
  b <- prior[2]

  ## Initialize frequency counts.
  freq <- rep(0, dim(pos)[2])
  
  ####### positive class #######
  ## Compute number of positive documents.
  numOfPositiveDocs <- dim(pos)[1]
  
  ## Compute features frequencies.
  freq <- colSums(pos)

  ## Add smoothing parameters.
  num <- freq + a
  den <- numOfPositiveDocs + a + b
  
  ## Estimate parameters of positive class.
  estPos <- num / den
  
  ####### negative class #######
  ## Compute features frequencies.
  freq <- colSums(neg)
  
  ## Compute number of negative documents.
  numOfNegativeDocs <- dim(neg)[1]
  
  ## Add smoothing parameters.
  num <- freq + a
  den <- numOfNegativeDocs + a + b
  
  ## Estimate parameters of negatives class.
  estNeg <- num / den
  
  ## Return a list of two vectors.
  return(list(positive = estPos, negative = estNeg))
  
}