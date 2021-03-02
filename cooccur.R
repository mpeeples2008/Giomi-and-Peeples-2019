## Co-occurrence assessment script
## This function expects a binary data frame object that contains only 1s and 0s with the contexts under consideration as rows and the 
## categories as columns and each cell represents the presence or absence of a particular category in a particular context

cooccur <- function(x) {
  
  # calculate the proportional occurrence of each artifact class
  nm.p <- colSums(x)/nrow(x) 
  
  # calculated observed co-occurrences through matrix multiplication
  obs <- t(as.matrix(x)) %*% (as.matrix(x)) 
  diag(obs) <- 0
  
  # create matrix of expected values based on proportional occurrence 
  expect <- matrix(0,nrow(obs),ncol(obs)) 
  for (i in 1:nrow(obs)) {
    for (j in 1:ncol(obs)) {
      expect[i,j] <- (nm.p[i]*nm.p[j])*nrow(x)}} 
  
  # convert expected count to expected proportion
  p <- expect/nrow(x)
  
  # calculate final matrix of scores and output
  out <- (obs-expect)/(sqrt(expect*(1-p))) 
  diag(out) <- 0
  return(out)}

## end code
