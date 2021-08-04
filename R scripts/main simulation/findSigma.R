# Function is for obtaining a covariance matrix


findSigma <- function(corr=1, sigma1=1, sigma2=1 ){
  covar <- corr*(sqrt(sigma1*sigma2))
  sigma <- matrix(c(sigma1,covar,covar,sigma2),2,2)
  return(sigma)
}
