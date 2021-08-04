# function to generate simulation data ----
# rho1 and rho2 reliabilities of test1 and test 2
# ratio to determine the beta coefficients
# N sample size of the training set
# R2 effect size of the true model (full model)
# r12 collinearity
# Returns X = True Scores in training set (test 1 and test 2)
#         Xer = Observed Scores in training set
#         y = outcome variable in training set
#         ytest outcome variable in validation set
#         Xtest = True Scores in validation set 
#         Xtest.er = Observed Scores in validation set

p1GenData <- function(rho1, rho2, ratio, N, R2, r12){
  
  ntest <- 10000
  
  if(ratio == "2:1"){
    
    b1 <- sqrt(R2 / ((5/4) + r12))
    b2 <- .5*b1
  
    } else if(ratio == "1:1"){
    
     b1 <- sqrt(R2/(2+2*r12))
     b2 <- b1
  
    } else if(ratio == "1:2"){
    
      b1 <- sqrt(R2/(5+ 4*r12))
      b2 <- 2*b1
    
    } else {
      b1 <- sqrt(R2)
      b2 <- 0
      
    } 
  
  betas <- c(b1, b2)
  
  er1  <- (1-rho1)/rho1
  er2  <- (1-rho2)/rho2
  var_names <- c("x1", "x2")
  
  X <- mvrnorm(N, c(0,0), Sigma = findSigma(r12))
  y <- X%*%as.numeric(betas)  + rnorm(N, sd = sqrt(1- R2))
  
  Xtest <- mvrnorm(ntest, c(0,0), Sigma = findSigma(r12))
  ytest <- Xtest%*%as.numeric(betas)  + rnorm(ntest, sd = sqrt(1- R2))
  
  
  Xer <- X + cbind(rnorm(N, sd = sqrt(er1)),
                   rnorm(N, sd = sqrt(er2)))
  Xtest.er <- Xtest + cbind(rnorm(ntest, sd = sqrt(er1)),
                            rnorm(ntest, sd = sqrt(er2)))
  
  colnames(X)        <- var_names
  colnames(Xer)      <- var_names
  colnames(Xtest.er) <- var_names
  
  return(list(X = X, Xer = Xer, Xtest = Xtest, Xtest.er = Xtest.er, y = y, ytest = ytest, er1 = er1, er2 = er2))
  
}