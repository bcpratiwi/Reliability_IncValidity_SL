# least squares MSE in the training set  -----------------

ls_method_train <- function(SimData){
  
  # extract relevant data for analysis 
  y <- SimData$y
  Xer <- SimData$Xe
  
  m1 <- lm(y ~ Xer[, 1])
  m2 <- lm(y ~ Xer)
  
  MSEtrain_M1 <- mean((fitted(m1) - y)^2)
  MSEtrain_M2 <- mean((fitted(m2) - y)^2)
  IVtrain <- MSEtrain_M1 - MSEtrain_M2
  
  return(list(MSEtrain_M1, MSEtrain_M2, IVtrain))
  
}


