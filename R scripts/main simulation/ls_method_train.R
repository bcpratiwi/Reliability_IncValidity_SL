# least squares MSE in the training set  -----------------

ls_method_train <- function(SimData){
  
  # extract relevant data for analysis 
  y <- SimData$y
  Xer <- SimData$Xe
  
  m1 <- lm(y ~ Xer[, 1])
  m2 <- lm(y ~ Xer)
  
  bls <- coef(m1)
  bls_full <- coef(m2)
  
  MSEtrain_M1 <- mean((fitted(m1) - y)^2)
  MSEtrain_M2 <- mean((fitted(m2) - y)^2)
  IVtrain <- MSEtrain_M1 - MSEtrain_M2
  
  IVR2 <- summary(m2)$r.squared - summary(m1)$r.squared
  
  return(list(bls, bls_full, MSEtrain_M1, MSEtrain_M2, IVtrain, 
              R2train_M1=summary(m1)$r.squared, R2train_M2=summary(m2)$r.squared,
              IVR2train=IVR2))
  
}


