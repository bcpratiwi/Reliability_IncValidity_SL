# evaluation -- calculate prediction error for models and delta error (erM1 - erM2)


predError <- function(m1m2Estimates, SimData){
  
  Result <- as.data.frame(matrix(0, 3, 5))
  outcome <- c("Estimates","MSE1", "MSE2", "IV_MSE", "R2train")
  est  <- c("ridge", "least_squares", "simex" )
  
  colnames(Result) <- outcome
  Result[ , "Estimates"]  <- est
  
  
  
  list2env(m1m2Estimates, globalenv())
  list2env(SimData, globalenv())
  
  
  Result[Result$Estimates == "least_squares", "R2train"] <- deltaR2
  Result[Result$Estimates == "least_squares", "MSE1"]    <- mean((ytest -(bls[1] + Xtest.er[, 1] * bls[2]))^2) 
  Result[Result$Estimates == "least_squares", "MSE2"]    <- mean((ytest - (bls_full[1] + Xtest.er %*% bls_full[2:3]))^2)
  
 
  Result[Result$Estimates == "simex", "MSE1"] <- mean((ytest -(bsimex[1] + Xtest.er[, 1] * bsimex[2]))^2)
  Result[Result$Estimates == "simex", "MSE2"] <- mean((ytest -(bsimex_full[1] + Xtest.er %*% bsimex_full[2:3]))^2)
  
  Result[Result$Estimates == "ridge", "MSE1"] <- mean((ytest -(bridge[1] + Xtest.er[, 1] * bridge[2]))^2)
  Result[Result$Estimates == "ridge", "MSE2"] <- mean((ytest -(bridge_full[1] + Xtest.er %*% bridge_full[2:3]))^2)
  

  
  
  Result[ , "IV_MSE"] <- Result[ , "MSE1"] - Result[ , "MSE2"]
  
  return(Result)
  
  
}


