# evaluation -- calculate prediction error for models and delta error (erM1 - erM2)


predR2 <- function(m1m2Estimates, SimData){
  
  Result <- as.data.frame(matrix(0, 3, 4)) # removed R2train in outcome
  outcome <- c("Estimates","R2_M1", "R2_M2", "IV_R2")
  est  <- c("ridge", "least_squares", "simex" )
  
  colnames(Result) <- outcome
  Result[ , "Estimates"]  <- est
  
  list2env(m1m2Estimates, globalenv())
  list2env(SimData, globalenv())
  
  
#  Result[Result$Estimates == "least_squares", "R2train"] <- deltaR2
  Result[Result$Estimates == "least_squares", "R2_M1"]    <- cor(ytest, (bls[1] + Xtest.er[, 1] * bls[2]))^2
  Result[Result$Estimates == "least_squares", "R2_M2"]    <- cor(ytest, (bls_full[1] + Xtest.er %*% bls_full[2:3]))^2
  
  Result[Result$Estimates == "simex", "R2_M1"] <- cor(ytest, (bsimex[1] + Xtest.er[, 1] * bsimex[2]))^2
  Result[Result$Estimates == "simex", "R2_M2"] <- cor(ytest,(bsimex_full[1] + Xtest.er %*% bsimex_full[2:3]))^2
  
  Result[Result$Estimates == "ridge", "R2_M1"] <- cor(ytest, (bridge[1] + Xtest.er[, 1] * bridge[2]))^2
  Result[Result$Estimates == "ridge", "R2_M2"] <- cor(ytest, (bridge_full[1] + Xtest.er %*% bridge_full[2:3]))^2
  
  Result[ , "IV_R2"] <- Result[ , "R2_M2"] - Result[ , "R2_M1"]
  
  return(Result)
}


