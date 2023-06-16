# least squares estimates -----------------

ls_method <- function(SimData){
  
  # extract relevant data for analysis 
  y <- SimData$y
  Xer <- SimData$Xe
  
  m1 <- lm(y ~ Xer[, 1])
  m2 <- lm(y ~ Xer)
  
  bls <- coef(m1)
  bls_full <- coef(m2)
  
  return(list(bls = bls , bls_full = bls_full))
  
}


