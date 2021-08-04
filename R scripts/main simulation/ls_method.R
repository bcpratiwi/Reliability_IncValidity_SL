# least squares estimates -----------------

ls_method <- function(SimData){
  
  # extract relevant data for analysis 
  y <- SimData$y
  Xer <- SimData$Xe
  
  m1 <- lm(y ~ Xer[, 1])
  m2 <- lm(y ~ Xer)
  
  bls <- coef(m1)
  bls_full <- coef(m2)
  
  deltaR2 <- summary(m2)$r.squared - summary(m1)$r.squared
  
  return(list(bls = bls , bls_full = bls_full, deltaR2 = deltaR2))
  
}


