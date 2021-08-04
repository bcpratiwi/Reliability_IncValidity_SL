# obtain shrinkage estimates chosen method = ridge regression ----



ridge_method <- function(SimData){
  
  # extract relevant data for analysis  
  y <- SimData$y
  Xer <- SimData$Xer

  
  m1_ridge <- ridge.cv(Xer[ ,1], y)
  m1_ridge <- lm.ridge.univariate(Xer[ ,1], y, lambda = m1_ridge$lambda.opt)
  
  m2_ridge <- ridge.cv(Xer, y)
  m2_ridge <- lm.ridge(y ~ Xer, lambda = m2_ridge$lambda.opt)
  
  
  bridge = m1_ridge
  
  bridge_full = coef(m2_ridge)
  
  return(list(bridge = bridge, bridge_full = bridge_full))
  
  
  
}