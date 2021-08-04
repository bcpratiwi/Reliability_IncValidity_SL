# obtain estimates from SIMEX algorithm ------

simex_method <- function(SimData){
  
  # extract relevant data for analysis 
  y <- SimData$y
  Xer <- SimData$Xer

  er1 <- SimData$er1     # error variance test 1
  er2 <- SimData$er2     # error varaince test 2
  
  var_names <- c("x1", "x2")
  
  
  if(er1 > 0){
    m1 <- lm(y ~ x1 , x = TRUE, data = cbind.data.frame(y, Xer))
    m1 <- simex(m1, SIMEXvariable = var_names[1] , measurement.error = sqrt(er1))
     } else {
    m1 <- lm(y ~ x1 , x = TRUE, data = cbind.data.frame(y, Xer))
  }
  
  
  
  if((er1 == 0)&&(er2 == 0)){  # no correction if rho1 = rho2 = 1
    
      m2 <- lm(y ~ x1 + x2, x = TRUE, data = cbind.data.frame(y, Xer))
  
    }else if((er1 == 0)&&(er2 > 0)){
      
      m2 <- lm(y ~ x1 + x2, x = TRUE, data = cbind.data.frame(y, Xer))
      m2 <- simex(m2, SIMEXvariable = var_names[2] , measurement.error = sqrt(er2))
    
    } else if((er1 > 0)&&(er2 > 0)){
      
      m2 <- lm(y ~ x1 + x2, x = TRUE, data = cbind.data.frame(y, Xer))
      m2 <- simex(m2, SIMEXvariable = var_names , measurement.error = cbind(sqrt(er1), sqrt(er2)))
      
    } else {
      
      m2 <- lm(y ~ x1 + x2, x = TRUE, data = cbind.data.frame(y, Xer))
      m2 <- simex(m2, SIMEXvariable = var_names[1] , measurement.error = sqrt(er1))
      
  }
  
  
  bsimex = coef(m1)
  bsimex_full = coef(m2)
  
  return(list(bsimex = bsimex, bsimex_full = bsimex_full, 
              m1varJackLambda = m1$variance.jackknife.lambda,
              m2varJackLambda = m2$variance.jackknife.lambda))
  
}