# generate regression coefficients for the 4 scenarios
p1GenBs <- function(ratio, R2, r12){
  
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
  
  return(betas)
  
}