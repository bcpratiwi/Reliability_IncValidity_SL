# This script contains the simulation in Appendix A 
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(jtools)
library(reshape2)


reps <- 100
result <- expand.grid(reps = 1:reps, shrink = seq(0.1, 1.1, by = 0.1), 
                      outMSE = 0, outR2 = 0)

set.seed(1234)

N <- 10000                                     # population size
r <- .2                                        # true effect
rho <- .9                                      # reliability
er <- (1-rho)/rho      
x <- rnorm(N)           
xer <- x + rnorm(N, sd = sqrt(er))
y <- r*x + rnorm(N, sd = sqrt(1-r^2))
n <- 100                                       # training sample
idx <- 1:N                                     # index for all obs in population
sh <- length(unique(result$shrink))
s <- unique(result$shrink)

for(i in 1:reps){
  
  cat('rep =', i , "\r")
  test <- sample(idx, 100)
  train <- sample(idx[-test], n)
  
  m1 <- lm(y[train] ~ xer[train])
 
  # apply shrinkage and correction for b^ols in the validation/test sample
  for(j in 1:sh) { 
    bshrink <- s[j]*coef(m1)[2]                                    # s*b^ols
    inter <- coef(m1)[1]                                           # intercept 
    fit <- inter + xer[test]*bshrink
    result[result$reps == i & result$shrink == s[j], "outMSE"] <- mean((y[test] - fit) ^2)
    result[result$reps == i & result$shrink == s[j], "outR2"] <-  cor(y[test], fit)^2
  } 
}


# rearrange the matrix containing the results -----------
meltR <- melt(result, id.vars = c("shrink", "reps"), measure.vars = c( "outMSE", "outR2")) 

# aggregate the solutions based on the mean -----------
result_ave <- aggregate(cbind(outMSE, outR2) ~ shrink, mean, data = result)

gg1 <- ggplot(result_ave, aes(x = shrink)) + geom_point(aes(y = outMSE)) + 
  geom_line(aes(y = outMSE)) + 
  scale_x_continuous(breaks = unique(result_ave$shrink)) +
  labs(y = "Mean Squared Error", x = expression(s)) +
  theme_apa() 

gg2 <- ggplot(result_ave, aes(x = shrink)) + geom_point(aes(y = outR2)) + 
  geom_line(aes(y = outR2)) +
  scale_x_continuous(breaks = unique(result_ave$shrink)) + 
  labs(y = expression(R^2), x = expression(s)) + theme_apa() 


gg <- grid.arrange(gg1, gg2, ncol = 2)

# save plots -------
ggsave("out_MSE_R2_shrink.pdf", gg, width = 10, height = 6)
ggsave("outMSE_shrink.pdf", gg1, width = 6, height = 6)
ggsave("outR2_shrink.pdf", gg2, width = 6, height = 6)







