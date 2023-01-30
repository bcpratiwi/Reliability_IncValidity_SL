# This script is for the empirical application using data from Niessent et al. (2016)
# Oridnary least squares and ridge method were used to estimate the weights for the prediction rules
# This script is for the empirical application using data from Niessent et al. (2016)
# Oridnary least squares and ridge method were used to estimate the weights for the prediction rules

library(foreign)
library(caret)
library(MASS)

# load parcor package
setwd("~/parcor") 
files <- dir()
for(i in 1:length(files)) source(files[i])
files <- dir(); for(i in 1:length(files)) source(files[i])

setwd("~p1_application")
proxPred_dutch_exc <- read.spss("Complete data Psychology sample.sav", to.data.frame = T)  # load data from Niessen et al (2016)
X <- c("MeanHSG", "RawScore_Psychology", "RawScore_English", "RawScore_Math")

proxPred_dutch_exc <- cbind.data.frame(MeanFYG = proxPred_dutch_exc[ , "MeanFYG"], proxPred_dutch_exc[ ,X])
proxPred_dutch_exc <- proxPred_dutch_exc[complete.cases(proxPred_dutch_exc), ]

obs <- 791

proxPred_dutch_exc <- proxPred_dutch_exc[!(rownames(proxPred_dutch_exc) %in% obs), ]
var_names <- c("MeanHSG", "RawScore_Psychology", "RawScore_English", "RawScore_Math")

N <- nrow(proxPred_dutch_exc)
reps <- 100

X <- as.matrix(proxPred_dutch_exc[, var_names])
y <- as.vector(proxPred_dutch_exc[, "MeanFYG"])

M1 <- MeanFYG ~ MeanHSG
M2a <- MeanFYG ~ MeanHSG + RawScore_Psychology
M2b <- MeanFYG ~ MeanHSG + RawScore_English
M2c <- MeanFYG ~ MeanHSG + RawScore_Math

MSE_dutch_exc_folds_HSG <- expand.grid(folds = 10, reps = 1:reps, Method = c("least squares", "ridge"),  MSE1 = 0, MSE2a = 0, MSE2b = 0, MSE2c = 0)

ypred1 <- rep(0, N)
ypred2a <- rep(0, N)
ypred2b <- rep(0, N)
ypred2c <- rep(0, N)
kfold <- 10

for(r in 1:reps){
  set.seed(r)
  print(r)
  fold <- sample(rep(1:kfold, each = ceiling(N/kfold)), N, replace = F)
  
  # Least squares method -------------
  for (k in 1:kfold){ 
    
    model_ls_1 <- lm(M1, data = proxPred_dutch_exc[fold != k, ])
    model_ls_2a <- lm(M2a , data = proxPred_dutch_exc[fold != k,, ])
    model_ls_2b <- lm(M2b , data = proxPred_dutch_exc[fold != k,, ])
    model_ls_2c <- lm(M2c , data = proxPred_dutch_exc[fold != k,, ])
    
    ypred1[fold==k] <- predict(model_ls_1, newdata = proxPred_dutch_exc[fold==k, ])
    ypred2a[fold==k] <- predict(model_ls_2a, newdata = proxPred_dutch_exc[fold==k, ])
    ypred2b[fold==k] <- predict(model_ls_2b, newdata = proxPred_dutch_exc[fold==k, ])
    ypred2c[fold==k] <- predict(model_ls_2c, newdata = proxPred_dutch_exc[fold==k, ])
    
  }  
  
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "least squares" , 4] <- mean((proxPred_dutch_exc[, "MeanFYG"] - ypred1)^2)
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "least squares" , 5] <- mean((proxPred_dutch_exc[, "MeanFYG"] - ypred2a)^2)
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "least squares" , 6] <- mean((proxPred_dutch_exc[, "MeanFYG"] - ypred2b)^2)
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "least squares" , 7] <- mean((proxPred_dutch_exc[, "MeanFYG"] - ypred2c)^2)
  
  # shrinkage method -----------------
  
  for (k in 1:kfold){ 
    
    model_ridge_1 <- ridge.cv(X[fold!=k, 1], y[fold!=k])
    model_ridge_1 <- lm.ridge.univariate(X[fold!=k, 1], y[fold!=k], lambda = model_ridge_1$lambda.opt)
    
    model_ridge_2a <- ridge.cv(X[fold!=k, c(1,2)], y[fold!=k])
    model_ridge_2a <- lm.ridge( y[fold!=k] ~ X[fold!=k, c(1,2)], lambda = model_ridge_2a$lambda.opt)
    
    model_ridge_2b <- ridge.cv(X[fold!=k, c(1,3)], y[fold!=k])
    model_ridge_2b <- lm.ridge( y[fold!=k] ~ X[fold!=k, c(1,3)], lambda = model_ridge_2b$lambda.opt)
    
    model_ridge_2c <- ridge.cv(X[fold!=k, c(1,4)], y[fold!=k])
    model_ridge_2c <- lm.ridge( y[fold!=k] ~ X[fold!=k, c(1,4)], lambda = model_ridge_2c$lambda.opt)
    
    ypred1[fold==k] <- cbind(1, X[fold==k, 1]) %*% t(model_ridge_1)
    ypred2a[fold==k] <- cbind(1, X[fold==k, c(1,2)]) %*% coef(model_ridge_2a)
    ypred2b[fold==k] <- cbind(1, X[fold==k, c(1,3)]) %*% coef(model_ridge_2b)
    ypred2c[fold==k] <- cbind(1, X[fold==k, c(1,4)]) %*% coef(model_ridge_2c)
    
  }
  
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "ridge" , 4] <- mean((y - ypred1)^2)
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "ridge" , 5] <- mean((y - ypred2a)^2)
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "ridge" , 6] <- mean((y - ypred2b)^2)
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "ridge" , 7] <- mean((y - ypred2c)^2)
  
}


MSE_dutch_exc_folds_HSG$IVa <- MSE_dutch_exc_folds_HSG[ ,4] - MSE_dutch_exc_folds_HSG[ ,5]
MSE_dutch_exc_folds_HSG$IVb <- MSE_dutch_exc_folds_HSG[ ,4] - MSE_dutch_exc_folds_HSG[ ,6]
MSE_dutch_exc_folds_HSG$IVc <- MSE_dutch_exc_folds_HSG[ ,4] - MSE_dutch_exc_folds_HSG[ ,7]

summary(MSE_dutch_exc_folds_HSG[ , c(8,9,10)])

library(ggplot2)
library(jtools)
p2a <- ggplot(data = MSE_dutch_exc_folds_HSG, aes(x = Method, y = IVa)) + geom_boxplot() + 
          labs(title = "Incremental Validity for PSYCHOLOGY", y = "Incremental Validity", x = "Estimation Method") +
          ylim(c(-0.01, 0.065)) + theme_bw()
p2b <- ggplot(data = MSE_dutch_exc_folds_HSG, aes(x = Method, y = IVb)) + geom_boxplot() + 
          labs(title = "Incremental Validity for ENGLISH", y = "Incremental Validity", x = "Estimation Method") + 
          ylim(c(-0.01, 0.065)) + theme_bw()
p2c <- ggplot(data = MSE_dutch_exc_folds_HSG, aes(x = Method, y = IVc)) + geom_boxplot() + 
          labs(title = "Incremental Validity for MATH", y = "Incremental Validity", x = "Estimation Method") +  
          ylim(c(-0.01, 0.065)) + theme_bw()

ggsave("IVpsych.pdf", p2a, width = 15, height = 15, units = "cm")
ggsave("IVenglish.pdf", p2b, width = 15, height = 15, units = "cm")
ggsave("IVmath.pdf", p2c, width = 15, height = 15, units = "cm")

quantile(subset(MSE_dutch_exc_folds_HSG, Method = "least squares")[, 8], probs = c(0.1, 0.9))
quantile(subset(MSE_dutch_exc_folds_HSG, Method = "least squares")[, 9], probs = c(0.1, 0.9))
quantile(subset(MSE_dutch_exc_folds_HSG, Method = "least squares")[, 10], probs = c(0.1, 0.9))

quantile(subset(MSE_dutch_exc_folds_HSG, Method = "ridge")[, 8], probs = c(0.1, 0.9))
quantile(subset(MSE_dutch_exc_folds_HSG, Method = "ridge")[, 9], probs = c(0.1, 0.9))
quantile(subset(MSE_dutch_exc_folds_HSG, Method = "ridge")[, 10], probs = c(0.1, 0.9))

save(MSE_dutch_exc_folds_HSG, file = "MSE_dutch_exc_folds_HSG_mdr.Rdata")

MSE_dutch_exc_folds_HSG$rmseIV <- sqrt(MSE_dutch_exc_folds_HSG[ ,4]) - sqrt(MSE_dutch_exc_folds_HSG[ ,5])
aggregate(rmseIV  ~ Method, mean, data = MSE_dutch_exc_folds_HSG)

# Classical approach ----------------
model1 <- lm(M1, data = proxPred_dutch_exc)
model2a <- lm(M2a, data = proxPred_dutch_exc)
anova(model1, model2a)

model2b <- lm(M2b, data = proxPred_dutch_exc)
anova(model1, model2b)

model2c <- lm(M2c, data = proxPred_dutch_exc)
anova(model1, model2c)

R1 <- summary(model1)$r.squared 
R2a <- summary(model2a)$r.squared 
R2b <- summary(model2b)$r.squared 
R2c <- summary(model2c)$r.squared 

MSE1 <- mean((proxPred_dutch_exc$MeanFYG - fitted(model1))^2)
MSE2a <- mean((proxPred_dutch_exc$MeanFYG - fitted(model2a))^2)
MSE2b <- mean((proxPred_dutch_exc$MeanFYG - fitted(model2b))^2)
MSE2c <- mean((proxPred_dutch_exc$MeanFYG - fitted(model2c))^2)

TAB <- matrix(NA, 3, 4)

TAB[1, 1] <- MSE1 - MSE2a
TAB[2, 1] <- MSE1 - MSE2b
TAB[3, 1] <- MSE1 - MSE2c

TAB[1, 2] <- R2a - R1
TAB[2, 2] <- R2b - R1
TAB[3, 2] <- R2c - R1

TAB[1, 3] <- anova(model1, model2a)$F[2]
TAB[2, 3] <- anova(model1, model2b)$F[2]
TAB[3, 3] <- anova(model1, model2c)$F[2]

TAB[1, 4] <- anova(model1, model2a)["Pr(>F)"][2,1]
TAB[2, 4] <- anova(model1, model2b)["Pr(>F)"][2,1]
TAB[3, 4] <- anova(model1, model2c)["Pr(>F)"][2,1]

rownames(TAB) <- c("Psychology", "English", "Math")
colnames(TAB) <- c("MSE", "R2", "F", "p")
library(xtable)
xtable(TAB)
