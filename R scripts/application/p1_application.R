# This script is for the empirical application using data from Niessent et al. (2016)
# Oridnary least squares and ridge method were used to estimate the weights for the prediction rules

install.packages(c("caret", "MASS", "parcor", "foreign"))

library(foreign)
library(parcor)
library(caret)
library(MASS)

proxPred_dutch_exc <- read.spss("mydata.sav", to.data.frame = T)  # load data from Niessen et al (2016)
X <- c("HSGPA", "Psychology")

proxPred_dutch_exc <- cbind.data.frame(FYGPA = proxPred_dutch_exc[ , "FYGPA"], proxPred_dutch_exc[ ,X])
proxPred_dutch_exc <- proxPred_dutch_exc[complete.cases(proxPred_dutch_exc), ]


obs <- 791

proxPred_dutch_exc <- proxPred_dutch_exc[!(rownames(proxPred_dutch_exc) %in% obs), ]

N <- nrow(proxPred_dutch_exc)
reps <- 100


var_names <- c("HSGPA", "Psychology")


MSE_dutch_exc_folds_HSG <- expand.grid(folds = 10, reps = 1:reps, Method = c("least squares", "ridge"),  MSE1 = 0, MSE2 = 0)
X <- as.matrix(proxPred_dutch_exc[, var_names])
y <- as.vector(proxPred_dutch_exc[, "FYGPA"])

ypred1 <- 0
ypred2 <- 0


for(r in 1:reps)
{
  set.seed(r)
  print(r)
  fold <- createFolds(y, k = 10)
  
  
  # ordinary least squares method -------------
  for (k in 1:10){ 
    
    model_ls_1 <- lm(FYGPA ~ HSGPA, data = proxPred_dutch_exc[- fold[[k]], ])
    model_ls_2 <- lm(FYGPA ~ HSGPA + Psychology , data = proxPred_dutch_exc[- fold[[k]], ])
    
    ypred1[fold[[k]]] <- predict(model_ls_1, newdata = proxPred_dutch_exc[fold[[k]], ])
    ypred2[fold[[k]]] <- predict(model_ls_2, newdata = proxPred_dutch_exc[fold[[k]], ])
    
  }  
  
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "least squares" , 4] <- mean((proxPred_dutch_exc[, "FYGPA"] - ypred1)^2)
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "least squares" , 5] <- mean((proxPred_dutch_exc[, "FYGPA"] - ypred2)^2)
  
  
  
  # shrinkage method -----------------
  
  for (k in 1:10){ 
    
    model_ridge_1 <- ridge.cv(X[- fold[[k]], 1], y[- fold[[k]]])
    model_ridge_1 <- lm.ridge.univariate(X[- fold[[k]], 1], y[- fold[[k]]], lambda = model_ridge_1$lambda.opt)
    
    model_ridge_2 <- ridge.cv(X[-fold[[k]], ], y[- fold[[k]]])
    model_ridge_2 <- lm.ridge( y[-fold[[k]]] ~ X[- fold[[k]], ], lambda = model_ridge_2$lambda.opt)
    
    
    ypred1[fold[[k]]] <- cbind(1, X[fold[[k]], 1]) %*% t(model_ridge_1)
    ypred2[fold[[k]]] <- cbind(1, X[fold[[k]], ]) %*% coef(model_ridge_2)
    
  }
  
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "ridge" , 4] <- mean((y - ypred1)^2)
  MSE_dutch_exc_folds_HSG[MSE_dutch_exc_folds_HSG$reps == r & MSE_dutch_exc_folds_HSG$Method == "ridge" , 5] <- mean((y - ypred2)^2)
  
}


MSE_dutch_exc_folds_HSG$IV <- MSE_dutch_exc_folds_HSG[ ,4] - MSE_dutch_exc_folds_HSG[ ,5]

save(MSE_dutch_exc_folds_HSG, file = "MSE_dutch_exc_folds_HSG.Rdata")

MSE_dutch_exc_folds_HSG$rmseIV <- sqrt(MSE_dutch_exc_folds_HSG[ ,4]) - sqrt(MSE_dutch_exc_folds_HSG[ ,5])
aggregate(rmseIV  ~ Method, mean, data = MSE_dutch_exc_folds_HSG)

# MSE classical approach ----------------
model1 <- lm(FYGPA ~ HSGPA, data = proxPred_dutch_exc)
model2 <- lm(FYGPA ~ HSGPA + Psychology, data = proxPred_dutch_exc)

MSE1 <- mean((proxPred_dutch_exc$FYGPA - fitted(model1))^2)
MSE2 <- mean((proxPred_dutch_exc$FYGPA - fitted(model2))^2)
MSE1 - MSE2

summary(model2)$r.squared - summary(model1)$r.squared 
