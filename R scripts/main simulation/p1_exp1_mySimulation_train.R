args <- commandArgs(TRUE)
args <- as.numeric(args)


# load library packages and source additional functions ----
library(mefa, lib.loc="directory")
library(MASS, lib.loc="directory")

source("findSigma.R")
source("p1GenData.R")
source("ls_method_train.R")

# simulation design ----------
rho1   <- seq(.5, 1, by = .1)
rho2   <- rho1
N      <- c(50, 100, 200, 500, 1000)
ratio  <- c("1:0", "2:1" , "1:1" , "1:2")
r12    <- seq(.1, .9, by = .2)
R2     <- seq(.1, .4, by = .1)

r   <- args

design <- expand.grid("rho1" = rho1, "rho2" = rho2, "ratio" = ratio, "N" = N, "r12" = r12, "R2" = R2)
design$ndesign <- 1:nrow(design)

RESULTS_train <- expand.grid(rep=r, ndesign=design$ndesign, 
                             Estimates="least_squares", 
                             inter_M1=0, b1_M1=0, inter_M2=0, b1_M2=0, b2_M2=0,
                             MSEtrain_M1=0, MSEtrain_M2=0, IVMSEtrain=0,
                             R2train_M1=0, R2train_M2=0, IVR2train=0)

for(i in 1:nrow(design)){
  
  set.seed((r + 1000)*design$ndesign[i])
  
  cat("FULLdesign = ", design$ndesign[i], "replication = " , r, "\n")
  
  # generate data --
  SimData <- do.call(p1GenData, design[i, -7]  )
  
  # Analyze data set with least squares -----
  ls_est <- ls_method_train(SimData)     # R2 train is also in here
  
  RESULTS_train[RESULTS_train$rep == r & RESULTS_train$ndesign == design$ndesign[i], 
                c("inter_M1", "b1_M1", "inter_M2", "b1_M2", "b2_M2")] <- unlist(ls_est[c(1,2)])
  RESULTS_train[RESULTS_train$rep == r & RESULTS_train$ndesign == design$ndesign[i],
                c("MSEtrain_M1", "MSEtrain_M2", "IVMSEtrain",
                  "R2train_M1", "R2train_M2", "IVR2train")] <- unlist(ls_est[-c(1,2)])
  
  
} # design


RESULTS_train <- merge(RESULTS_train, design, by="ndesign")
assign(paste0("RESULTS_train_rep", r), RESULTS_train)
rm(RESULTS_train)
setwd("directory")
save(list = ls(pattern = "RESULTS_train"), file = paste0("RESULTS_train_rep", r, ".Rdata"))

