# args is a stringvector
args <- commandArgs(TRUE)
args <- as.numeric(args)


# load library packages and source additional functions ----
library(mefa, lib.loc="directory")
library(simex, lib.loc="directory")
library(MASS, lib.loc="directory")
# load parcor stuff
parcor_files <- dir("parcor")
sapply(parcor_files, function(x) source(here::here("parcor", x)))

source("findSigma.R")
source("p1GenData.R")
source("ls_method.R")
source("simex_method.R")
source("ridge_method.R")
source("predError.R")
source("predR2.R")

chunk <- function(x,n) split(x, sort(rank(x)%%n))

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


RESULTS <- expand.grid(rep=r, ndesign=design$ndesign, Estimates=c("ridge", "least_squares", "simex"), 
                       Inter_M1=0, b1_M1=0, Inter_M2=0, b1_M2=0, b2_M2=0,
                       MSE_M1=0, MSE_M2=0, IV_MSE=0,
                       R2_M1=0, R2_M2=0, IV_R2=0)

for(i in 1:nrow(design)){
  
  set.seed((r + 1000)*design$ndesign[i])
  
  cat("design = ", design$ndesign[i], "replication = " , r, "\n")
  
  # generate data --
  SimData <- do.call(p1GenData, design[i, -7]  )
  
  
  # Analyze data set with least squares -----
  ls_est <- ls_method(SimData)     
  
  # Analyze data set with SIMEX   ----------
  simex_est <- simex_method(SimData)
  
  # Analyze data set with ridge ----
  ridge_est <- ridge_method(SimData)
  
  
  # Save estimates from each method   ----
  m1m2Estimates <- c(ridge_est, ls_est, simex_est[c("bsimex","bsimex_full")])
  RESULTS[RESULTS$rep== r & RESULTS$ndesign == i, 
          c("Inter_M1", "b1_M1",
            "Inter_M2", "b1_M2", "b2_M2")] <- cbind(rbind(m1m2Estimates[["bridge"]], m1m2Estimates[["bls"]], m1m2Estimates[["bsimex"]]),
                                                    rbind(m1m2Estimates[["bridge_full"]], m1m2Estimates[["bls_full"]], m1m2Estimates[["bsimex_full"]]))
  
  # calculate prediction err m1 m2 and IV (erM1 - erM2)  ----
  MSE <- predError(m1m2Estimates, SimData)
  R2 <- predR2(m1m2Estimates, SimData)
  RESULTS[RESULTS$rep== r & RESULTS$ndesign == i, c("MSE_M1", "MSE_M2", "IV_MSE")] <- MSE[ , c("MSE1", "MSE2", "IV_MSE")]
  RESULTS[RESULTS$rep== r & RESULTS$ndesign == i, c("R2_M1", "R2_M2", "IV_R2")] <- R2[ , c("R2_M1", "R2_M2", "IV_R2")]
  
} # design


assign(paste0("RESULTS_rep", r), RESULTS)
rm(RESULTS)
setwd("directory")
save(list = ls(pattern = "RESULTS"), file = paste0("RESULTS_rep", r, ".Rdata"))




















