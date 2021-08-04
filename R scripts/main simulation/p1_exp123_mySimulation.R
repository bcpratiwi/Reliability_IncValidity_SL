# args is a stringvector
args <- commandArgs(TRUE)
args <- as.numeric(args)



# ndesign : 14400
# run : counter for job in cluster. nJobs : 400
# each job runs 36 design
run <- args


# load library packages and source additional functions ----

library(parcor)
library(mefa)
library(simex)
library(MASS)


source("findSigma.R")
source("p1GenData.R")
source("ls_method.R")
source("simex_method.R")
source("ridge_method.R")
source("predError.R")

# simulation design ----------

rho1   <- seq(.5, 1, by = .1)
rho2   <- rho1
N      <- c(50, 100, 200, 500, 1000)
ratio  <- c("1:0", "2:1" , "1:1" , "1:2")
r12    <- seq(.1, .9, by = .2)
R2     <- seq(.1, .4, by = .1)

Rep    <- 100

design <- expand.grid("rho1" = rho1, "rho2" = rho2, "ratio" = ratio, "N" = N, "r12" = r12, "R2" = R2)

design$ndesign <- 1:nrow(design)

design  <- design[((run - 1)*36 +1):(run * 36), ]


# create objects RESULT -----------
outcomeResult          <- c("Estimates", "MSE1", "MSE2", "IV_MSE", "R2train",
                            "M1_Inter", "M1_B1", "M2_Inter", "M2_B1", "M2_B2", "study")
indexDesignResult      <- expand.grid("Estimates" = c("ridge", "least_squares", "simex"), 
                                 "replication" = 1:Rep, "dnum" = design$ndesign)
checkResult            <- paste0("Row", indexDesignResult$dnum, "Rep", indexDesignResult$replication)
nEst             <- length(unique(indexDesignResult$Estimates))
RESULT           <- cbind.data.frame(indexDesignResult$Estimates, matrix(0, Rep * nrow(design) * nEst, length(outcomeResult) - 2), checkResult)
colnames(RESULT) <- outcomeResult


# create counter  ---- 
cResult  <- 1:(nrow(design) * Rep * nEst)
cResult  <- chunk(cResult, nrow(design)* Rep)


for(i in 1:nrow(design)){
  
  for(r in 1:Rep){
    
   set.seed((r + 1000)*design$ndesign[i])

   cat("design = ", design$ndesign[i], "replication = " , r, "\n")

   # generate data --
   SimData <- do.call(p1GenData, design[i, -7]  )


   # Analyze data set with least squares -----

   ls_est <- ls_method(SimData)     # R2 train is also in here

   # Analyze data set with SIMEX   ----------
   simex_est <- simex_method(SimData)

   # Analyze data set with ridge ----
   ridge_est <- ridge_method(SimData)


   # Save estimates from each method   ----
   m1m2Estimates <- c(ridge_est, ls_est, simex_est[c("bsimex","bsimex_full")])

   # calculate prediction err m1 m2 and IV (erM1 - erM2)  ----
   Result <- predError(m1m2Estimates, SimData)
   
   RESULT[cResult[[(i - 1) * Rep + r]], c("MSE1", "MSE2", "IV_MSE", "R2train")] <- Result[ , c("MSE1", "MSE2", "IV_MSE", "R2train")]
   
   
   # variance of the estimates from simex -----

   m1m2Estimates[["deltaR2"]]  <- NULL   # remove IV delta R2
   
   RESULT[cResult[[(i - 1) * Rep + r]], c("M1_Inter","M1_B1",
                                          "M2_Inter","M2_B1", "M2_B2")] <- cbind(rbind(m1m2Estimates[[1]], m1m2Estimates[[3]], m1m2Estimates[[5]]),
                                                                                 rbind(m1m2Estimates[[2]], m1m2Estimates[[4]], m1m2Estimates[[6]]))

  } # replication
  

} # design


matrixDesignResult <- rep(design, each = nEst * Rep)

RESULT <- cbind.data.frame(matrixDesignResult, RESULT, "Job" = rep(run, nrow(RESULT)))

save(RESULT, file = paste("Result","Job", run ,".Rdata" , sep ="")) 

















