# load library packages and source additional functions ----

library(mefa)
library(MASS)

chunk <- function(x,n) split(x, sort(rank(x)%%n))

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

Rep    <- 100

FULLdesign <- expand.grid("rho1" = rho1, "rho2" = rho2, "ratio" = ratio, "N" = N, "r12" = r12, "R2" = R2)
FULLdesign$ndesign <- 1:nrow(FULLdesign)
# create objects RESULT -----------
outcomeResult          <- c("Estimates", "MSEtrain_M1", "MSEtrain_M2", "IVtrain", "study")
indexDesignResult      <- expand.grid("Estimates" =  "least_squares", 
                                 "replication" = 1:Rep, "dnum" = FULLdesign$ndesign)
checkResult            <- paste0("Row", indexDesignResult$dnum, "Rep", indexDesignResult$replication)
nEst             <- length(unique(indexDesignResult$Estimates))
RESULT           <- cbind.data.frame(indexDesignResult$Estimates, matrix(0, Rep * nrow(FULLdesign) * nEst, length(outcomeResult) - 2), checkResult)
colnames(RESULT) <- outcomeResult
check <- expand.grid(reps = 1:100, design = 1:36, run = 1:400)
check$design <- rep(FULLdesign$ndesign, each = 100)

RESULT <- cbind(RESULT, check)

# ndesign : 14400
# run : counter for job in cluster. nJobs : 400
# each job runs 36 design


for(run in 1:400){
  
  design  <- FULLdesign[((run - 1)*36 +1):(run * 36), ]
  
  
  for(i in 1:nrow(design)){
    
    for(r in 1:Rep){
      
     set.seed((r + 1000)*design$ndesign[i])
  
     cat("job = ", run, "FULLdesign = ", design$ndesign[i], "replication = " , r, "\n")
     
     # generate data --
     SimData <- do.call(p1GenData, design[i, -7]  )
  
  
     # Analyze data set with least squares -----
  
     ls_est <- ls_method_train(SimData)     # R2 train is also in here
     RESULT[RESULT$reps == r & RESULT$design == design$ndesign[i] & RESULT$run == run , 
            c("MSEtrain_M1", "MSEtrain_M2", "IVtrain")] <- c(ls_est)

    } # replication
  } # design
} # run


matrixDesignResult <- rep(FULLdesign, each = nEst * Rep)
RESULTS_train <- cbind.data.frame(matrixDesignResult, RESULT)

save(RESULTS_train, file = "RESULTS_train.Rdata") 


















