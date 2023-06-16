filenames <- dir(path = "directory",
                 pattern = ".Rdata", full.names = T)

for(i in 1:length(filenames)) load(filenames[i])

RESULTS <- do.call(rbind, lapply(ls(pattern = "RESULTS_rep"), get)) 
rm(list = ls(pattern = "rep"))
setwd("directory")
rho1   <- seq(.5, 1, by = .1)
rho2   <- rho1
N      <- c(50, 100, 200, 500, 1000)
ratio  <- c("1:0", "2:1" , "1:1" , "1:2")
r12    <- seq(.1, .9, by = .2)
R2     <- seq(.1, .4, by = .1)

design <- expand.grid("rho1" = rho1, "rho2" = rho2, "ratio" = ratio, "N" = N, "r12" = r12, "R2" = R2)

design$ndesign <- 1:nrow(design)
RESULTS <- merge(RESULTS, design, by="ndesign")
save(RESULTS, file = "RESULTS.Rdata")





