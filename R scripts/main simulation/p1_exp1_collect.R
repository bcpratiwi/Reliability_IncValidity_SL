filenames <- dir(path = "directory",
                 pattern = ".Rdata", full.names = T)

for(i in 1:length(filenames)) load(filenames[i])

RESULTS_train <- do.call(rbind, lapply(ls(pattern = "RESULTS_train_rep"), get)) 
rm(list = ls(pattern = "rep"))
setwd("directory")

save(RESULTS_train, file = "RESULTS_train.Rdata")





