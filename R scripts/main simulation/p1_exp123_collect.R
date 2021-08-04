


filenames <- paste0("ResultJob", 1:400, ".Rdata")


RESULT <- data.frame()

for(i in 1:length(filenames)){
  
  cat("\rJob = ", i)
  
  RESULT  <- rbind(RESULT, get(load(filenames[i])))
  
  
}


save(RESULT, file = "AllResults.Rdata")
rm(RESULT)




