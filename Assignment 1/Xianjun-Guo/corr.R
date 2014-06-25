corr <- function(directory, threshold = 0) {
  cr <- vector(mode="numeric",length=0)
  result <- complete(directory)
  for(i in 1:length(result[["id"]])){
    if(result[["nobs"]][i]>threshold){
      fileID <- result[["id"]][i]
      path <- paste(c(directory,"\\",formatC(fileID,width=3,flag=0),".csv"),collapse="")
      data <- read.csv(path)
      good <- complete.cases(data)
      temp <- cor(as.numeric(data[good,][[2]]),as.numeric(data[good,][[3]]))
      cr <- c(cr,temp)   
      
    }
  }
  cr
}