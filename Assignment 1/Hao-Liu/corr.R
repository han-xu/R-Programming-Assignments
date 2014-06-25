corr <- function(directory, threshold = 0) {
    files <- list.files(path=directory, pattern="*.csv")
    complen <- complete(directory)
    res <- numeric()
    idx <- 1
    for(i in files){
        if(complen[idx, 2] > threshold){
            data <- read.csv(paste(directory,"/",i, sep=""), head = T)[,c("sulfate", "nitrate")]
            data <- na.omit(data)
            res <- union(res, cor(data[, c("sulfate")], data[, c("nitrate")]))
        }
        idx <- idx + 1
    }
    res
}  
