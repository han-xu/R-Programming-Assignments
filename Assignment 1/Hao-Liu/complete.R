complete <- function(directory, id = 1:332) {
    files <- list.files(path=directory, pattern="*.csv")[id]
    x <- numeric(length(id))
    y <- numeric(length(id))
    for(i in id){
        data <- read.csv(paste(directory,"/",files[i], sep=""), head = T)
        x[i] <- i
        y[i] <- nrow(na.omit(data))
    }
   res <- data.frame(x, y)
   names(res) <- c("id", "nobs")
   res
}

