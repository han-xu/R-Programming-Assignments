pollutantmean <- function(directory, pollutant, id = 1:332){
    files <- list.files(path=directory, pattern="*.csv")[id]
    sum <- 0
    count <- 0
    for(i in files){
        data <- read.csv(paste(directory,"/",i, sep=""), head = T)[,c(pollutant)]
        sum <- sum + sum(data, na.rm = T)
        count <- count + length(na.omit(data)) 
    }
    sum/count
}
