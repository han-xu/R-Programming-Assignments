corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    id_nobs <- complete(directory, 1:332)
    id_above_thres <- subset(id_nobs$id, id_nobs$nobs > threshold)
    file_list <- list.files(directory, full.names=TRUE)
    
    correlations <- c()
    
    for (i in as.vector(id_above_thres, mode="numeric")) {
        data <- read.csv(file_list[i])
        correlations <- c(correlations, cor(data$nitrate, data$sulfate, use="complete"))
    }
    
    correlations
}