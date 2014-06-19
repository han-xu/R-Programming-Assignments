corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
#         cr <- vector(mode="numeric", length=0)
        cr <- numeric(0)
        count <- complete(directory, 1:332)
        for (i in count$"id"[count$"nobs">threshold]) {
                csv_name <- paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = "")
                table <- read.csv(csv_name)
                cpl <- na.omit(table)
                cr <- c(cr, cor(cpl$sulfate, cpl$nitrate))
        }
        cr
}