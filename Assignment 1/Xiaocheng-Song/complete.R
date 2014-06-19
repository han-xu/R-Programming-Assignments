complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    file_list <- list.files(directory, full.names=TRUE)
    dat <- data.frame()
    id_nobs <- data.frame()
    
    for (i in id) {
#         dat <- rbind(dat, read.csv(file_list[i]))    
        nobs <- sum(complete.cases(read.csv(file_list[i])))
        id_nobs <- rbind(id_nobs, c(i, nobs))
    }
    colnames(id_nobs)<-c("id", "nobs")
    id_nobs
} 