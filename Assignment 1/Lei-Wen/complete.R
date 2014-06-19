complete <- function(directory, id = 1:332){
    complete <- data.frame()
    file_id <- list.files(directory)
    
    for(i in id){ 
        file_address <- paste(directory,file_id[i],sep="/")
        data_ <- read.csv(file_address, header = TRUE, sep=",")
        good <- complete.cases(data_)
        
        num <- nrow(data_[good,])
        complete <- rbind(complete,data.frame(id=i,nobs=num))
    }
    
    return (complete)
}