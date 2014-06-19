pollutantmean <- function(directory, pollutant, id=1:332){
#     csv_read = read.csv(directory, header = TRUE, sep = ",")
    data_file = data.frame()
    file_id = list.files(directory)
    
    for(i in id){
        file_address <- paste(directory,file_id[i],sep="/")
        file <- read.csv(file_address, header = TRUE, sep=",")
        
        data_file = rbind(data_file,file)
    }
    
    mean_ = mean(data_file[,pollutant],na.rm=TRUE)
    return (mean_)
}