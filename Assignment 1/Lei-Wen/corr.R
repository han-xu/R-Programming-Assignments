corr <- function(directory,threshold=0){
    file_id <- list.files(directory)
    output <- numeric()
    
    for(id in file_id){
        file_address <- paste(directory,id,sep='/')
        csv_reader <- read.csv(file_address)
        
        good <- complete.cases(csv_reader)
        useful_data <- csv_reader[good,]
        
        if(nrow(useful_data)>threshold){
            value <- cor(useful_data['sulfate'],
                         useful_data['nitrate'])
            output <- append(output,value)
        }
    }
    
    return (output)
}