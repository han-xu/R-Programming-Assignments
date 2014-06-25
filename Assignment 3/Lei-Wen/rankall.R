rankall <- function(outcome, num = 'best'){    
    outcome_files <- read.csv(
        '~/Desktop/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv'
        ,colClass='character')
    
    category <- list('heart attack'=11, 'heart failure'=17, 
                     'pneumonia'=23)
    
    outcome_col <- category[outcome]
    if(is.null(outcome_col)){
        stop('invalid outcome')
    }
    outcome_col <- as.numeric(outcome_col)
    
    valid_data <- cbind(outcome_files[,2],outcome_files[,7],
                        outcome_files[,outcome_col])
    valid_data <- valid_data[complete.cases(as.numeric(valid_data[,3])),]
    valid_data <- as.data.frame(valid_data)
    
    if (num == 'best')
        index = 1
    else if(num == 'worst')
        index = -1
    else
        index <- as.numeric(num)
    
    find_hospital <- lapply(split(valid_data,valid_data$V2),
                        function(x) {
                            x <- x[order(as.numeric(as.character(x[,3])),x[,1]),]
                            if(index == -1)
                                tmp <- tail(x,n=1)
                            else
                                tmp <- x[index,]
                            x <- tmp[1,1]
                            })
    
    output <- data.frame()
    state_names <- names(find_hospital)
    for (s in state_names){
        output <- rbind(output,data.frame(hospital=find_hospital[[s]],
                                          state = s))
    }
    
    return (output)   
}