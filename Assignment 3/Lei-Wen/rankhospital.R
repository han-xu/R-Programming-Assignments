rankhospital <- function(state, outcome, num = "best"){
    #     hos_data <- read.csv(
    #         '~/Desktop/rprog-data-ProgAssignment3-data/hospital-data.csv'
    #         ,colClass='character')
    outcome_files <- read.csv(
        '~/Desktop/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv'
        ,colClass='character')
    
    category <- list('heart attack'=11, 'heart failure'=17, 
                     'pneumonia'=23)
    
    state_id <- unique(outcome_files[,'State'])    
    if(!(state %in% state_id))
        stop('invalid state')
    
    outcome_col <- category[outcome]
    if(is.null(outcome_col)){
        stop('invalid outcome')
    }
    outcome_col <- as.numeric(outcome_col)
    
    data_val <- split(outcome_files,outcome_files$State)
    spec_val <- data_val[state]
    spec_val <- as.data.frame(spec_val)
    
    check_val <- cbind(spec_val[,2], spec_val[,outcome_col])
    check_val[,2] <- as.numeric(check_val[,2])
    check_val <- check_val[complete.cases(check_val[,2]),]
    
    best_option <- check_val[order(as.numeric(check_val[,2]),check_val[,1]),1]
    
    if (num == 'best')
        return (best_option[1])
    else if(num == 'worst')
        return (tail(best_option, n=1))
    
    return (best_option[as.numeric(num)])
}
