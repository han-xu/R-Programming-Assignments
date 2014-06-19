rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("A3data/outcome-of-care-measures.csv")
    
    if (!(state %in% data$State)) {
        stop("invalid state")
    }
    
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    } else if (outcome == "heart attack") {
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else {
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    
    data_subset <- subset(data[c("Hospital.Name", outcome)], data$State == state & data[, outcome] != "Not Available")
    data_ordered <- data_subset[with(data_subset, order(as.numeric(as.character(data_subset[, outcome])), data_subset$Hospital.Name)), ]
    

    if (num == "best") {
        return(as.vector(data_ordered[1, 1]))
    }
    else if (num == "worst") {
        return(as.vector(data_ordered[nrow(data_ordered), 1]))
    }
    else if (num > nrow(data_ordered)) {
        return(NA)
    }
    else {
        return(as.vector(data_ordered[num, 1]))
    }
    
    
}