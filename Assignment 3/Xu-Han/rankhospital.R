rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        nb <- NULL
        
        ## Check that state and outcome are valid
        if (!state %in% table$State) {
                stop("invalid state")
        }
        if (outcome == "heart attack") {
                col.no <- 11
        } else if (outcome == "heart failure") {
                col.no <- 17
        } else if (outcome == "pneumonia") {
                col.no <- 23
        } else {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if (num == "worst") {
                desc <- TRUE
                nb <- 1
        } else if (num == "best") {
                desc <- FALSE
                nb <- 1
        } else {
                desc <- FALSE
                nb <- as.numeric(num)
        }
        table[, col.no] <- as.numeric(table[, col.no])
        filted <- table[table$State == state, ]
        if (nb>nrow(filted)) {
                return(NA)
        }
        o <- order(filted[ ,col.no], filted$Hospital.Name, decreasing=desc)
        filted[o[nb], ]$Hospital.Name
}