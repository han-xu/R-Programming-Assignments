rankall <- function(outcome, num = "best") {
        ## Read outcome data
        table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        nb <- NULL
        
        ## Check that state and outcome are valid
        if (outcome == "heart attack") {
                col.no <- 11
        } else if (outcome == "heart failure") {
                col.no <- 17
        } else if (outcome == "pneumonia") {
                col.no <- 23
        } else {
                stop("invalid outcome")
        }
        
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
        t <- split(table, table$State)
        ## For each state, find the hospital of the given rank
        extr <- function(x) {
                y <- x[order(x[ ,col.no], x$Hospital.Name, decreasing=desc),]
                data.frame(hospital = y[nb, ]$Hospital.Name, state = y[nb, ]$State)
        }
        list <- lapply(t, extr)
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        result <- do.call("rbind", list)
        result
}
