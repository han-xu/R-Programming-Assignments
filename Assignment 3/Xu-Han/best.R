best <- function(state, outcome) {
        ## Read outcome data
        table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
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
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        ##filted <- cbind(table[ ,col.no], table[["Hospital.Name"]], table[["State"]])[table$State == state]
        table[, col.no] <- as.numeric(table[, col.no])
        filted <- table[table$State == state, ]
        o <- order(filted[ ,col.no], filted$Hospital.Name)
        filted[o[1], ]$Hospital.Name
        ## filted[ ,col.no]
}
