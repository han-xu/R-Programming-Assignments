rankall <- function(outcome, num = "best") {
    data <- read.csv("A3data/outcome-of-care-measures.csv")
    
#     if (!(state %in% data$State)) {
#         stop("invalid state")
#     }
    
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    } else if (outcome == "heart attack") {
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else {
        outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    
    hos_state <- data.frame()

    for (state in as.vector(sort(unique(data$State)))) {
        
        data_subset <- subset(data[c("Hospital.Name", outcome)], data$State == state & data[, outcome] != "Not Available")
        data_ordered <- data_subset[with(data_subset, order(as.numeric(as.character(data_subset[, outcome])), data_subset$Hospital.Name)), ]
            
        if (num == "best") {
            hos_state_new <- data.frame(data_ordered[1, 1], state)
#             hos_state <- rbind(hos_state, data.frame(data_ordered[1, 1], state))
#             return(as.vector(data_ordered[1, 1]))
        }
        else if (num == "worst") {
            hos_state_new <- data.frame(data_ordered[nrow(data_ordered), 1], state)
#             hos_state <- rbind(hos_state, data.frame(data_ordered[nrow(data_ordered), 1], state))
#             return(as.vector(data_ordered[nrow(data_ordered), 1]))
        }
        else if (num > nrow(data_ordered)) {
#             data.frame <- unname(data.frame)
            hos_state_new <- data.frame(data.frame(NA, state))
            colnames(hos_state_new) <- c("hospital", "state")
#             hos_state <- rbind(hos_state, hos_state_new)
#             return(NA)
        }
        else {
            hos_state_new <- data.frame(data_ordered[num, 1], state)
#             hos_state <- rbind(hos_state, data.frame(data_ordered[num, 1], state))
#             return(as.vector(data_ordered[num, 1]))
        }

#         if (first) {
#             colnames(hos_state) <- c("hospital", "state")
#             first <- FALSE
#         }
        colnames(hos_state_new) <- c("hospital", "state")
        hos_state <- rbind(hos_state, hos_state_new)
        

    }

    
#     hos_state_ordered <- hos_state[with(hos_state, order(hos_state$state)), ]
    return(hos_state)
    
}