best <- function(state, outcome, num = "best"){
  outcomes <- read.csv("/home/liuhao/R/workspace/outcome-of-care-measures.csv", 
                       colClasses = "character")
  
  if(!any(state == outcomes$State)){
    stop('invalid state')
  }
  
  if(outcome == 'heart attack'){
    i <- 11
  } else if (outcome == 'heart failure'){
    i <- 17
  } else if (outcome == 'pneumonia'){
    i <- 23
  } else{
    stop('invalid outcome')
  }
  
  outcomes.state <- outcomes[outcomes$State == state, ]
  outcomes.state[, i] <- as.numeric(x = outcomes.state[, i])
  
  outcomes.state <- outcomes.state[complete.cases(outcomes.state), ]
  
  outcomes.state <- outcomes.state[order(outcomes.state[, i], 
                                         outcomes.state$Hospital.Name), ]
  return.names <- outcomes.state[1, ]$Hospital.Name
  
  return.names[1]
}