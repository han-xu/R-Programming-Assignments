rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ## Check that state and outcome are valid
  if(outcome == "heart attack") column <- 11
  else if(outcome == "heart failure") column <- 17
  else if(outcome == "pneumonia") column <-23
  else stop("invalid outcome")
  
  specific <- data[,c(2, column, 7)]
  
  ## For each state, find the hospital of the given rank
  splitData <- split(specific,specific[[3]])
  finalResult <- data.frame(hospital=character(),state=character())
  
  dealing <- function(data, num){
    state <- data[1,3]
    data <- data.frame(data[1],as.numeric(data[[2]]))
    data <- data[!is.na(data[[2]]),]
    data <- data[order(data[[2]],data[[1]]),]
    
    len <- length(data[[2]])
    if(num == "best") result <- data[1,1]
    else if(num == "worst") result <- data[len,1]
    else if(len >= num) result <- data[num,1]
    else result <- NA
    data.frame(hospital=result, state=state)
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  re <- lapply(splitData, dealing, num = num)
  finalResult <- do.call("rbind", re)
  finalResult
}
