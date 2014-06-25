best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  if(outcome == "heart attack") column <- 11
  else if(outcome == "heart failure") column <- 17
  else if(outcome == "pneumonia") column <-23
  else stop("invalid outcome")
  
  specific <- data[data["State"]==state,c(2, column)]
  if(length(specific[[1]])==0) stop("invalid state")
  
  minimum <- min(as.numeric(specific[,2]), na.rm=TRUE)

  hospName <- specific[as.numeric(specific[,2]) == minimum, 1]
  sorted <- sort(hospName)

  sorted[1]
}