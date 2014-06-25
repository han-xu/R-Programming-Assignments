rankhospital <- function(state, outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  if(outcome == "heart attack") column <- 11
  else if(outcome == "heart failure") column <- 17
  else if(outcome == "pneumonia") column <-23
  else stop("invalid outcome")
  
  specific <- data[data["State"]==state,c(2, column)]
  if(length(specific[[1]])==0) stop("invalid state")
  
  specific <- data.frame(specific[1],as.numeric(specific[[2]]))
  sorted <- specific[order(specific[[2]],specific[[1]]),]
  sorted <- sorted[!is.na(sorted[[2]]),]  
  
  len <- length(sorted[[2]])
  if(num == "best") result <- sorted[1,1]
  else if(num == "worst") result <- sorted[len,1]
  else if(len >= num) result <- sorted[num,1]
  else result <- NA

  result
}
