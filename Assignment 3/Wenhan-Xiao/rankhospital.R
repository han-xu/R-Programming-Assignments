rankhospital <- function(state, outcome, num = "best") {
  firstCharToUpper <- function(x) {
    nc <- length(x)
    for (i in 1:nc){
      first <- first <- substr(x[i],1,1)
      first <- toupper(first)
      x[i] <- paste(c(first, substr(x[i], 2, nchar(x[i]))), collapse="")
    }
    x
  }
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if (length(data[data$State==state,]$State) == 0) {
    stop("invalid state")
  }
  colnames <- colnames(data)
  
  colname = "Hospital.30.Day.Death..Mortality..Rates.from."
  outcome <- unlist(strsplit(outcome," "))
  outcome <- firstCharToUpper(outcome)
  outcome <- paste(outcome, collapse=".")
  colname <- paste(c(colname, outcome),collapse="")
  #print (colname)
  if (length(colnames[colnames == colname])==0) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  mydata <- data[data$State==state,c("Hospital.Name","State", colname)]
  mydata[, colname] <- as.numeric(mydata[, colname])
  mydata <- mydata[order(mydata[,3], mydata[,1]),]
  mydata <- cbind(mydata,rank = 1:dim(mydata)[1])
  cntNAs <- length(mydata[!is.na(mydata[,colname]),]$"State")
  if (num == "best"){
    num = 1
  }
  else if (num == "worst"){
    num = cntNAs
  }
  
  ans <- mydata[mydata$rank==num,]$"Hospital.Name"
  if (length(ans)==0){
    ans = NA
  }
  ans
}