rankall <- function(outcome, num = "best") {
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
  ## For each state, find the hospital of the given rank
  state <- unique(data$State)
  state <- state[order(state)]
  hospital <- vector("character", length(state))
  ans <- data.frame(hospital,state,row.names = state, stringsAsFactors=F)
  for(i in 1:length(state)){
    tmpnum = num
    tmpstate <- state[i]
    mydata <- data[data$State==tmpstate,c("Hospital.Name","State", colname)]
    mydata[, colname] <- as.numeric(mydata[, colname])
    
    #rank the data by rate & name, append the ranking
    mydata <- mydata[order(mydata[,3], mydata[,1]),]
    mydata <- cbind(mydata,rank = 1:dim(mydata)[1])
    
    #count of NAs in this state
    cntNAs <- length(mydata[!is.na(mydata[,colname]),]$"State")
    if (tmpnum == "best"){
      tmpnum = 1
    }
    else if (tmpnum == "worst"){
      tmpnum = cntNAs
    }
    
    hospital <- mydata[mydata$rank==tmpnum,]$"Hospital.Name"
    if (length(hospital)==0){
      hospital = NA
    }
    ans[state==tmpstate,] <- c(hospital, tmpstate) 
  }
  
  ans
}
