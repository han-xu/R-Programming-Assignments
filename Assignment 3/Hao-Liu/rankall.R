rankall <- function(outcome, num = "best"){
  myData <- read.csv(file = "/home/liuhao/R/workspace/outcome-of-care-measures.csv",
                     colClasses = 'character')
  
  if(outcome == 'heart attack'){
    i <- 11
  } else if (outcome == 'heart failure'){
    i <- 17
  } else if (outcome == 'pneumonia'){
    i <- 23
  } else{
    stop('invalid outcome')
  }
  
  unique.states <- sort(unique(myData$State))
  
  result.df <- list()
  
  for(state in unique.states){
    myData.state <- myData[myData$State == state, ]
    myData.state[, i] <- as.numeric(x = myData.state[, i])
    myData.state <- myData.state[complete.cases(myData.state), ]
    
    if(num == "best"){
      numrank = 1
    }else if (num == "worst"){
      numrank = nrow(myData.state)
    }else if (is.numeric(x = num)){
      if(num < 1 || num > nrow(myData.state)){
        result.df <- rbind(result.df, list(NA, state))
        next
      }else 
        numrank <- num
    }
    else {stop('invalid num')}
    
    myData.state <= myData.state[order(myData.state[, i], myData.state$Hospital.Name), ]
    
    return.names <- myData.state[numrank, ]$Hospital.Name
    
    result.df <- rbind(result.df, list(return.names[1], state))
  }
  
  result.df <- as.data.frame(x = result.df)
  colnames(x = result.df) <- c('hospital', 'state')
  result.df
}