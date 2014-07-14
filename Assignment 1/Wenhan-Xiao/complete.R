complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  nc <- length(id)
  nobs <- vector("numeric", nc)
  cors <- vector("numeric", nc)
  ans <- data.frame(id, nobs, cors)
  for (i in 1:length(id)) {
    #print (i)
    if (id[i]<10) {
      file <- paste(c(directory, "//00", id[i], ".csv"), collapse = "")
    }
    else if (id[i]<100) {
      file <- paste(c(directory, "//0", id[i], ".csv"), collapse = "")
    }
    else {
      file <- paste(c(directory, "//", id[i], ".csv"), collapse = "")
    }
    #print (file)
    lines <- read.csv(file)
    
    #my_data <- c(my_data, lines[[pollutant]])
    good <- complete.cases(lines)
    data <- lines[good,]
    nob <- dim(data)[1]
    cor <- cor(data$nitrate, data$sulfate)
    ans[id==id[i],] <- c(id[i], nob, cor)
    #ans[i,] <- (id[i], nob)
  }
  ans
}