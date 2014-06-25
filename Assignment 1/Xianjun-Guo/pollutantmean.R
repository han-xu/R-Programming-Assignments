pollutantmean <- function(directory, pollutant, id = 1:332){
  sum <- 0
  num <- 0
  for(i in id){
    if(i<10)
      path <- c(directory,"\\","00",as.character(i),".csv")
    else if(i<100)
      path <- c(directory,"\\","0",as.character(i),".csv")
    else 
      path <- c(directory,"\\",as.character(i),".csv")
    
    path <- paste(path,collapse='')
    
    data <- read.csv(path)
    sum <- sum + sum(data[[pollutant]],na.rm = TRUE)
    num <- num + sum(!is.na(data[[pollutant]]))
  }
  options(digits=4)
  mean <- sum/num
  print(mean)
}

