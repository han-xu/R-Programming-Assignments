complete <- function(directory, id = 1:332){
  result = data.frame(id=numeric(),nobs=numeric())
  for(i in id){
    path <- paste(c(directory,"\\",formatC(i,width=3,flag=0),".csv"),collapse="")
    data <- read.csv(path)
    good <- complete.cases(data)
    result <- rbind(result, data.frame(id=i,nobs=sum(good)))   
  }
  result
}