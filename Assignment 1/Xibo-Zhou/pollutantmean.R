pollutantmean <- function(directory, pollutant, id = 1:332) {

	target <- vector(length=0)

	for(i in id) {

		if(i>0 & i<10) {
			index <- paste("00",i,sep="")
		}
		else if(i>=10 & i<100) {
			index <- paste("0",i,sep="")
		}
		else{
			index <- i
		}

		name <- paste(directory,"/",index,".csv",sep="")
		data <- read.csv(name)[pollutant]
		target <- c(target,data[!is.na(data)])
	}

	round(mean(target), digits=3)
#	mean(target)
}
