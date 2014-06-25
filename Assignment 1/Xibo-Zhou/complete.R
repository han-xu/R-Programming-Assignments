complete <- function(directory, id = 1:332) {

	nobs <- vector(length=0)

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
                data <- read.csv(name)
                count <- dim(data[(!is.na(data["sulfate"]) & !is.na(data["nitrate"])),])[1]
		nobs <- c(nobs, count)
        }

        data.frame(id, nobs)
}

