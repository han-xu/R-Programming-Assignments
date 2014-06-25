corr <- function(directory, threshold = 0) {

	result <- vector("numeric",length=0)
	nobs <- vector(length=0)
	rest <- complete(directory)
	id <- rest[rest[2]>threshold,]["id"]

	if (dim(id)[1] > 0) {
 	       for(i in 1:dim(id)[1]) {

        	        if(id[i,]>0 & id[i,]<10) {
                	        index <- paste("00",id[i,],sep="")
               		}
        	        else if(id[i,]>=10 & id[i,]<100) {
                	        index <- paste("0",id[i,],sep="")
               		 }
                	else{
				index <- id[i,]
                	}

                	name <- paste(directory,"/",index,".csv",sep="")
                	data <- read.csv(name)
			data <- data[(!is.na(data["sulfate"]) & !is.na(data["nitrate"])),]
                	result <- c(result, cor(data["sulfate"], data["nitrate"]))
        	}
	}

        result
}

