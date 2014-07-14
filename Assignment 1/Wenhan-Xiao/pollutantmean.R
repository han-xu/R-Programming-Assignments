pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  #id <- as.numeric(id)
  my_data <- numeric()
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
    my_data <- c(my_data, lines[[pollutant]])
  }

  mean(my_data, na.rm = TRUE)
}