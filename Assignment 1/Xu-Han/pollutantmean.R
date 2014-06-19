pollutantmean <- function(directory, pollutant, id = 1:332) {
        if (pollutant == "sulfate" | pollutant == "nitrate") {
                #options(digits = 4)
                #wd <- getwd()
                data <- numeric(0)
                for (i in id) {
                        csv_name <- paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = "")
                        table <- read.csv(csv_name)
                        data <- c(data, table[[pollutant]])
                }
                pmean <- mean(data, na.rm = TRUE)
                round(pmean, 3)
        }
}