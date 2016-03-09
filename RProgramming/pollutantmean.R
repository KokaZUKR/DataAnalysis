setwd('~/R')
pollutantmean <- function(directory, pollutant, id = 1:332) {
  x <- vector()
  for (i in id){
    if (i>=1 & i<10){
      filename <- paste(directory, "/00", paste(i, "csv", sep = "."), sep = "")
      df <- read.csv(filename)
      if (pollutant == "nitrate"){
        x <- append(x, df$nitrate)
      } else x <- append(x, df$sulfate)
      
    }else if (i>=10 & i<100){
      filename <- paste(directory, "/0", paste(i, "csv", sep = "."), sep = "")
      df <- read.csv(filename)
      if (pollutant == "nitrate"){
        x <- append(x, df$nitrate)
      } else x <- append(x, df$sulfate)
      
    }else {
      filename <- paste(directory, "/", paste(i, "csv", sep = "."), sep = "")
      df <- read.csv(filename)
      if (pollutant == "nitrate"){
        x <- append(x, df$nitrate)
      } else x <- append(x, df$sulfate)
      
    }
  }
  mean(x, na.rm = T)
}