complete <- function(directory, ids = 1:332){
  id <- vector()
  nobs <- vector()
  for (i in ids){
    id <- append(id, i)
    if (i>=1 & i<10){
      filename <- paste(directory, "/00", paste(i, "csv", sep = "."), sep = "")
      df <- read.csv(filename)
      nobs_per_id <- length(na.exclude(df)[,3])
      nobs <- append(nobs, nobs_per_id)
      
    }else if (i>=10 & i<100){
      filename <- paste(directory, "/0", paste(i, "csv", sep = "."), sep = "")
      df <- read.csv(filename)
      nobs_per_id <- length(na.exclude(df)[,3])
      nobs <- append(nobs, nobs_per_id)
      
    }else {
      filename <- paste(directory, "/", paste(i, "csv", sep = "."), sep = "")
      df <- read.csv(filename)
      nobs_per_id <- length(na.exclude(df)[,3])
      nobs <- append(nobs, nobs_per_id)
      
    }
  }
  data.frame(id, nobs)
  }


