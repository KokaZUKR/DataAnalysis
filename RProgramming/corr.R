corr <- function(directory, threshold = 0) {
  correlation_vector <- vector()
  for (i in 1:332){
    
    if (i>=1 & i<10){
      filename <- paste(directory, "/00", paste(i, "csv", sep = "."), sep = "")
      df <- read.csv(filename)
      number_of_observed <- nrow(df[!is.na(df$sulfate) & !is.na(df$nitrate),])
      if(number_of_observed > threshold){
        df <- (df[!is.na(df$sulfate) & !is.na(df$nitrate),])
        cor_val <- cor(df$sulfate,df$nitrate)
        correlation_vector <- append(correlation_vector, cor_val)
      } 
        
      
    }else if (i>=10 & i<100){
      filename <- paste(directory, "/0", paste(i, "csv", sep = "."), sep = "")
      df <- read.csv(filename)
      number_of_observed <- nrow(df[!is.na(df$sulfate) & !is.na(df$nitrate),])
      if(number_of_observed > threshold){
        df <- (df[!is.na(df$sulfate) & !is.na(df$nitrate),])
        cor_val <- cor(df$sulfate,df$nitrate)
        correlation_vector <- append(correlation_vector, cor_val)
      }
      
    }else {
      filename <- paste(directory, "/", paste(i, "csv", sep = "."), sep = "")
      df <- read.csv(filename)
      number_of_observed <- nrow(df[!is.na(df$sulfate) & !is.na(df$nitrate),])
      if(number_of_observed > threshold){
        df <- (df[!is.na(df$sulfate) & !is.na(df$nitrate),])
        cor_val <- cor(df$sulfate,df$nitrate)
        correlation_vector <- append(correlation_vector, cor_val)
      }
      
    }
  }
  if (length(correlation_vector) == 0){
    return(c())
  } else {
    
    return(correlation_vector[!is.na(correlation_vector)])
  }
}