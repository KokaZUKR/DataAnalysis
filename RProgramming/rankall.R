rankall <- function(outcome, num = "best") {
  outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  outcome_data <- na.omit(outcome_data)
  valid_states <- unique(outcome_data$State)
  outcome_vector <- rep(c(outcome),length(valid_states))
  num_vector <- rep(c(num), length(valid_states))
  if (num == "best"){
    source('best.R')
    states <- mapply(best, valid_states, outcome_vector)
  }
  else if (num == "worst"){
    source('worst.R')
    states <- mapply(worst, valid_states, outcome_vector)
  }
  else {
    source('rankhospital.R')
    states = mapply(rankhospital, valid_states, outcome_vector, num_vector)
  }
  df <- as.matrix(unlist(states))
  colnames(df) <- paste("state")
  return(df)
}