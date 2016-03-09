rankhospital <- function(state, outcome, num = "best") {
  outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  outcome_data <- na.omit(outcome_data)
  outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  valid_states <- unique(outcome_data$State)
  valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  valid_choices <- c('worst', 'best')
  if (num == "best"){
    source('best.R')
    best(state, outcome)
    return()
  }
  else if (num == "worst") {
    source('worst.R')
    worst(state, outcome)
    return()
  }
  
  if (is.element(state, valid_states) & is.element(outcome, valid_outcomes)) {
    outcome_data <- outcome_data[outcome_data$State == state,]
    if (num > length(outcome_data$Hospital.Name)){
      #print("Number is larger that the number of hospitals in that state")
      return(NA)
    }
    if (outcome == 'heart attack') {
      ranked_state <- outcome_data[order(outcome_data[11]),2][num]
    }
    else if (outcome == 'heart failure') {
      ranked_state <- outcome_data[order(outcome_data[17]),2][num]
    }
    else if (outcome == 'pneumonia') {
      ranked_state <- outcome_data[order(outcome_data[19]),2][num]
    }
    
  }
  else if (!is.element(state, valid_states)) {
    stop("invalid state")
  }
  else if (!is.element(outcome, valid_outcomes)) {
    stop("invalid outcome")
  }
  return(ranked_state)
}