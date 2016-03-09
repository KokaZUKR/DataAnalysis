best <- function(state, outcome) {
  outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  outcome_data <- na.omit(outcome_data)
  outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  valid_states <- unique(outcome_data$State)
  valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if (is.element(state, valid_states) & is.element(outcome, valid_outcomes)) {
    outcome_data <- outcome_data[outcome_data$State == state,]
    if (outcome == 'heart attack') {
      min_ha <- min(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm = TRUE)
      outcome_data <- na.omit(outcome_data)
      best_state <- outcome_data[outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min_ha , 2]
    }
    else if (outcome == 'heart failure') {
      min_hf <- min(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm = TRUE)
      best_state <- outcome_data[outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min_hf , 2]
    }
    else if (outcome == 'pneumonia') {
      min_pm <- min(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm = TRUE)
      best_state <- outcome_data[outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min_pm , 2]
    }
    
  }
  else if (!is.element(state, valid_states)) {
    stop("invalid state")
  }
  else if (!is.element(outcome, valid_outcomes)) {
    stop("invalid outcome")
  }
  best_state <- best_state[!is.na(best_state)]
  if (length(best_state) > 1) {
    best_state <- sort(best_state)[1]
    return(best_state)
  }
  else return(best_state)
}