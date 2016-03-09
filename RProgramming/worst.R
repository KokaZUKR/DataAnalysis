worst <- function(state, outcome) {
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
      max_ha <- max(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm = TRUE)
      outcome_data <- na.omit(outcome_data)
      worst_state <- outcome_data[outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == max_ha , 2]
    }
    else if (outcome == 'heart failure') {
      max_hf <- max(TX_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm = TRUE)
      worst_state <- outcome_data[outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == max_hf , 2]
    }
    else if (outcome == 'pneumonia') {
      max_pm <- max(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm = TRUE)
      worst_state <- outcome_data[outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == max_pm , 2]
    }
    
  }
  else if (!is.element(state, valid_states)) {
    stop("invalid state")
  }
  else if (!is.element(outcome, valid_outcomes)) {
    stop("invalid outcome")
  }
  worst_state <- worst_state[!is.na(worst_state)]
  if (length(worst_state) > 1) {
    worst_state <- sort(worst_state)[1]
    return(worst_state)
  }
  else return(worst_state)
}