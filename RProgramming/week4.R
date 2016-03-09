outcome <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
head(outcome)
#Hist of "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

options(warn = -1)

source('rankall.R')
rankall("heart failure", 10)
