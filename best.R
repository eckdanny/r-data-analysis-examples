# Week 3, Programming Assignment part 5
best <- function(state, outcome) {
  # Finding the best hospital in a state
  #
  # Args:
  #   state: Character vector of length 1 which identifies the state using the
  #          2-character abbreviation.
  #   outcome: Character vector of lenght 1 indicating outcome name.
  #
  # Returns:
  #   Returns a character vector of length 1 indicating the name of the hospital
  #   (Hospital.name) with the lowest 30-day mortality for specified state and
  #   outcome.

  # Set Working Directory
  setwd('/home/deck/Dropbox/coursera/DataAnalysis/pa3/')

  load <- function() {
    # Read outcome data
    data <- read.csv(
      file = "data/outcome-of-care-measures.csv",
      colClasses = "character")
    # Make more readable column names
    pretty.colnames <- c("heart_attack", "heart_failure", "pneumonia")
    num.colnames <- c(11, 17, 23)
    # Coerce to numeric
    for(i in num.colnames) {
      data[, i] <- as.numeric(data[, i])
    }
    colnames(data)[num.colnames] <- pretty.colnames
    data
  }
  # Load data
  data <- suppressWarnings(load())
  # Coerce to character
  state <- as.character(state)
  outcome <- as.character(outcome)
  # Validate state
  if (!(state %in% unique(data$State))) {
    stop("invalid state")
  }
  # Validate outcome
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  # Return hospital name in that state with lowest 30-day death rate
  data.state <- data[which(data$State == state), ]
  j <- which(colnames(data.state) == sub(" ", "_", outcome, fixed = TRUE ))
  data.state.o <- data.state[order(data.state[, j], data.state[, 2]), ]
  data.state.o[1, 2]
}
