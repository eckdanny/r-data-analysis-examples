# Week 3 - Programming Assignment part 7
rankall <- function(outcome, num = "best") {
  # Ranking Hospitals in all States
  #
  # Args:
  #   outcome: Character vector of length 1 indicating outcome name.
  #   num: Numeric vector of length 1 indicating (alternatively 'best' or 'worst')
  # Returns:
  #   2-column data frame with hospital names and 2-character state abbreviation
  #   for all 50 states (some may contain NAs). Columns should be named
  #   'hospital' and 'state'. Hospitals that do not have data on a particular
  #   outcome should be excluded prior to determining rankings

  load <- function() {
    # Read outcome data
    data <- read.csv(
      file = "data/outcome-of-care-measures.csv",
      colClasses = "character")
    # Extract only useful cols
    data <- data[, c(7, 2, 11, 17, 23)]
    # Make more readable column names
    colnames(data) <- c("state",
                        "hospital_name",
                        "heart_attack",
                        "heart_failure",
                        "pneumonia")
    # Coerce numeric cols
    for(i in seq.int(3,5)) {
      suppressWarnings(data[, i] <- as.numeric(data[, i]))
    }
    data
  }
  # Read outcome data
  data <- load()
  # Validate outcome
  outcome <- as.character(outcome)
  if (!(sub(" ", "_", outcome, fixed = TRUE) %in% colnames(data)[seq(3,5)])) {
    stop("invalid outcome")
  }
  # Validate num
  if (class(num) == "character") {
    if (!(num %in% c("best", "worst"))) {
      stop("invalid num")
    }
  }
  # Initialize empty vectors for data frame
  df.hospital <- character(0)
  df.state <- character(0)
  # For each state, find the hospital of the given rank for given outcome
  col <- sub(" ", "_", outcome, fixed = TRUE)
  for (state in sort(unique(as.character(tmp$state))))
  {
    o <- data[data$state == state, ]
    o <- o[order(o[, col], o$hospital_name, na.last = NA), ]

    if (class(num) == "character") {
      if (num == "best") {
        this.num <- 1
      } else {
        this.num <- nrow(o)
      }
    } else {
      this.num <- num
    }

    if (this.num > nrow(o)) {
      this.hospital <- NA
    } else {
      this.hospital <- o$hospital[this.num]
    }

    df.hospital <- c(df.hospital, this.hospital)
    df.state <- c(df.state, state)
  }
  # Return a data frame
  output <- data.frame(hospital = df.hospital, state = df.state)
}
