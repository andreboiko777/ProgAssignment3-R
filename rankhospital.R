## Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
  ## Read the outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Setting the valid state and outcome names
  valid_states <- unique(data$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  outcomes_col <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  names(outcomes_col) <- outcomes
  
  ## Checking to see if they are valid
  if(!(state %in% valid_states)) {
    stop("invalid state")
  }
  if(!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  
  ## Subset the data for the given state
  state_data <- subset(data, State == state)
  
  ## Set a variable with the outcome "sickness"
  sickness <- outcomes_col[outcome]
  
  ## Replacing "Not available" with NA
  state_data[[sickness]][state_data[[sickness]] == "Not Available"] <- NA
  
  ## Convert outcome data to numeric 
  state_data[[sickness]] <- as.numeric(state_data[[sickness]])
  
  ## Removing NA
  state_data <- state_data[!is.na(state_data[[sickness]]), ]

  ## Creating a data.frame with Hospital.Name and mortality rate
  best_hospitals <- state_data[, c("Hospital.Name", sickness)]
  
  ## Sorting the df by Mortality rate in ascending order, and handling ties
  ## with alphabetical order
  best_hospitals <- best_hospitals[order(best_hospitals[[sickness]], state_data[["Hospital.Name"]]), ]
  
  ## Returning depending on num (default = best)
  if(num == "best") {
    return(best_hospitals[1, "Hospital.Name"])
  } 
  if(num == "worst") {
    return(best_hospitals[nrow(best_hospitals), "Hospital.Name"])
  } else {
    return(best_hospitals[num, "Hospital.Name"])
  }

  
}
