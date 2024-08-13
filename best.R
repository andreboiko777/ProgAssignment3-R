## Finding the best hospital in a state
best <- function(state, outcome) {
  ## Read the outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Setting the valid state and outcome names
  valid_states <- unique(data$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  outcomes_col <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  names(outcomes_col) <- outcomes
  
  ## Checking if they are valid
  if(!(state %in% valid_states)) {
    stop("invalid state")
  }
  if(!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  
  ## Subset data for given state
  state_data <- subset(data, State == state)
  
  ## Set a variable with column for outcome name
  sickness <- outcomes_col[outcome]
  
  ## Replacing "Not available" with NA
  state_data[[sickness]][state_data[[sickness]] == "Not Available"] <- NA
  
  ## Convert outcome data to numeric 
  state_data[[sickness]] <- as.numeric(state_data[[sickness]])
  
  ## Removing NA values
  state_data <- state_data[!is.na(state_data[[sickness]]), ]
  
  ## Setting a "value" variable
  value <- state_data[[sickness]]
  
  ## Searching for the minimum value
  min_value <- min(value, na.rm = TRUE)
  best_hospitals <- state_data[value == min_value, "Hospital.Name"]
  
  ## Handling ties
  best_hospital <- sort(best_hospitals)[1]
  
  ## Returning hospital that matches
  return(best_hospital)

}