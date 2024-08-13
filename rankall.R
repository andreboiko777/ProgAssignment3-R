## for this function/script to work 
## it is necessary to have the file "rankhospital.R"
## in the same folder as this script

## Ranking hospital in all states
rankall <- function(outcome, num = "best") {
  ## Sourcing rankhospital formula
  source("rankhospital.R")
  
  ## Read the outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Get a list of states
  states <- unique(data$State)
  
  ## Create a vector to store results
  results <- vector("list", length(states)) 
  
  ## Loop through each state and get the hospital of the given rank
  for (i in seq_along(states)) {
    state <- states[i]
    hospital <- rankhospital(state, outcome, num)
    results[[i]] <- c(hospital, state)
  }
  
  ## Converting the results to a df
  results_df <- do.call(rbind, results)
  colnames(results_df) <- c("hospital", "state")
  
  ## Sot the df by state
  results_df <- as.data.frame(results_df)
  results_df <- results_df[order(results_df$state), ]
  
  return(results_df)
}
