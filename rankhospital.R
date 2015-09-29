rankhospital <- function(state, outcome, num = "best") {
  ## Function called rankhospital that takes three arguments: the 2-character abbreviated name of a
  ## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
  ## The function returns returns a character vector with the name
  ## of the hospital that has the ranking specified by the num argument.
  ## The outcomes can be one of "heart attack", "heart failure", or "pneumonia". 
  ## The num argument can take values "best", "worst", or an integer indicating the ranking
  ## (smaller numbers are better).
  ## If the number given by num is larger than the number of hospitals in that
  ## state, then the function should return NA.
  ## Hospitals that do not have data on a particular outcome 
  ## should be excluded from the set of hospitals when deciding the rankings.
  ## If there is a tie for the best hospital for a given outcome, then the hospital names should
  ## be sorted in alphabetical order and the first hospital in that set should be chosen.
  
  ## Read outcome data
  fname <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  
  ## Column position/name of interest for hospital name
  ## [2] "Hospital.Name"
  
  ## Column position/name of interest for state
  ## [7] "State"
  
  ## Column name of interest for different outcome mortality rates
  ## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"   -> "heart attack"
  ## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"  -> "heart failure"
  ## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"      -> "pneumonia"
  
  ## Check that state and outcome are valid
  ## The function should check the validity of its arguments. 
  
  ## If an invalid state value is passed to best, the function should throw an error via the stop function 
  ## with the exact message "invalid state". 
  if (nrow(subset(fname, fname[,7]==state, select = c(2)))==0) 
  {
    stop("invalid state")
  }
  
  ## If an invalid outcome value is passed to best, the function should throw an error via the stop function 
  ## with the exact message "invalid outcome"
  else if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia") 
  {
    stop("invalid outcome")
  }
  
  else
  {
    if (outcome == "heart attack") {ndx=11}
    else if (outcome == "heart failure") {ndx=17}
    else {ndx=23}
    
    t<-subset(fname, fname[,7]==state , select = c(2,7,ndx)) 
    u<-suppressWarnings(order(as.numeric(t[,3]),t[,1],na.last = NA))
    
    ## Return hospital name in that state with given rank 30-day death rate
    if (num=="best") {
      return(t[u[1],1])
    }
    else if (num=="worst") {
      return(t[u[length(u)],1])
    }
    else if (as.numeric(num)>length(u)) {
      return(NA)
    }
    else{
      return(t[u[as.numeric(num)],1])
    }
    
  }
  
}