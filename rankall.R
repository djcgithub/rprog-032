rankall <- function(outcome, num = "best") {
  ## function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num).
  ## The function returns returns 2-column data frame
  ## containing the hospital in each state that has the ranking specified in num.
  ## The function should return a value for every state (some may be NA).
  ## The outcomes can be one of "heart attack", "heart failure", or "pneumonia". 
  ## The num argument can take values "best", "worst", or an integer indicating the ranking
  ## (smaller numbers are better).
  ## If the number given by num is larger than the number of hospitals in that
  ## state, then the function should return NA.
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
  
  v<-data.frame()
  
  ## Check that outcome is valid
  ## If an invalid outcome value is passed to best, the function should throw an error via the stop function 
  ## with the exact message "invalid outcome"
  if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia") 
  {
    stop("invalid outcome")
  }
  
  else
  {
    if (outcome == "heart attack") {ndx=11}
    else if (outcome == "heart failure") {ndx=17}
    else {ndx=23}
    
    for (state in unique(fname[,7])){
      t<-subset(fname, fname[,7]==state , select = c(2,7,ndx))
      u<-suppressWarnings(order(as.numeric(t[,3]),t[,1],na.last = NA))
      
      ## Return hospital name state with given rank 30-day death rate
      if (num=="best") {
        v<-rbind(v,(t[u[1],c(1,2)]))
      }
      else if (num=="worst") {
        v<-rbind(v,(t[u[length(u)],c(1,2)]))
      }
      else if (as.numeric(num)>length(u)) {
        v<-rbind(v,c(NA,state))
      }
      else{
        v<-rbind(v,(t[u[as.numeric(num)],c(1,2)]))
      }  
     }
  }

  row.names(v)<-unique(fname[,7])
  names(v)<-c("hospital","state")
  y<-order(unique(fname[,7]),na.last = NA)
  v[y,]
  
}