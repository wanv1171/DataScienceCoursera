rankhospital <- function(state, outcome, num = "best") {
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (!(state %in% outcomeData$State)) {
    stop("invalid state", call. = TRUE)
  }
  else if (!(outcome %in% c("heart attack","pneumonia","heart failure"))) {
    stop("invalid outcome", call. = TRUE)
  }
  else {
    outcomeData <- outcomeData[, !grepl("Number", names(outcomeData)) & 
                                 !grepl("Estimate", names(outcomeData)) & 
                                 !grepl("Footnote", names(outcomeData)) & 
                                 !grepl("Comparison", names(outcomeData)) &
                                 (grepl("Hospital.Name", names(outcomeData)) |
                                    grepl("State", names(outcomeData)) |
                                    grepl("Mortality", names(outcomeData)))]
    
    outcomeData[,3:5] <- apply(outcomeData[,3:5], 2, as.numeric)
    filteredOutcome <- outcomeData[rowSums(is.na(outcomeData)) == 0,]
    
    symptom <- 0
    
    if (outcome == "heart attack") {
      symptom <- 3
    }
    else if (outcome == "heart failure") {
      symptom <- 4
    }
    else {
      symptom <- 5
    }
    
    stateOutcome <- filteredOutcome[filteredOutcome$State == state,]
    orderedStateOutcome <- stateOutcome[order(stateOutcome[,symptom], stateOutcome[,"Hospital.Name"]),]
    
    if (num == "best") {
      return(c(orderedStateOutcome[1,"Hospital.Name"]))
    }
    else if (num == "worst") {
      return(c(orderedStateOutcome[nrow(orderedStateOutcome),"Hospital.Name"]))
    }
    else {
      return(c(orderedStateOutcome[as.numeric(num),"Hospital.Name"]))
    }
  }
  
  return()
}
