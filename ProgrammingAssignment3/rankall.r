rankall <- function(outcome, num="best") {
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (!(outcome %in% c("heart attack","pneumonia","heart failure"))) {
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
    
    suppressWarnings(outcomeData[,3:5] <- apply(outcomeData[,3:5], 2, as.numeric))
    
    
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
    
    outcomeData <- outcomeData[!is.na(outcomeData[,symptom]),]
    outcomeByState <- split(outcomeData, outcomeData$State)
    
    returnDataFrame <- data.frame("hospital name" = character(), "state" = character())
    
    if (num == "best") {
      for (index in 1:length(outcomeByState)) {
        orderedStateOutcome <- outcomeByState[[index]][order(outcomeByState[[index]][,symptom], outcomeByState[[index]][,"Hospital.Name"]),]
        returnDataFrame <- rbind(returnDataFrame, data.frame("hospital.name" = orderedStateOutcome[1,"Hospital.Name"], "state" = names(outcomeByState[index])))
      }
    }
    else if (num == "worst") {
      for (index in 1:length(outcomeByState)) {
        orderedStateOutcome <- outcomeByState[[index]][order(outcomeByState[[index]][,symptom], rev(outcomeByState[[index]][,"Hospital.Name"]), decreasing = TRUE),]
        returnDataFrame <- rbind(returnDataFrame, data.frame("hospital.name" = orderedStateOutcome[1,"Hospital.Name"], "state" = names(outcomeByState[index])))
      }
    }
    else {
      for (index in 1:length(outcomeByState)) {
        orderedStateOutcome <- outcomeByState[[index]][order(outcomeByState[[index]][,symptom], outcomeByState[[index]][,"Hospital.Name"]),]
        returnDataFrame <- rbind(returnDataFrame, data.frame("hospital.name" = orderedStateOutcome[as.numeric(num),"Hospital.Name"], "state" = names(outcomeByState[index])))
      }
    }
    return(returnDataFrame)
  }
  
  return()
}
