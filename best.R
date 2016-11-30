best <- function(state,outcome)
{
  ## reads the data from csv
  hos_best <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##checks if args are valid and returns error if they are invalid
  check1 <- state %in% hos_best$State
  check2 <- outcome %in% c("heart attack","heart failure","pneumonia")
  if (check1 == FALSE)
  {
    stop ('invalid state')
  }
  if (check2 == FALSE)
  {
    stop ('invalid outcome')
  }
  ##subsets data to only required state
  hos_best2 <- subset(hos_best,hos_best$State==state)
  ##gets the column of required outcome
  if (outcome == "heart attack")
   {  hos_best2 <- hos_best2[,c(2,7,11)]  }
  else if (outcome == "heart failure")
   {  hos_best2 <- hos_best2[,c(2,7,17)]  }
  else 
  {  hos_best2 <- hos_best2[,c(2,7,23)]   }
  ##inserts NA values if outcome values are Not Available
  hos_best2[hos_best2=="Not Available"] <- NA
  ##removes NA's
  hos_best2 <- hos_best2[complete.cases(hos_best2),]
  ##converts the outcome values character to numeric
  hos_best2[,3] <- as.numeric(hos_best2[,3])
  ##Gets minimum value of outcome value
  value <- min(hos_best2[,3])
  ##Gives best hospitals
  best_hosp <- hos_best2[hos_best2[,3]==value,1]
  ##If there is a tie orders the hospitals alphabetically
  if(length(best_hosp>1))
  {
    best_hosp <- sort(best_hosp)
  }
  ##returns the best hospital
  return (best_hosp[1])
}