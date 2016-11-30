rankhospital <- function(state,outcome,num="best")
{
  ## reads the data from csv
  hos_best <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##checks if args are valid and returns error if they are invalid
  check1 <- state %in% hos_best$State
  check2 <- outcome %in% c("heart attack","heart failure","pneumonia")
  check3 <- is.numeric(num)
  if (check1 == FALSE)
  {
    stop('invalid state')
  }
  if (check2 == FALSE)
  {
    stop ('invalid outcome')
  }
  if (check3 == FALSE)
  {
    if ((num %in% c("best","worst")) ==FALSE)
      {  stop ('invalid rank')  }
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
  ##Orders the hospitals alphabetically
  hos_best2 <- hos_best2[order(hos_best2[,1]),]
  ##Assigns rank of each hospital 
  hos_best2[,4] <- rank(hos_best2[,3],ties.method = "first")
  ##if rank is worst sets num to max rank
  if (num=="worst")
  { num <- max(hos_best2[,4])}
  ##if rank is best sets num to 1
  if(num=="best")
  { num <- 1}
  ##checks given rank is present for given state
  if(num < nrow(hos_best2)+1)
  {  best_hosp <- hos_best2[hos_best2[,4]==num,1] }
  else
  {  best_hosp <- NA}
  ##returns the hospital with given rank
  return(best_hosp) 
}