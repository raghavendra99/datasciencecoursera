rankall <- function(outcome,num="best")
 {
  ## reads the data from csv
  hos_best <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##checks if args are valid and returns error if they are invalid
  check1 <- outcome %in% c("heart attack","heart failure","pneumonia")
  check2 <- is.numeric(num)
  if (check1 == FALSE)
  {
    stop ('invalid outcome')
  }
  if (check2 == FALSE)
  {
    if ((num %in% c("best","worst")) ==FALSE)
      { stop ('invalid rank') }
  }
  ##gets the column of required outcome
  if (outcome == "heart attack")
  {  hos_best2 <- hos_best[,c(2,7,11)]  }
  else if (outcome == "heart failure")
  {  hos_best2 <- hos_best[,c(2,7,17)]  }
  else 
  {  hos_best2 <- hos_best[,c(2,7,23)]   }
  ##inserts NA values if outcome values are Not Available
  hos_best2[hos_best2=="Not Available"] <- NA
  ##removes NA's
  hos_best2 <- hos_best2[complete.cases(hos_best2),]
  ##converts the outcome values character to numeric
  hos_best2[,3] <- as.numeric(hos_best2[,3])
  ##Orders the hospitals alphabetically
  hos_best2 <- hos_best2[order(hos_best2[,1]),]
  ##Assigns the rank to every hospital based on outcome and state
  hos_best3 <- transform(hos_best2, rank = ave(hos_best2[,3],hos_best2$State, FUN = function(x) rank(x, ties.method = "first")))
  ##Gives the values of all states
  all_state <- unique(hos_best3[,2])
  ##If rank is best gives all haspitals which have rank 1
  if(num=="best")
  { best_hosp <- hos_best3[hos_best3[,4]==1,c(1,2)]
    best_hosp <- best_hosp[order(best_hosp[,2]),]
  }
  ##If rank is best gives all haspitals which have maximum rank 
  else if (num=="worst")
  {
    vol <- aggregate(hos_best3$rank,by=list(hos_best3$State),max)
    colnames(vol) <- c("State","rank")
    vol2 <- merge(vol,hos_best3)
    best_hosp <- vol2[,c(3,1)]
  }
  ##If rank is any integer
  else
  {
      ##Gets the hospital.name and state column
      hos_best4 <- hos_best3[hos_best3[,4]==num,c(1,2)]
      ##Loops through all states and checks if all states have given rank
      for (i in all_state)
        {     
            if((i %in% hos_best4[,2])==FALSE)
              {
                ##Adds NA if state doesn't have a rank
                hos_best4 <- rbind(hos_best4,c(NA,i))
              }
      }
      ##orders the states alphabetically
       best_hosp <- hos_best4[order(hos_best4[,2]),]
  }    
   ## Assigns rownames and colnames to all rows as states
   rownames(best_hosp) <- best_hosp[,2]
   colnames(best_hosp) <- c("hospital","state")
   return(best_hosp)
}