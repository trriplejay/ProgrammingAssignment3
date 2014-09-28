best <- function(state=".", outcome=".") {
  ## Read outcome data
  thedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  ## 
  ## Check that state and outcome are valid
  if (!(state %in% thedata[,7]))
    stop("invalid state")
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(outcome %in% outcomes))
    stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ##figure out which column we need in addition to "State" based on given outcome
  neededCol<-NA
  if(outcome == outcomes[1]) #heart attack maps to column 11
    neededCol<-11
  else if(outcome == outcomes[2])
    neededCol<-17
  else if(outcome == outcomes[3])
    neededCol<-23
  
  #now we know which columns we care about, so take a subset
  
  subdata <- thedata[c(2,7, neededCol)]
  subdata[, 3] <- as.numeric(subdata[, 3])
  subdata2 <- subdata[subdata$State==state,]
  
  ##data is now filtered down to the appropriate columns
  ##and the appropriate state, now we just need the
  ## smallest value for that state
  
  ##order the list first based on the rate column, secondly based
  ##on the hospital's name
  ordered<-subdata2[ order(subdata2[,3], subdata2[,1]),]
  finalName<- ordered[1,1]
  return(finalName)
}