rankhospital <- function(state=".", outcome=".", num= "best") {
  ## Read outcome data
  thedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  ## 
  ## Check that state and outcome are valid
  if (!(state %in% thedata[,7]))
    stop("invalid state")
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!(outcome %in% outcomes))
    stop("invalid outcome")
  
  if ((num!= "best")&&(num!="worst"))
    num <- as.numeric(num)
  
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
  
  subdata <- thedata[c(2,7, neededCol)] # get hospital name, state, and rate
  subdata[, 3] <- as.numeric(subdata[, 3]) #make sure rate is a numeric value
  subdata2 <- subdata[subdata$State==state,] # look at specific state
  
  
  ##data is now filtered down to the appropriate columns
  ##and the appropriate state, now we just need to
  ##find the entry that we were asked to find
  
  ##order the list first based on the rate column, secondly based
  ##on the hospital's name
  ordered<-subdata2[ order(subdata2[,3], subdata2[,1]),] # order the data
  ordered<-ordered[complete.cases(ordered),]
  if(num=="best")
    finalName<- ordered[1,1]
  else if (num=="worst")
    finalName<- ordered[nrow(ordered),1]
  else{
    if(num > nrow(ordered))
      return (NA)
    else finalName<- ordered[num,1]
  }
    
  
  return(finalName)
}