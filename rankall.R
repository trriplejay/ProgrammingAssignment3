rankall <- function(outcome=".", num= "best") {
  ## Read outcome data
  thedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  ## 
  ## Check that outcome is valid
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
  
  #order the data first by state, then by rate, then by name
  subdataX <- subdata[order(subdata[,2], subdata[,3], subdata[,1]),]
  
  statelist<- unique(subdataX[,2]) #make a list of all unique states
  #statelist<- order(statelist[1])
  
  ##data is now filtered down to the appropriate columns
  ##and the appropriate state, now we just need to
  ##find the entry that we were asked to find
  
  x<-character() #vector of state
  y<-character() #vector of hostpital name
  counter<-1
  for (st in statelist){
    #find the right row for each state, and add it
    # to the final result
    x[counter]<-st
    #filter down to current state
    tempdata <- subdataX[subdataX$State==st,] # look at specific state
    tempdata<- tempdata[complete.cases(tempdata),]
    if(num=="best")
      y[counter] <- tempdata[1,1]
    else if (num=="worst")
      y[counter]<- tempdata[nrow(tempdata),1]
    else{
      if(num > nrow(tempdata))
        y[counter]<- NA
      else y[counter]<- tempdata[num,1]
    }
    counter = counter + 1
    
  }
  finalResult = data.frame(y, x)
  colnames(finalResult)<- c("hospital", "state")
  rownames(finalResult)<-statelist
    
  return(finalResult)
}
