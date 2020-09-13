rankall<-function(state,outcome,num="best"){
  
  #read "outcome-of-care-measures" data
  table19=read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  #check outcome validity
  possible_outcomes=c("heart attack","heart failure","pneumonia")
  if(!(any(outcome==possible_outcomes)==T)) stop("invalid outcome")
  
  #check state code validity
  possible_state_codes=table19[,7]
  if(!(any(state==possible_state_codes)==T)) stop("invalid state")
  
  
}