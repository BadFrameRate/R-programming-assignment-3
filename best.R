best<-function(state,outcome){
  
  #Read "outcome-of-care-measures" data
  table19=read.csv("outcome-of-care-measures.csv")
  
  #check outcome validity
  possible_outcomes=c("heart attack","heart failure","pneumonia")
  for(i in seq_along(possible_outcomes))
    if(!(any(outcome==possible_outcomes)==T)) stop("invalid outcome")
  
  #check state code validity
  possible_state_codes=table19[,7]
  for(i in seq_along(possible_state_codes))
    if(!(any(state==possible_state_codes)==T)) stop("invalid state")
  
}