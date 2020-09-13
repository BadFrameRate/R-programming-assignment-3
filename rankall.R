rankall<-function(state,outcome,num="best"){
  
  #read "outcome-of-care-measures" data
  table19=read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  #check outcome validity
  possible_outcomes=c("heart attack","heart failure","pneumonia")
  if(!(any(outcome==possible_outcomes)==T)) stop("invalid outcome")
  
  #check state code validity
  possible_state_codes=unique(table19[,7])
  if(!(any(state==possible_state_codes)==T)) stop("invalid state")
  
  #subsetting according to outcome
  if(outcome=possible_outcomes[1]) all_rates<-table19[,c(2,7,11)]
  if(outcome=possible_outcomes[2]) all_rates<-table19[,c(2,7,17)]
  if(outcome=possible_outcomes[3]) all_rates<-table19[,c(2,7,23)]
  
  #explicit NA removal
  na_free_rates<-all_rates[!is.na(as.numeric(all_rates[,3])),]
}