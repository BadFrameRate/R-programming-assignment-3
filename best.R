best<-function(state,outcome){
  
  #read "outcome-of-care-measures" data
  table19=read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  #check outcome validity
  possible_outcomes=c("heart attack","heart failure","pneumonia")
  if(!(any(outcome==possible_outcomes)==T)) stop("invalid outcome")
  
  #check state code validity
  possible_state_codes=unique(table19[,7])
  if(!(any(state==possible_state_codes)==T)) stop("invalid state")
  
  #extracting column of death rates for the given outcome in the given state
  
    #subsetting according to state code
    rows_to_use<-table19[which(table19$State==state),]
  
    #subsetting according to outcome
    if(outcome==possible_outcomes[1]) rates_data<-rows_to_use[,c(2,11)]
    if(outcome==possible_outcomes[2]) rates_data<-rows_to_use[,c(2,17)]
    if(outcome==possible_outcomes[3]) rates_data<-rows_to_use[,c(2,23)]
  
  na_free_rates<-rates_data[!is.na(as.numeric(rates_data[,2])),]
    
  #sorting rates in ascending order, using hospital names to break ties
  sorted_rates<-rates_data[order(as.numeric(na_free_rates[,2]),na_free_rates[,1]),]
  
  #returning hospital name
  sorted_rates[1,1]
}