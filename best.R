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
  
  #extracting column of death rates for the given outcome in the given state
  
    #subsetting according to state code
    rows_to_use<-table19[which(table19$State==state),]
  
    #subsetting according to outcome
    if(outcome==possible_outcomes[1]) raw_data<-rows_to_use[,c(2,11)]
    if(outcome==possible_outcomes[2]) raw_data<-rows_to_use[,c(2,17)]
    if(outcome==possible_outcomes[3]) raw_data<-rows_to_use[,c(2,23)]
  
  #removing NAs
  rates_data<-raw_data[!is.na(raw_data[,2]),]
  
  #sorting rates in ascending order, using hospital names to break ties
  sorted_rates<-order(rates_data[,2],rates_data[,1],dec=F)
  
  #returns hospital name
  sorted_rates[[1,1]]
  
}