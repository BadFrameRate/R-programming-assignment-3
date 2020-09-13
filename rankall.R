rankall<-function(outcome,num="best"){
  
  #read "outcome-of-care-measures" data
  table19=read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  #check outcome validity
  possible_outcomes<-c("heart attack","heart failure","pneumonia")
  if(!(any(outcome==possible_outcomes)==T)) stop("invalid outcome")
  
  #vector of all unique state codes in alphabetical order
  possible_state_codes<-sort(unique(table19[,7]))
  
  #subsetting according to outcome
  if(outcome==possible_outcomes[1]) all_rates<-table19[,c(2,7,11)]
  if(outcome==possible_outcomes[2]) all_rates<-table19[,c(2,7,17)]
  if(outcome==possible_outcomes[3]) all_rates<-table19[,c(2,7,23)]
  
  #explicit NA removal
  na_free_rates<-all_rates[!is.na(as.numeric(all_rates[,3])),]
  
  #splitting the data according to states
  split_data<-split(na_free_rates[,c(1,3)],na_free_rates[,2])
  
  #order data in every state using anonymous function
  ordered_state_data<-lapply(split_data,function(df) df<-df[order(df[,2],df[,1]),])
  
  #indexing "best" and numeric values for 'num' argument
  if(num=="best") i<-1
  if(!is.na(as.numeric(num))) i<-as.numeric(num)
  
  #vector of required ranks' hospital names
  hospital_name<-character()
  
  #loop for extracting required hospital name of every state
  for(j in seq_along(possible_state_codes)){
    curr_df<-ordered_state_data[[which(names(ordered_state_data)==possible_state_codes[j])]]
    if(num=="worst") i<-length(curr_df[,2]) #index if 'num' argument is "worst"
    hospital_name[j]<-curr_df[i,1]
  }
  
  output<-data.frame("hospital"=hospital_name,"state"=possible_state_codes)
  output
}