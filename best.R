best <- function(state,outcome) {
    ##read outcome data into R
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## look at data 
    colnames(data) 
    ##col 2 has hosp name, col 7 has states
    ##cols 11 (heart attack),17(heart failure),23(pneumonia)
    
    ## check if state and outcome are valid 
    if (!any(data['State']==state)) {
        stop('Invalid State')
    }
    
    col = integer()
    if(outcome == "heart attack") 
        col <- 11
    else if(outcome == "heart failure") 
        col <- 17
    else if(outcome == "pneumonia") 
        col <- 23
    else  
        stop('Invalid Outcome')
    
    ## Return hospital name in that state with lowest 30-day death rate
    hosprank <- data[data$State == state,]
    hosprank[, col] <- suppressWarnings(as.numeric(hosprank[,col]))
    
    ## remove missing values
    cc <- complete.cases(hosprank)
    hosprank <- hosprank[cc,]
    
    ## return hospital based on alphabetical order
    hosprank[which(hosprank[,col] == min(hosprank[,col])),][,2][[1]]

}
