rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that state and outcome are valid
    col = integer()
    if(outcome == "heart attack") {
        col <- 11
    }
    else if(outcome == "heart failure") {
        col <- 17
    }
    else if(outcome == "pneumonia") {
        col <- 23
    }
    else  {
        stop('Invalid Outcome')
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    
    ## Cast outcome column to numeric
    data[ ,col] <- suppressWarnings(as.numeric(data[ ,col]))
    
    ## Hospitals that do not have data on a particular outcome could be excluded
    data <- data[complete.cases(data), ]  
    
    ## Sort The hospitals 
    hosprank <- data[order(data$State, data[,col], data$Hospital.Name), ]
    
    ##Find the levels, just for information, 7 being State
    states<- factor(hosprank[ , 7])
    
    results <- list()
    if(num == "best") {
        results <- tapply(hosprank[['Hospital.Name']], states, function(name) { return(name[1]) })
    }
    else if(num == "worst") {
        results <- tapply(hosprank[['Hospital.Name']], states, function(name) { return(name[length(name)]) })
    }
    else {
        results <- tapply(hosprank[['Hospital.Name']], states, function(name) { return(name[num]) })
    }
    
    ##get the hospital name in the alphabetical order
    data.frame(hospital = results, state = names(results))
}
