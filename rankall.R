rankall <- function(outcome, num = "best") {
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Check to ensure outcome is correct (this is a large loop encapsulating most of the code)
if (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") {
    data[,11] <- as.numeric(data[,11])
    data[,17] <- as.numeric(data[,17])
    data[,23] <- as.numeric(data[,23])
    x <- na.omit(data[, c(2, 7, 11, 17, 23)])

    unique_States <- sort(unique(data$State))
    for (i in 1:length(unique_States)) {
         subState <- split(x, x$State)
    }
    if (num == "best") {num <- 1}
 ## Begin the ranking by outcome
    if (outcome == "heart attack") {
        for (i in 1:length(unique_States)) {
            subState[[i]] <- subState[[i]][order(subState[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,subState[[i]]$Hospital.Name, subState[[i]]$State),]
        }
       final <- data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
    for (i in 1: length(unique_States)) {

        final[i,] <- subState[[i]][1,]
 #       print(subState[[i]][1,])
    }
    return(final)
    }
## end outcome heart attack
    if (outcome == "heart failure") {

    }
## end outcome heart failure
    if (outcome == "pneumonia") {

    }
## end outcome pneumonia
## End the ranking by outcome
    } else {
## End - Check to ensure outcome is correct (this is a large loop encapsulating most of the code)
        stop("invalid outcome")
    }
    #return(hs)
}


