rankall <- function(outcome, num = "best") {
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Check to ensure outcome is correct (this is a large loop encapsulating most of the code)
if (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") {
## Initialize the working data frame "hs" - stands for "Hospital | State" - which is the required output of the function) with the subset of data we'll be working with.  The end result of each "if (outcome == "xxx) {}" code blocks below should be just the first two columns of this data frame for each state.
    hs <- data.frame(Hospital=character(),State=character(),HA.Rank=integer(),HF.Rank=integer(),PN.Rank=integer(),HA.Mor.Rate=integer(),HF.Mor.Rate=integer(),PN.Mor.Rate=integer(),stringsAsFactors=FALSE)
## the columns that contain comparison data need to be converted from string to numeric
    data[,11] <- as.numeric(data[,11])
    data[,17] <- as.numeric(data[,17])
    data[,23] <- as.numeric(data[,23])
## Subset the entire data frame loaded from csv to just the columns we'll work with. Columns 3,4,5 below (set to NA) are not present in the file, these will be calculated columns for the rankings by outcome.
    for (i in 1: nrow(data)) {
        hs[i,1] <- data$Hospital.Name[i]
        hs[i,2] <- data$State[i]
        hs[i,3] <- NA  ##Heart Attack Mor. Rate Ranking (calculated field)
        hs[i,4] <- NA  ##Heart Failure Mor. Rate Ranking (calculated field)
        hs[i,5] <- NA  ##Pneumonia Mor. Rate Ranking (calculated field)
        hs[i,6] <- data[i,11]
        hs[i,7] <- data[i,17]
        hs[i,8] <- data[i,23]
    }
## Make state data frames
    unique_States <- sort(unique(hs$State))
    for (i in 1:length(unique_States)) {
        subState <- split(hs, hs$State)
    }
##Set the Ranks per state for each outcome
    for (i in 1:length(unique_States)) {
        subState[[i]]["HA.Rank"] <- rank(subState[[i]]["HA.Mor.Rate"],ties.method="first")
        subState[[i]]["HF.Rank"] <- rank(subState[[i]]["HF.Mor.Rate"],ties.method="first")
        subState[[i]]["PN.Rank"] <- rank(subState[[i]]["PN.Mor.Rate"],ties.method="first")
    }
    if (num == "best") {num <- 1}
 ## Begin the ranking by outcome
    if (outcome == "heart attack") {
        x <- vector("numeric",length=length(unique_States))
        for (i in 1:length(unique_States)) {
            ##check to make sure there is a hospital with the requested ranking
            noHospWithNumRank <- tryCatch(
                x[i] <- which(subState[[i]]["HA.Rank"]==num),
                error=function(e) e
            )
            if(!inherits(noHospWithNumRank, "error")) {
            ##returns 0 rows for this state if there is not a hospital with requested rating
            }
        }
        final <- data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
        for (i in 1: length(x)) {
            subState[[i]] <- subState[[i]][x[i],1:2]
            noHospWithNumRank <- tryCatch(
                final[i,] <- subState[[i]],
                error=function(e) e
            )
            if(!inherits(noHospWithNumRank, "error")) {
                ##returns 0 rows for this state if there is not a hospital with requested rating
            }
        }
    return(subState)
    #return(final)
    }
## end outcome heart attack
    if (outcome == "heart failure") {
        x <- vector("numeric",length=length(unique_States))
        for (i in 1:length(unique_States)) {
            ##check to make sure there is a hospital with the requested ranking
            noHospWithNumRank <- tryCatch(
                x[i] <- which(subState[[i]]["HF.Rank"]==num),
                error=function(e) e
            )
            if(!inherits(noHospWithNumRank, "error")) {
                ##returns 0 rows for this state if there is not a hospital with requested rating
            }
        }

        final <- data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
        for (i in 1: length(x)) {
            subState[[i]] <- subState[[i]][x[i],1:2]
            noHospWithNumRank <- tryCatch(
                final[i,] <- subState[[i]],
                error=function(e) e
            )
            if(!inherits(noHospWithNumRank, "error")) {
                ##returns 0 rows for this state if there is not a hospital with requested rating
            }
        }
    #return(subState)
    return(final)
    }
## end outcome heart failure
    if (outcome == "pneumonia") {
        x <- vector("numeric",length=length(unique_States))
        for (i in 1:length(unique_States)) {
            ##check to make sure there is a hospital with the requested ranking
            noHospWithNumRank <- tryCatch(
                x[i] <- which(subState[[i]]["PN.Rank"]==num),
                error=function(e) e
            )
            if(!inherits(noHospWithNumRank, "error")) {
                ##returns 0 rows for this state if there is not a hospital with requested rating
            }
        }
        final <- data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
        for (i in 1: length(x)) {
            subState[[i]] <- subState[[i]][x[i],1:2]
            noHospWithNumRank <- tryCatch(
                final[i,] <- subState[[i]],
                error=function(e) e
            )
            if(!inherits(noHospWithNumRank, "error")) {
                ##returns 0 rows for this state if there is not a hospital with requested rating
            }
        }
    return(final)
    }
## end outcome pneumonia
## End the ranking by outcome
    } else {
## End - Check to ensure outcome is correct (this is a large loop encapsulating most of the code)
        stop("invalid outcome")
    }
    #return(hs)
}


