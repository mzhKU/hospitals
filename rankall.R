#!/usr/bin/env Rscript


options(warn=-1,width=98)


# **************************************************************
# Q4 DESCRIPTION
# --------------------------------------------------------------
# Note: If the number of hospitals in a state
#       is lower than 'num', 'NA' should be returned.
# --------------------------------------------------------------



# **************************************************************
# IMPLEMENTATION
# --------------------------------------------------------------

# Mortality rates
mr <- NULL

get_hospitals <- function(splitted_list, rank)
{
    hospitals <- NULL
    for(i in splitted_list)
    {
        # Get hospital with 'rank' in 'i' block of splitted_list.
        # i[[1]] gives the hospital names of the block.
        # Include only state id also in case rank is beyond number
        # of hospitals.
        i <- i[complete.cases(i), ]
        if(rank=="wo" | rank=="worst")
        {
            worst_hospital <- length(i[[1]])
            tmp <- cbind(  i[[1]][worst_hospital],
                           i[[2]][worst_hospital]  )
            hospitals <- rbind(hospitals, tmp)
        }
        # Prevent implicit type cast of rank.
        else if(rank > length(i[[1]]) & class(rank)=="numeric")
        {
            tmp <- cbind(NA, i[[2]][1])
            hospitals <- rbind(hospitals, tmp)
        } else {
            tmp <- cbind(i[[1]][rank], i[[2]][rank]) 
            hospitals <- rbind(hospitals, tmp)
        }
    }
    colnames(hospitals) <- c("hospital", "state")
    hospitals
}

rankall <- function(outcome_name, num="best")
{
    oc <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                  colClasses="character")

    #states <<- levels(factor(oc$State))
    #valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    #if(!outcome_name %in% valid_outcomes)
    #{
    #    stop("invalid outcome")
    #} 

    # 1) Identify columns reporting on mortality rates from
    #    hospital name, state, heart attack, heart failure and
    #    pneumonia (manually).
    mortality_rates <- c(2, 7, 11, 17, 23)

    # 2) Create temporary subset of 'oc_st' reduced to only
    #    include 'mortality_rates', type cast to numeric.
    mr <<- oc[, mortality_rates]
    mr[, 3] <<- as.numeric(mr[, 3])
    mr[, 4] <<- as.numeric(mr[, 4])
    mr[, 5] <<- as.numeric(mr[, 5])
    #mr <<- mr[complete.cases(mr), ]
    colnames(mr) <<- c("hospital", "state", "ha", "hf", "pn")

    # Reassign default and 'best' value, 'worst' is
    # assigned in 'get_hospitals'.
    if(num=="best")
    {
        num <- 1
    }

    # Select outcomes.
    if(outcome_name=="ha" | outcome_name=="heart attack")
    {
        mr <<- mr[, c(1,2,3)]
    }
    if(outcome_name=="hf" | outcome_name=="heart failure")
    {
        mr <<- mr[, c(1,2,4)]
    }
    if(outcome_name=="pn" | outcome_name=="pneumonia")
    {
        mr <<- mr[, c(1,2,5)]
    }

    # Order by State > Mortality Rate > Hospital Name,
    # generate a list of hospitals splitted by state.
    mr <<- mr[order(mr[,2], mr[, 3], mr[, 1]), ]
    sl <- split(mr, factor(mr[, 2]))
    sol <- get_hospitals(sl, num)
    return(data.frame(sol))
}
# --------------------------------------------------------------




# **************************************************************
# CODE NOTES
# --------------------------------------------------------------
#
# Calculate the mean of mortality rate per state for
# pneumonia (mr[, 5]):
# > tapply(mr[, 5], factor(mr[, 2]), mean)
#
# Split into groups of states
# > split(mr, factor(mr[, 2]))
#
# k <- split(mr, factor(mr[, 2]))
# tapply(mr[, 5], factor(mr[, 2]), get_ranked_state,
#                                  split(mr, factor(mr[, 2])),
#                                  rank)
#
# Access specific elements from 'k'-block
# k
# k$WY
# k$WY[[2]]
# k$WY[[2]][1]
# for(i in seq_along(k))
# {
#     if(k==special_index)
#     {
#          print(k[[i]])
#     }
# }
#
# --------------------------------------------------------------
