#!/usr/bin/env Rscript

options(width=120)

# Finding the best hospital in a state

# Q2. 

# Provide two-letter abbreviated state name and outcome name.
best <- function(id=character(0), outcome_name=character(0))
{
    oc <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                  colClasses="character")

    # Check if 'id' and 'outcome_name' are valid identifiers
    states <- levels(factor(oc$State))
    if(!id %in% states)
    {
        stop("invalid state")
    }

    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome_name %in% valid_outcomes)
    {
        stop("invalid outcome")
    } 

    # Return character vector with the name of hopsital with lowest
    # mortality for specified outcome and state.
    # 1) Create a temporary data frame consisting of a subset with
    #    only the required state data.
    oc_st <- oc[oc$State == id, ]
    
    # 2) Identify columns reporting on mortality rates from
    #    hospital name, state, heart attack, heart failure and
    #    pneumonia (manually).
    mortality_rates <- c(2, 7, 11, 17, 23)

    # 3) Create temporary subset of 'oc_st' reduced to only
    #    include 'mortality_rates', type cast to numeric and
    #    exclude rows with 'NA' values.
    oc_st_mr <- oc_st[, mortality_rates]
    oc_st_mr[, 3] <- as.numeric(oc_st_mr[, 3])
    oc_st_mr[, 4] <- as.numeric(oc_st_mr[, 4])
    oc_st_mr[, 5] <- as.numeric(oc_st_mr[, 5])
    oc_st_mr <- oc_st_mr[complete.cases(oc_st_mr), ]
    colnames(oc_st_mr) <- c("Name", "State", "Heart Attack",
                            "Heart Failure", "Pneumonia")

    # 4) Return best hospital for specific outcome.
    #    '<<-' operater used to assign values in parent scope,
    #    these can then be accessed from 'rankhospital'.
    if(outcome_name == "heart attack")
    {
        oc_st_mr_ha <<- oc_st_mr[order(oc_st_mr[, 3], oc_st_mr[, 1]), ]
        solution <- as.character(oc_st_mr_ha[1,1])
    }
    if(outcome_name == "heart failure")
    {
        oc_st_mr_hf <<- oc_st_mr[order(oc_st_mr[, 4], oc_st_mr[, 1]), ]
        solution <- as.character(oc_st_mr_hf[1,1])
    }
    if(outcome_name == "pneumonia")
    {
        oc_st_mr_pn <<- oc_st_mr[order(oc_st_mr[, 5], oc_st_mr[, 1]), ]
        solution <- as.character(oc_st_mr_pn[1,1])
    }

    solution
}
