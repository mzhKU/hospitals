#!/usr/bin/env Rscript

source("best.R")
options(warn=-1)

# Q3.

# rankhospital("MD", "heart failure", 5)
# > <Name of hospital with 5th lowest 30-day mortality>
rankhospital <- function(           id=character(0),
                outcome_name=character(0),
                         num="best")
{
    best(id, outcome_name)
    if(num=="best")
    {
        num <- 1
    }
    if(outcome_name=="heart attack")
    {
        if(num=="worst")
        {
            num <- length(oc_st_mr_ha[, 1])
        }
        if(num>length(oc_st_mr_ha[, 1]))
        {
            return(NA)
        }
        sol <- oc_st_mr_ha[num, 1]
    }
    if(outcome_name=="heart failure")
    {
        if(num=="worst")
        {
            num <- length(oc_st_mr_hf[, 1])
        }
        if(num>length(oc_st_mr_hf[, 1]))
        {
            return(NA)
        }
        sol <- oc_st_mr_hf[num, 1]
    }
    if(outcome_name=="pneumonia")
    {
        if(num=="worst")
        {
            num <- length(oc_st_mr_pn[, 1])
        }
        if(num>length(oc_st_mr_pn[, 1]))
        {
            return(NA)
        }
        sol <- oc_st_mr_pn[num, 1]
    }
    sol
}
