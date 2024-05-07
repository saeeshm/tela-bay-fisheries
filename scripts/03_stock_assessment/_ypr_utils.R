# Author: Saeesh Mangwani
# Date: 2024-04-13

# Description: Helper functions for Yield-per-recruit analysis

# ==== libraries ====
library(TropFishR)

# ==== Functions ====

# Given a vector of lengths, calculated relative age and age-difference (delta-t)
calc_delta_t <- function(lengths, params, return_age=F){
  rel_age <- TropFishR::VBGF(param=params, L=lengths, na.rm=T)
  delta_t <- lead(rel_age)-rel_age
  return(delta_t)
}

# Given a vector of survival probabilities per-length class, calculates the
# number of individuals (assuming a default initial recruitment number of 1000)
calc_num_cohort <- function(survivals, num_recruits=1000){
  # Initial list
  nums <- vector(mode='numeric', length=length(survivals))
  nums[1] <- num_recruits
  # Modifying using a lag
  for(i in 2:length(nums)){
    nums[i] <- nums[i-1]*survivals[i-1]
  }
  return(nums)
}

# Given a vector of length intervals, and input model coefficients calculates
# the average weight of individuals in each length interval (uses defaults for a
# and b if none are present)
calc_mean_weight <- function(lengths, a=0.01, b=3){
  p1 <- 1/(lead(lengths) - lengths)
  p2 <- a*(b+1)
  p3 <- (lead(lengths)^(b+1)) - (lengths^(b+1))
  mwt <- p1*p2*p3
  return(mwt)
}
