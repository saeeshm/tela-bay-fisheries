# Author: Saeesh Mangwani
# Date: 2024-05-10

# Description: Helper functions for mesh-size selectivity analysis

# ==== libraries ====
library(stringr)

# ==== Functions ====

# A function that estimates the proportional selectivity for a given input
# length and mesh size, taking a selectivity model as an input and an optional
# proportional indication of relative power. It calls the relevant equation
# construction function based on the passed model type 
get_sel_func <- function(smdl){
  # What is the fit type?
  ftype <- str_squish(smdl$fit.type)
  if(str_detect(ftype, 'lognorm')){
    return(est_selectivity_lognorm)
  }else if(str_detect(ftype, 'norm.sca')){
    return(est_selectivity_normsca)
  }else{
    print(paste("Error with model: ", ftype, '. This model type is not supported!'))
    return()
  }
}

# Estimating proportional selectivity for a normal-proportional-variance
# selectivity model
est_selectivity_normsca <- function(length, meshsize, smdl, rel_power=NULL, ...){
  v1 <- (length - smdl$gear.pars['k1',][1]*meshsize)^2
  v2 <- (2*smdl$gear.pars['k2',][1] * (meshsize^2))
  res <- exp(-(v1/v2))
  if(!is.null(rel_power)){
    res <- res*rel_power[as.character(meshsize)]
  }
  return(res)
}

# Estimating proportional selectivity for a lognormal-proportional-variance
# selectivity model
est_selectivity_lognorm <- function(length, meshsize, mesh1=2, smdl, rel_power=NULL){
  # Getting model estimates
  mu1 <- smdl$gear.pars[1,][1]
  sigma <- smdl$gear.pars[2,][1]
  # Calculating equation components
  p1 <- log(meshsize/mesh1)
  p2 <- (sigma^2)/2
  p3_num <- (log(length) - mu1 - p1)^2
  p3_denom <- 2*(sigma^2)
  # Final result
  res <- (1/length)*exp((mu1 + p1 - p2 - (p3_num/p3_denom)))
  if(!is.null(rel_power)){
    res <- res*rel_power[as.character(meshsize)]
  }
  return(res)
}
