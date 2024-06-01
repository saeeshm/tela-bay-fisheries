# Author: Saeesh Mangwani
# Date: 2024-05-14

# Description: Helper functions for summarizing YPR results

# ==== Libraries ====
library(tidyr)
library(ggplot2)
library(patchwork)

# ==== Functions ====

# Froese indicators ----------

# Calculates all relevant froese indicators given an estimate of Linf, using
# empirical equations following Froese 2004 and Frose and Binholan 2000
calc_froese_indics <- function(Linf, Lmax=NULL){
  # Length of maturity
  Lmat <- exp((0.8979 * log(Linf)) - 0.0782)
  # Optimal length range 
  Lopt <- exp((1.0421 * log(Linf)) - 0.2742)
  opt_range <- c(Lopt - Lopt*0.1, Lopt + Lopt*0.1)
  # Megaspawner length
  LMspawn <- Lopt+Lopt*0.1
  return(list(
    'Lmat' = Lmat,
    'opt_range' = opt_range,
    'LMspawn' = LMspawn
  ))
}
