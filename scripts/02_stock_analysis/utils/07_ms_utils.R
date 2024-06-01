# Author: Saeesh Mangwani
# Date: 2024-05-07

# Description: Helper functions for preparing analysis results

# ==== libraries ====
library(ggplot2)
library(purrr)

# ==== Paths and global variables ====

# Custom plotting theme
qtheme <- function(size=12, font='serif'){
  theme_minimal(size, font) +
    theme(plot.background = element_rect(fill='white', colour=NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
}

# Function to calculate shannon diversity given a vector of species names
calc_spec_index <- function(specvec, wt = NULL, index='div'){
  # print(specvec)
  stopifnot(index %in% c('rich', 'div', 'eq'))
  # Count of each unique species
  uspecs <- table(specvec)
  # Number of unique = species richness
  nunq <- length(uspecs)
  # Returning
  if(index == 'rich'){
    return(nunq)
  }
  # If there is a weight variable, replacing counts with caught weight
  if(!is.null(wt)){
    wttab <- data.frame('species' = specvec, 'wt' = wt) |> 
      group_by(species) |> 
      summarize(wt = sum(wt))
    # Getting proportion of species by weight instead of counts
    wtspecs <- setNames(wttab$wt, wttab$species)
    pspecs <- wtspecs/sum(wtspecs)
  }else{
    # Proportion for each species by count
    pspecs <- uspecs/length(specvec)
  }
  # Shannon diversity index
  shannon <- -1*sum(pspecs * log(pspecs))
  if(index=='eq'){
    # Shannon-max for equitability index
    even_pspecs <- rep(1/length(pspecs), length(pspecs))
    hmax <- -1*sum(even_pspecs * log(even_pspecs))
    # Equitability index if asked
    return(shannon/hmax)
  }
  return(shannon)
}

# Function that given a log base and length-range, divides that length range
# into log-width bins of length
calc_log_bins <- function(lmin, lmax, logBase){
  stopifnot(logBase >= 1)
  # Using the base, creating a large sequence of its exponent values (enough to
  # exceed the length range)
  numVals <- lmax/logBase
  binBreaks <- round(logBase^seq(1:numVals), 2)
  # Removing breaks which fall outside the range of the data
  keepidx <- which((binBreaks >= lmin) & (binBreaks <= lmax))
  # Adding one at the tail to capture the last group which gets removed
  keepidx[length(keepidx)+1] <- (tail(keepidx,1)+1)
  # Picking only the needed breaks, i.e the ones that cover the length range
  binBreaks <- binBreaks[keepidx]
  # Calculating bin-widths (size 1 less than the number of breaks)
  binWidths <- discard(binBreaks - lag(binBreaks), is.na)
  # Setting bin labels as the mid-points between each bin
  binLabs <- round(discard(binBreaks + (c(binWidths, NA)/2), is.na), 1)
  # Saving all in an output list
  outList <- list(
    'breaks' = binBreaks,
    'labels' = binLabs,
    'widths' = setNames(binWidths, binLabs)
  )
}
