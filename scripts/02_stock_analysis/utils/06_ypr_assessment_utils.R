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

# Catch-curve ----------

# Function to extract catch-curve data from the path to a YPR result object
get_cc_tab <- function(respath){
  # Reading catchcurve object
  res <- readRDS(respath)
  cc <- res$cc
  # Creating catchcurve dataframe
  ccdat <- tibble(
    'length' = cc$midLengths,
    'lnC_dt' = cc$lnC_dt,
    't_midL' = cc$t_midL,
    'Z' = round(cc$Z, 2),
    'Z_se' = round(cc$se, 1)
  )
  # adding a flag to indicate the regression interval
  regint <- rep(F, nrow(ccdat))
  regint[cc$reg_int[1]:cc$reg_int[2]] <- T
  ccdat$inclReg = regint
  # Returning
  ccdat
}

# Function to plot the catch-curve data for a given year, taking as an input the
# catch-curve table produced from the above function
plot_cc <- function(cctab){
  # Getting reference values as vectors
  ccInt <- cctab |> filter(inclReg)
  maxX <- max(cctab$t_midL)
  maxY <- max(cctab$lnC_dt, na.rm=T)
  Z <- cctab$Z[1]
  se <- cctab$Z_se[1]
  # Plotting
  ggplot() +
    geom_point(data=cctab, aes(x = t_midL, y=lnC_dt),
               size=2.5, shape=21) +
    geom_smooth(data=ccInt, aes(x = t_midL, y=lnC_dt),
                method='lm', se=F,
                colour='darkorange') +
    annotate('text', label=paste('Z =', Z, '+/-', se),
             x=3, y=7, hjust=1, 
             family='serif', size=3.5) +
    geom_point(data=ccInt, aes(x = t_midL, y=lnC_dt),
               size=2.5, shape=21,
               fill='darkorange',
    ) +
    lims(x=c(0, 3.5), 
         y=c(2, 8.5)) +
    qtheme(12, 'serif') +
    labs(
      y = 'Ln(C) / dt',
      x = 'Estimated age'
    )
}

# F-arrays ----------

# Function to extract selectivity and VPA F-arrays from a YPR object
get_f_array <- function(respath){
  # Reading objects
  res <- readRDS(respath)
  # Extraction gear-based selection curves
  fmtab <- res$seltable |> 
    rename('Length' = long_bin) |> 
    mutate(sel = cum_sel * res$mortalities$f) |> 
    # Joining VPA FM curves
    left_join(
      tibble('Length' = res$vpa$midLengths, 
             'vpa' = res$vpa$FM_calc)
    ) |> 
    dplyr::select('Length', 'Gear selectivity' = sel, 'VPA estimate' = vpa) |> 
    pivot_longer(
      cols=2:3,
      names_to = 'sel_type',
      values_to='val'
    )
  return(fmtab)
}

# Function to plot F-arrays against Frose reference points
plotFMCurves <- function(fmdat, froese_indics, ylim_max=3){
  # stopifnot(selgroupkw %in% c('Gear', 'VPA'))
  
  # For Gear-based selection curves, highlight those that were guessed
  fmdat |> 
    # filter(str_detect(sel_type, selgroupkw)) |>
    ggplot() +
    geom_line(aes(x = Length, y = val, colour=sel_type), linewidth=0.7) +
    scale_color_manual(values=c('orange', '#008080')) +
    labs(
      x = 'Length (cm)',
      y = 'Fishing Mortality',
      colour='F-Array Type'
    ) +
    coord_cartesian(ylim=c(0, ylim_max), 
                    xlim=c(
                      floor(min(fmdat$Length))-1, 
                      ceiling(froese_indics$LMspawn)+6
                    )) +
    # Length of maturity
    geom_vline(xintercept = froese_indics$Lmat, linetype='dashed') +
    annotate('text', label='L-Mat',
             x=froese_indics$Lmat, y=ylim_max, 
             hjust=-0.2,
             # hjust=1.2,
             size=2.5, family='serif') +
    # Optimal length range for maximum yield
    annotate('rect', 
             xmin = froese_indics$opt_range[1], xmax = froese_indics$opt_range[2], 
             ymin = -1, ymax = ylim_max+1,
             fill='darkgreen',
             alpha=0.2) +
    annotate('text', label='L-Mspawn',
             x=froese_indics$LMspawn, y=ylim_max, 
             hjust=-0.2,
             # hjust=1.2,
             size=2.5, family='serif') +
    # Length of mega-spawners
    geom_vline(xintercept = froese_indics$LMspawn, 
               linetype='dashed', colour='firebrick') +
    qtheme()
}
