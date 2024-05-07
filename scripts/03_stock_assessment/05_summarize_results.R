# Author: Saeesh Mangwani
# Date: 2024-04-14

# Description: Summary and assessment of stock status per-year based on
# length-frequency data. This script summarizes the YPR outputs produced in the
# previous script, as well as calculates Froese's indicators for fishery health
# for each year

# ==== libraries ====
library(readr)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(ggnewscale)

# ==== Paths and global variables ====

# Full fisheries database
fdb_path <- 'data/stock_assessment/lane_snapper_total.csv'

# Path to selectivity and growth parameter object
selgrowth_path <- 'data/stock_assessment/growth_selectivity.rds'

# Selectivity corrected YPR results
selcor_dir <- 'output/stock_assessment/ypr/annual_tables_selcor'

# No-selectivity corrected YPR results
noselcor_dir <- 'output/stock_assessment/ypr/annual_tables_noselcor'

# Spict results ptah
spict_path <- 'output/stock_assessment/spict/best_spict.rds'

# Output directory
out_dir <- 'output/stock_assessment/pres_plots'

# Custom ggplot theme
mytheme <- function(size=12, font='serif'){
  theme_minimal(size, font) +
    theme(
      plot.background=element_rect(fill='white', colour=NULL, linewidth = 0),
      panel.background = element_rect(fill='white', colour=NULL),
      panel.border = element_blank()
    ) 
}
  
# ==== Reading data ====
fdb_og <- read_csv(fdb_path)
sg <- readRDS(selgrowth_path)

# YPR results - selectivity corrected
fpaths <- list.files(selcor_dir, full.names = T)
fnames <- list.files(selcor_dir) |> str_extract('\\d+')
scor_paths <- setNames(fpaths, fnames)

# YPR results - no-selectivity correction applied
fpaths <- list.files(noselcor_dir, full.names = T)
fnames <- list.files(noselcor_dir) |> str_extract('\\d+')
nscor_paths <- setNames(fpaths, fnames)

# Spict results
spict <- readRDS(spict_path)

# ==== Preliminary tidying ====
fdb <- fdb_og |> 
  # Keeping only data for Tela
  filter(region == 'Tela') |>
  # Converting month to factor
  mutate(month = factor(month, levels=month.abb)) |> 
  # Removing rows with missing length information
  filter(!is.na(longitud)) |> 
  # Removing outlier lengths
  mutate(isanom = anomalize::iqr(longitud)) |>
  filter(isanom == 'No') |> 
  # Removing outlier mesh size - has only 1 data point
  filter(is.na(luz_malla) | luz_malla < 6) |>
  # Fixing precision of total length and mesh size
  mutate(longitud = round(longitud, 2)) |> 
  mutate(luz_malla = round(luz_malla, 1)) |> 
  # Keeping only relevant columns
  dplyr::select(year, month, fecha, longitud, peso, horas_pesca, luz_malla)

# Splitting full-dataset by year
yds <- fdb |> 
  group_by(year) |> 
  group_split()
names(yds) <- unique(fdb$year)

# ==== Basic descriptives plot (time-trend) ====
ts_plot <- fdb |> 
  # Getting the isoweek for each year
  mutate(isoweek = isoweek(fecha), .after='fecha') |> 
  # Converting isoweek to a single date using the first date of each isoweek
  mutate(fecha = as.Date(paste(year, isoweek, 1, sep = "-"), "%Y-%W-%w"),
         .after='isoweek') |>
  # Aggregating by week for visualizing the trends
  # group_by(year, month, isoweek) |>
  group_by(fecha) |>
  # rowwise() |> 
  summarize(
    'Length (cm)'=mean(longitud, na.rm=T),
    'Weight (grams)'=mean(peso, na.rm=T),
  'Effort (hours)'=mean(horas_pesca, na.rm=T)
  ) |>
  # Removing outliers for hours fished
  # filter(`Effort (hours)` < 15) |>
  # Calculating CPUE
  mutate(`CPUE (gram/hour)` = `Weight (grams)`/`Effort (hours)`) |> 
  tidyr::pivot_longer(`Length (cm)`:`CPUE (gram/hour)`, names_to='var', values_to = 'val') |> 
  mutate(var = factor(var, levels=c('Weight (grams)', 'Effort (hours)', 'CPUE (gram/hour)', 'Length (cm)'))) |> 
  ggplot(aes(x=fecha, y=val)) +
  geom_smooth(colour='grey', alpha=0.3, method='loess', span=0.5) +
  geom_line(colour='black', alpha=0.4) +
  geom_point(aes(colour=var), alpha=0.6, show.legend = F) +
  scale_colour_manual(values=c('firebrick', 'orange', 'darkgreen', 'darkblue')) +
  facet_wrap('var', scales='free', nrow=4) +
  mytheme() +
  labs(
    x = NULL,
    y=NULL,
  )
ts_plot

# ==== Froese-indicators plots ====

# Calculating standard reference points for index-based assessment (Froese and
# Binohlan 2000, Froese 2004) - uses the full dataset

# Comparing empirical Linf to estimated based on the VBGF
Linf_emp <- exp(0.044 + 0.9841*log(max(fdb$longitud)))
Linf_emp
sg$elefan_sa_result$par$Linf
curr_linf <- sg$elefan_sa_result$par$Linf
curr_linf <- Linf_emp
# Calculating Length-at-first-maturity, length-at-optimum-yield and therefore
# the length range of optimal catch, and length of megaspawners
Lmat <- exp((0.8979 * log(curr_linf)) - 0.0782)
Lopt <- exp((1.0421 * log(curr_linf)) - 0.2742)
opt_range <- c(Lopt - Lopt*0.1, Lopt + Lopt*0.1)
mspawn_length <- Lopt+Lopt*0.1

# For each year, calculating metrics
froese_indics <- imap_dfr(yds, \(cdb, cyear){
  tibble(
    'year' = cyear,
    # Percentage of catch that is mature
    'perc_mature' = mean((cdb$longitud >= Lmat)),
    # Percentage of catch in the optimal yield range
    'perc_optimal' = mean(between(cdb$longitud, opt_range[1], opt_range[2])),
    # Percentage of mega-spawners in the catch
    'perc_mspawn' = mean((cdb$longitud >= mspawn_length))
  )
})

# Plot summarizing results from the Froese indicators ----------
froese_plot <- froese_indics |> 
  tidyr::pivot_longer(cols=2:4, names_to='indic', values_to='val') |> 
  mutate(val=val*100) |> 
  mutate(indic = case_when(
    indic == 'perc_mature' ~ '% Mature',
    indic == 'perc_optimal' ~ '% Optimal',
    indic == 'perc_mspawn' ~ '% Megaspawner',
  )) |> 
  mutate(indic = factor(indic, levels=c('% Mature','% Optimal', '% Megaspawner'))) |> 
  ggplot(aes(x = year, y = val, colour=indic)) +
  geom_point() +
  geom_line(aes(group=indic), alpha=1) +
  scale_colour_manual(values=c('#885A89','#8AA8A1', '#FADF7F')) +
  # scale_colour_brewer(palette='Set2')+
  mytheme() +
  ylim(0, 100) +
  labs(
    x = NULL, 
    y='% of Catch',
    colour = 'Indicator'
  )
froese_plot

# ==== Summary plots for YPR-curves per year ====

# Function to prepare the summary dataframes
prep_summ_dfs <- function(pathlist){
  # Full-plotting dataframe for standard YPR line-chart
  full_df <- imap_dfr(pathlist, \(path, cyear){
    # Reading objects
    res <- readRDS(path)
    res$ypr |> 
      dplyr::select(scenario, FM_scaling, yield, biomass) |> 
      mutate(year = cyear, .before='scenario')
  })
  
  # Dataframe containing reference points for each year and scenario
  ref_pts <- imap_dfr(pathlist, \(path, cyear){
    # Reading objects
    res <- readRDS(path)
    # Calculating extra parameters - proportion of biomass relative to unfished
    # biomass and U-function
    res$ypr |> 
      group_by(scenario) |> 
      group_modify(\(scntab, y){
        # The overall fishing mortality for this scenario
        curr_m <- ifelse(str_detect(y[[1]], 'pauly'), res$m_pauly, res$m_then)
        overall_fm <- res$catchcurve$Z - curr_m
        # Unfished-biomass estimate
        b0 <- scntab[1,]$biomass
        # Calculated additional metrics
        scntab <- scntab |> 
          # % unfished-biomass
          mutate(perc_unfished_bms = biomass/b0) |> 
          # U-function (number of recruits = 1000) |> 
          mutate(u_01 = 0.1*(b0/res$num_recruits)*FM_scaling) |> 
          mutate(u_05 = 0.5*(b0/res$num_recruits)*FM_scaling)
        # Extracting only rows which are the reference points
        curr_f <- scntab[which.min(abs(scntab$FM_scaling - overall_fm)), ]
        fmax <- scntab[which.max(scntab$yield), ]
        f_01 <- scntab[which.max(scntab$yield - scntab$u_01), ]
        # Returning the reference points table
        bind_rows(curr_f, fmax, f_01) |> 
          mutate('type' = factor(c('F-current', 'F-MSY', 'F-0.1'), 
                                 levels=c('F-current', 'F-MSY', 'F-0.1')),
                 .before='FM_scaling')
      }) |> 
      ungroup() |> 
      mutate(year = cyear, .before='scenario')
  })
  list(
    'full' = full_df,
    'ref' = ref_pts
  )
}

# Getting dataframes for selectivity corrected and uncorrected data
scor <- prep_summ_dfs(pathlist = scor_paths)
nscor <- prep_summ_dfs(pathlist = nscor_paths)

# ==== Visualizing ====

# Standard YPR curves for each year showing all scenarios ----------

# Helper function for plotting the standard YPR curves functions for a single
# year
yprplot <- function(full_df, ref_pts, cyear, var){
  stopifnot(var %in% c('yield', 'biomass'))
  label <- ifelse(var == 'yield', 'Yield/Recruit', 'Biomass/Recruit')
  # Getting the reference points for only this year
  refdf <- ref_pts |> 
    filter(year %in% c(cyear)) |> 
    mutate('F-Level' = type)
  
  # Subsetting the data from the full dataframe for this year and plotting
  p <- full_df |> 
    filter(year %in% c(cyear)) |>
    mutate(Scenario = case_when(
      scenario == 'gear_sel_pauly' ~ 'S.1',
      scenario == 'gear_sel_then' ~ 'S.2',
      scenario == 'vpa_sel_pauly' ~ 'S.3',
      scenario == 'vpa_sel_then' ~ 'S.4',
    )) |> 
    ggplot() +
    geom_line(aes(x = FM_scaling, y=!!sym(var), colour=Scenario, linetype=Scenario),
              alpha=0.8, linewidth=0.5) +
    scale_linetype_manual(values=c('solid', 'solid', 'dashed', 'dashed')) +
    scale_colour_manual(values=c('magenta4', 'skyblue2', 'magenta4', 'skyblue2')) +
    ggnewscale::new_scale_color() +
    geom_point(data=refdf, 
               aes(x = FM_scaling, y=!!sym(var), shape=`F-Level`, colour=`F-Level`),
               size=2,
               alpha=0.8) +
    scale_colour_manual(values=c('firebrick', 'orange', 'darkgreen')) +
    mytheme(12, 'serif') +
    labs(
      subtitle = ifelse(var=='yield', cyear, list(NULL))[[1]],
      x = NULL,
      y = ifelse(cyear %in% c('2016', '2020'), label, list(NULL))[[1]],
    )
  # Fixing y-axis limits
  if(var == 'yield'){
    p <- p + ylim(c(0, 26000))
  }else{
    p <- p + ylim(c(0, 200000))
  }
  return(p)
}

# Function to generate a mosaic of YPR curves
plot_ypr_mosaic <- function(full_df, ref_pts){
  # Using the same years for both
  years <- setNames(names(scor_paths), names(scor_paths))
  # Building base plots per year
  base_plots <- map(years, \(cyear){
    y <- yprplot(full_df, ref_pts, cyear, 'yield')
    b <- yprplot(full_df, ref_pts, cyear, 'biomass')
    list(y, b)
  })
  base_plots <- list_flatten(base_plots)
  
  # Aggregating into a full mosaic for each year
  layout <- '
  ACEG
  BDFH
  IKMO
  JLNP'
  wrap_plots(base_plots, nrow=2, ncol=4, design = layout, guides = 'collect') +
    plot_layout(axes='collect')
}

scor_mosaic <- plot_ypr_mosaic(scor$full, scor$ref)
nscor_mosaic <- plot_ypr_mosaic(nscor$full, nscor$ref)

# Catch-curves for each year ----------

# Function to extract catch-curve data for each year
get_cc_tab <- function(respath){
  # Reading catchcurve object
  res <- readRDS(respath)
  cc <- res$catchcurve
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
# Plotting
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
             x=2.5, y=7, hjust=1, 
             family='serif', size=3.5) +
    geom_point(data=ccInt, aes(x = t_midL, y=lnC_dt),
               size=2.5, shape=21,
               fill='darkorange',
    ) +
    lims(x=c(0, 2.5), 
         y=c(0.5, 7.5)) +
    mytheme(12, 'serif') +
    labs(
      y = 'Ln(C) / dt',
      x = 'Estimated age'
    )
}


# Producing catch-curve mosaics for selectivity corrected and uncorrected
# datasets
ccMosaic <- function(pathlist){
  ccdats <- map(pathlist, get_cc_tab)
  ccplots <- imap(ccdats, \(cctab, cyear){
    plot_cc(cctab) + labs(subtitle=cyear)
  })
  # Producing the mosaic
  wrap_plots(ccplots, nrow=4, guides = 'collect') + plot_layout(axes='collect')
}
scor_cc <- ccMosaic(scor_paths)
nscor_cc <- ccMosaic(nscor_paths)

# Raw and corrected length frequencies for each year ----------

# Function to extract and format the plots for mosaicing
getLfqPlots <- function(pathlist){
  imap(pathlist, \(path, cyear){
    # Reading objects
    res <- readRDS(path)
    # Returning only the catch-distribution plot
    res$corrcatchplot +
      xlim(c(floor(min(fdb$longitud))-1, ceiling(max(fdb$longitud))+1)) +
      labs(subtitle=cyear)
  })
}
# Creating the mosaic - only need the length-frequency corrected tables here
selLfq <- getLfqPlots(scor_paths)
lfqDists <- wrap_plots(selLfq, nrow=4, ncol=2, guides = 'collect') +
  plot_layout(axes='collect')

# Visualizing FM curves for each year ----------

# Extraction FM data for all scenarios and years
prep_fm_df <- function(pathlist){
  # Full-plotting dataframe for standard YPR line-chart
  imap_dfr(pathlist, \(path, cyear){
    # Reading objects
    res <- readRDS(path)
    # Extracting relevant input parameters
    overall_z <- res$catchcurve$Z
    m_pauly <- as.numeric(res$m_pauly)
    m_then <- as.numeric(res$m_then)
    # Extraction all selection curves
    fmtab <- res$seltable |> 
      mutate(`Theoretical Gear-based (Pauly M-estimate)` = cum_sel * (overall_z-m_pauly)) |> 
      mutate(`Theoretical Gear-based (Then M-estimate)` = cum_sel * (overall_z-m_then))
    # Joining VPA FM curves
    fmtab <- fmtab |> 
      left_join(
        tibble('Length' = res$vpa_pauly$midLengths, 
               'VPA-based (Pauly M-estimate)' = res$vpa_pauly$FM_calc)
      ) |> 
      left_join(
        tibble('Length' = res$vpa_then$midLengths, 
               'VPA-based (Then M-estimate)' = res$vpa_then$FM_calc)
      ) |> 
      pivot_longer(
        cols=2:8,
        names_to = 'sel_type',
        values_to='val'
      ) |> 
      mutate(year = cyear) |> 
      mutate(selGuessed = year %in% 2017:2019)
  })
}
scor_fm <- prep_fm_df(scor_paths)
nscor_fm <- prep_fm_df(nscor_paths)

# Plotting
plotFMCurves <- function(fmdat, selgroupkw, ylim_max=7.5){
  stopifnot(selgroupkw %in% c('Gear', 'VPA'))
  # For Gear-based selection curves, highlight those that were guessed
  if(selgroupkw == 'Gear'){
    baseplot <- fmdat |> 
      mutate(selGuessed = ifelse(selGuessed, 'Guessed', 'Observed')) |> 
      filter(str_detect(sel_type, selgroupkw)) |> 
      mutate(year = as.factor(year)) |> 
      ggplot() +
      # geom_point(size=0.6) +
      geom_line(aes(x = Length, y = val, colour=year, linetype=selGuessed), linewidth=0.7) +
      scale_colour_brewer(palette='PuOr') +
      scale_linetype_manual(values=c('dotted', 'solid')) +
      labs(
        x = NULL,
        y = 'FM',
        colour='Year',
        linetype='Selectivity',
      ) 
  # Otherwise no linetype aesthetic
  }else{
    baseplot <- fmdat |> 
      filter(str_detect(sel_type, selgroupkw)) |> 
      mutate(year = as.factor(year)) |> 
      ggplot() +
      # geom_point(size=0.6) +
      geom_line(aes(x = Length, y = val, colour=year), linewidth=0.7) +
      scale_colour_brewer(palette='PuOr') +
      labs(
        x = NULL,
        y = 'FM',
        colour='Year'
      )
  }
  # Adding aesthetics for the final plot
  baseplot + 
    coord_cartesian(ylim=c(0, ylim_max), 
                    xlim=c(floor(min(fdb$longitud))-1, ceiling(mspawn_length)+6)) +
    # Length of maturity
    geom_vline(xintercept = Lmat, linetype='dashed') +
    annotate('text', label='L-Mat',
             x=Lmat, y=ylim_max, 
             # hjust=-0.2,
             hjust=1.2,
             size=2.5, family='serif') +
    # Optimal length range for maximum yield
    annotate('rect', 
             xmin = opt_range[1], xmax = opt_range[2], 
             ymin = -1, ymax = ylim_max+1,
             fill='darkgreen',
             alpha=0.2) +
    annotate('text', label='L-Mspawn',
             x=mspawn_length, y=ylim_max, 
             # hjust=-0.2,
             hjust=1.2,
             size=2.5, family='serif') +
    # Length of mega-spawners
    geom_vline(xintercept = mspawn_length, 
               linetype='dashed', colour='firebrick') +
    mytheme() +
    labs(
      x = NULL,
      y = 'FM',
      colour='Year',
      linetype='Selectivity',
    ) +
    facet_wrap(facet='sel_type', nrow=1, scales='fixed')
}
# Creating the mosaic
gear <- plotFMCurves(scor_fm, 'Gear')
vpa <- plotFMCurves(scor_fm, 'VPA')
selCurves <- (gear/vpa) + plot_layout(guides='collect')
selCurves
# Combining the mosaic with the froese indicators
selFroese <- (selCurves/froese_plot)
selFroese

# ==== Comparative summary plots for F/FMSY and B/BMSY ====

# YPR: Distribution of F-MSY and F-0.1 relative to current F ----------
plot_ref_dist <- function(refdf){
  refdf |> 
    group_by(year, type) |> 
    summarize(
      'mean_fm' = mean(FM_scaling),
      'sd_fm' = sd(FM_scaling),
      'n_fm' = n()
    ) |> 
    mutate(
      sd_up = mean_fm + sd_fm,
      sd_down = mean_fm - sd_fm,
      se_up = mean_fm + (sd_fm/n_fm),
      se_down = mean_fm - (sd_fm/n_fm)
    ) |> 
    ggplot(aes(x = year, y = mean_fm, colour=type)) +
    geom_point(size=1) +
    geom_line(aes(group=type)) +
    geom_errorbar(aes(ymin = se_down, ymax=se_up), width=0.1) +
    scale_colour_manual(values=c('firebrick', 'orange', 'darkgreen')) +
    mytheme() +
    labs(
      x = NULL,
      y = 'Fishing mortality (^year-1)',
      colour = 'F-Level'
    )
}

# For each dataset, and then combined
scor_refdist <- plot_ref_dist(scor$ref)
nscor_refdist <- plot_ref_dist(nscor$ref)
comb_refdist <- plot_ref_dist(bind_rows(nscor$ref, scor$ref))

# YPR v/s SPICT comparison ----------

# Calculating F/Fmsy for YPR
ypr_ftab <- scor$ref |> 
  filter(scenario=='vpa_sel_pauly') |> 
  select(year, type, 'f' = FM_scaling) |> 
  pivot_wider(names_from = 'type', values_from = 'f') |> 
  setNames(c('year', 'f', 'fmsy', 'f01')) |> 
  mutate(`YPR F/F-MSY` = f/fmsy,
         `YPR F/F-0.1` = f/f01) |> 
  mutate(time = as.numeric(year)) |> 
  select(time, `YPR F/F-MSY`, `YPR F/F-0.1`) |> 
  pivot_longer(2:3, names_to='type', values_to='est')

# Plotting dataframe
plotdf <- spict$ftab |>
  mutate(time = as.numeric(time)) |> 
  mutate(type = 'Spict F/F-MSY') |> 
  filter(time < 2023.25) |> 
  bind_rows(ypr_ftab)

# Plotting
fmsy_comp_plot <- ggplot() +
  geom_line(data=plotdf, aes(x = time, y=est, colour=type)) +
  geom_point(data= plotdf |> dplyr::filter(type != 'Spict F/F-MSY'),
             aes(x = time, y=est, colour=type)) +
  geom_ribbon(data= plotdf |> dplyr::filter(type == 'Spict F/F-MSY'),
              aes(x = time, y=est, ymax=ul, ymin=ll), alpha=0.2) +
  scale_color_manual(values=c('darkgreen', 'orange', 'firebrick')) +
  # Sustainabile reference point
  geom_hline(yintercept = 1, linetype='dashed') +
  mytheme() +
  labs(
    x = NULL,
    y = 'F/F-MSY',
    colour = 'Source'
  )
fmsy_comp_plot

# ==== Saving all plots ====

# Descriptives time-series plot
ggsave(filename=file.path(out_dir, 'ts_desc_plot.png'), plot = ts_plot,
       width=12, height=8, unit='in')
# YPR mosaics
ggsave(filename=file.path(out_dir, 'ypr_mosaic_sel_cor.png'), plot = scor_mosaic,
       width=12, height=8, unit='in')
ggsave(filename=file.path(out_dir, 'ypr_mosaic_not_cor.png'), plot = nscor_mosaic,
       width=12, height=8, unit='in')
# Catch-curve regression mosaics
ggsave(filename=file.path(out_dir, 'cc_mosaic_sel_cor.png'), plot = scor_cc,
       width=10, height=8, unit='in')
ggsave(filename=file.path(out_dir, 'cc_mosaic_not_cor.png'), plot = nscor_cc,
       width=10, height=8, unit='in')
# YPR reference point distributions
ggsave(filename=file.path(out_dir, 'ypr_ref_pts_sel_cor.png'), plot = scor_refdist,
       width=8, height=6, unit='in')
ggsave(filename=file.path(out_dir, 'ypr_ref_pts_not_cor.png'), plot = nscor_refdist,
       width=8, height=6, unit='in')
ggsave(filename=file.path(out_dir, 'ypr_ref_pts_combined.png'), plot = comb_refdist,
       width=8, height=6, unit='in')
# YPR v/s SPiCT F/Fmsy
ggsave(filename=file.path(out_dir, 'spict_ypr_fmsy_comp.png'), plot = fmsy_comp_plot,
       width=8, height=6, unit='in')
# Froese indicator plot
ggsave(filename=file.path(out_dir, 'froese_indicators.png'), plot = froese_plot,
       width=8, height=6, unit='in')
# Length-frequency distributions per year
ggsave(filename=file.path(out_dir, 'lfq_dists_annual.png'), plot = lfqDists,
       width=7, height=7, unit='in', scale = 1.2)
# Fishing mortality curves mosaic per year
ggsave(filename=file.path(out_dir, 'fm_curves.png'), plot = selCurves,
       width=8, height=6, unit='in', scale = 1.2)
# Fishing mortality mosaic combined with the Froese indicator
ggsave(filename=file.path(out_dir, 'fm_curves_w_froese.png'), plot = selFroese,
       width=7, height=7, unit='in', scale = 1.2)