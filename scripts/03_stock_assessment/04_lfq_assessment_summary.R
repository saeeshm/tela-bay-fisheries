# Author: Saeesh Mangwani
# Date: 2024-04-14

# Description: Summary and assessment of stock status per-year based on
# length-frequency data. This script summarizes the YPR outputs produced in the
# previous script, as well as calculates Froese's indicators for fishery health
# for each year

# ==== libraries ====
library(readr)
library(stringr)
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

# Output directory
out_dir <- 'output/stock_assessment/ypr'

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

# Preliminary tidying
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

# ==== Catch-classification based on Froese indicators ====

# Calculating standard reference points for index-based assessment (Froese and
# Binohlan 2000, Froese 2004) - uses the full dataset

# Comparing empirical Linf to estimated based on the VBGF
Linf_emp <- exp(0.044 + 0.9841*log(max(fdb$longitud)))
Linf_emp
sg$elefan_sa_result$par$Linf
curr_linf <- sg$elefan_sa_result$par$Linf
# curr_linf <- Linf_emp
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

# ==== Summarizing YPR results per year ====

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
  # Removing axies ticks where unnecessary to make the final mosaic cleaner
  # if((var == 'yield') | (cyear %in% 2016:2018)) {
  #   p <- p + theme(axis.text.x = element_blank())
  # }
  # if(!(cyear %in% c(2016, 2020))){
  #   p <- p + theme(axis.text.y = element_blank())
  # }
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

# Distribution of reference points relative to current ----------
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

# Plot visualizing raw and corrected length frequencies for each year ----------

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

# ==== Saving all plots ====

# YPR mosaics
ggsave(filename=file.path(out_dir, 'ypr_mosaic_sel_cor.png'), plot = scor_mosaic,
       width=12, height=8, unit='in')
ggsave(filename=file.path(out_dir, 'ypr_mosaic_not_cor.png'), plot = nscor_mosaic,
       width=12, height=8, unit='in')
# YPR reference point distributions
ggsave(filename=file.path(out_dir, 'ypr_ref_pts_sel_cor.png'), plot = scor_refdist,
       width=8, height=6, unit='in')
ggsave(filename=file.path(out_dir, 'ypr_ref_pts_not_cor.png'), plot = nscor_refdist,
       width=8, height=6, unit='in')
ggsave(filename=file.path(out_dir, 'ypr_ref_pts_combined.png'), plot = comb_refdist,
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