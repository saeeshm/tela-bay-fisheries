# Author: Saeesh Mangwani
# Date: 2024-04-10

# Description: Estimating von Bertallanfy growth curve with and without
# selectivity correction for the entire fishing period

# ==== libraries ====
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(patchwork)
library(TropFishR)
library(anomalize)
source('scripts/03_stock_analysis/utils/_general_utils.R')
source('scripts/03_stock_analysis/utils/03_selectivity_utils.R')

# ==== Paths and global variables ====

# Calale database path
# fdb_path <- 'data/stock_analysis/calale_sa_raw.csv'
# Calale LFQ-adjusted database path
lfq_adj_path <-  'data/stock_analysis/calale_lfq_adjust.csv'

# Output directory (for data outputs - used for subsequent analysis)
out_dir <- 'data/stock_analysis/ypr_calale'
# Plot directory (for reporting outputs)
plot_dir <- 'output/stock_analysis/ss_calale'
# dir.create(out_dir)
# dir.create(plot_dir)

# ==== Reading data ====
lfqAdj_og <- read_csv(lfq_adj_path)

# ==== Preparing data for analysis ====
lfqAdj <- lfqAdj_og |>
  # Removing data from gears with very little info
  filter(!tipo_arte %in% c('Chinchorro')) |> 
  # Removing rows with missing length information
  filter(!is.na(long_bin))

# ==== Preparing data for selectivity analysis  ====

# Getting all length observations as a vector
lenvec <- lfqAdj |> 
  dplyr::select(long_bin, adjCount) |> 
  pmap(function(long_bin, adjCount){
    rep(long_bin, adjCount)
  }) |> 
  unlist()

# Identifying lengths that are outliers
isanom <- anomalize::iqr(lenvec, alpha=0.05)
outLens <- lenvec[isanom=='Yes']

# Preparing selectivity dataframe
seldat <- lfqAdj |> 
  # Removing outliers
  filter(!long_bin %in% outLens) |> 
  # Creating year and month columns
  mutate(year = year(fecha)) |> 
  mutate(month = month.abb[month(fecha)]) |> 
  mutate(month = factor(month, levels=month.abb)) |> 
  # Combining all dates to a single year - basically setting all the years to be
  # the same, so that data from the same month across all the years are pooled
  # but monthly information is kept
  mutate(fecha = ymd(paste('2024', month, '15', sep='-'))) |> 
  mutate(luz_malla = case_when(
    tipo_arte == 'Gillnet - 2in' ~ 2,
    tipo_arte == 'Gillnet - 3in' ~ 3,
    T ~ NA
  )) |> 
  # Keeping only rows with available mesh-size info
  filter(!is.na(luz_malla))

# Mesh size samples per year
table(seldat |> dplyr::select(year, month))

# ==== Basic descriptives ====

# Mesh-size distibutions by year - 2017-19 had no mesh size info
seldat |> 
  dplyr::select(year, luz_malla) |> 
  table()

# Mean length of the catch by mesh size
meanLens <- seldat |> 
  mutate(luz_malla = paste(luz_malla, 'in')) |> 
  group_by(luz_malla) |> 
  summarize(mlen=round(mean(long_bin), 2))

# Plots of length-distributions by mesh-size
meshLfq <- seldat |> 
  mutate(luz_malla = paste(luz_malla, 'in')) |> 
  ggplot() +
  geom_bar(aes(x = long_bin, weight=adjCount),
           stat='count', fill='white', colour='black') +
  geom_vline(data=meanLens, 
             aes(xintercept = mlen), 
             colour='firebrick', linetype='dashed',
             linewidth=0.6) +
  geom_text(data = meanLens |> 
              mutate(lab=paste(mlen, 'cm')), 
            aes(x = mlen, label=lab),
            y=350, nudge_x = 5, size=3,
            family = 'serif') +
  scale_y_continuous(expand=c(0, 5)) +
  facet_wrap('luz_malla', scales = 'fixed') +
  qtheme() +
  labs(x='Length (cm)', y='Frequency')
meshLfq

# Mean, standard-deviation and skewness of lengths per mesh size
seldat |> 
  group_by(luz_malla) |> 
  group_modify(function(x, ...){
    lenvec <- x |> 
      dplyr::select(long_bin, adjCount) |> 
      pmap(function(long_bin, adjCount){
        rep(long_bin, adjCount)
      }) |> 
      unlist() 
    tibble(
      'mean' = mean(lenvec),
      'sd' = sd(lenvec),
      'skew' = moments::skewness(lenvec)
    )
  }) |> 
  tidyr::pivot_longer(2:4, names_to='var', values_to='val') |> 
  ggplot(aes(x = luz_malla, y = val)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap('var', nrow=3, scales='free')

# ==== Selectivity-curve fitting ====

# Creating the catch per net matrix
cpn_mat <- seldat |> 
  # Getting adjusted counts per mesh size per length class
  group_by(luz_malla, long_bin) |> 
  summarize(count = sum(adjCount, na.rm=T)) |> 
  tidyr::pivot_wider(names_from='luz_malla', values_from='count') |> 
  arrange(long_bin) |> 
  # Replacing NAs with 0
  mutate(across(!contains('long_bin'), \(x){
    ifelse(is.na(x), 0, x)
  })) |> 
  as.data.frame()
rownames(cpn_mat)  <- cpn_mat$long_bin
# Converting to a matrix that TropfishR can parse
cpn_mat <- cpn_mat |> 
  dplyr::select(-long_bin) |> 
  as.matrix() |> 
  as.table()
  
# Creating input object for selectivity curve estimation
params <- list(
  'midLengths' = sort(unique(seldat$long_bin)),
  'meshSizes' = sort(unique(seldat$luz_malla, na.rm=T)),
  'CatchPerNet_mat' = cpn_mat
)

# Fitting selectivity models - using multiple models and comparing AIC
mods <- c('norm.loc', 'norm.sca', 'lognorm')
mods <- setNames(mods, mods)
seltests <- map(mods, \(mod){
  seltest <- select_Millar(
    params, 
    rtype=mod, 
    plot=F
  )
  print(mod)
  print(seltest$out[4,])
  return(seltest)
})

# Visualizing fits for the ones with the best deviance - all have a very poor
# deviance pattern, so choosing the one with the lowest deviance
plot(seltests$norm.loc)
plot(seltests$norm.sca)
plot(seltests$lognorm)

# Chosen best model
# bestmod <- 'norm.sca'
bestmod <- 'lognorm'
seltest <- seltests[[bestmod]]

# Getting the base model result using the gillnet fit function - this function
# returns the parameters k1 and k2, which can be used to re-estimate the
# selectivity at any length (for some reason the select_Millar function does not
# return these)
sel_model <- gillnetfit(
  dat=matrix(c(params$midLengths, params$CatchPerNet_mat),
             byrow = FALSE, ncol=(dim(params$CatchPerNet_mat)[2]+1)), 
  meshsizes=params$meshSizes, 
  # rel.power=rel_power,
  # rtype='norm.sca',
  rtype=bestmod,
  details=F
)

# Getting the right function to estimate selectivity based on the model type
est_selectivity <- get_sel_func(sel_model)

# ==== Plotting ====

# Plotting the deviance residuals
devResidPlot <- seltest$Dev.resids |> 
  data.frame() |> 
  setNames(c('Length', 'Mesh', 'dev')) |> 
  mutate(Mesh = paste0(Mesh, ' inch')) |> 
  ggplot(aes(x = Length, y=dev)) +
  geom_hline(yintercept = 0) +
  geom_col(width = 0.05, colour='black', linewidth=0.2) +
  facet_wrap('Mesh', nrow=2) +
  qtheme() +
  labs(
    y = 'Deviance Residual'
  )

# Function for plotting the selectivity curve
plot_sel_curve <- function(plotdf, cum_col_name){
  ggplot() +
    # Cumulative retention curve 
    # geom_point(data= plotdf |> filter(curve_type == cum_col_name), 
    #            aes(x = Length, y = val, colour=curve_type)) +
    geom_line(data= plotdf |> filter(curve_type == cum_col_name), 
              aes(x = Length, y = val, colour=curve_type), 
              linewidth=0.8) +
    # Individual mesh retention curves
    geom_line(data= plotdf |> filter(curve_type != cum_col_name), 
              aes(x = Length, y = val, colour=curve_type), 
              linetype='dashed') +
    # scale_colour_brewer(palette='Dark2') +
    scale_colour_manual(values=c('#008080', 'orange', 'black')) +
    # geom_hline(yintercept = 1) +
    theme_minimal(12, 'serif') +
    labs(
      y='Retention probability',
      x='Length (cm)',
      colour='Curve'
    )
}

# est_selectivity(length = 13, 2, 2, sel_model, rel_power = rel_power)
# Visualizing the individual and cumulative selection ogives
selcurves <- round(seltest$selection_ogive_mat, 3) |> 
  as.data.frame() |>
  # mutate(across(2:3, \(x){ifelse(x==0, NA_real_, x)})) |> 
  rowwise() |> 
  mutate(cumulative = sum(c_across(!Length), na.rm=T)) %>%
  mutate(cumulative = cumulative/max(.$cumulative)) |> 
  setNames(c("Length", '2 cm', '3 cm', 'Cumulative')) |> 
  tidyr::pivot_longer(cols=!Length, names_to='curve_type', values_to='val') |> 
  plot_sel_curve(cum_col_name='Cumulative') +
  labs(subtitle='Estimated Mesh Selectivity')

# Plotting them as a mosaic with the raw length-frequency distribution
selmosaic <- (meshLfq + labs(subtitle='Catch Distribution by Mesh Size'))/
  (selcurves + labs(subtitle='Estimated Mesh Selectivity')) +
  plot_layout(guides='collect', axis_titles='collect_x')
selmosaic

# ==== Exporting plots and objects ====

# Plots 
ggsave(plot=selmosaic, filename=file.path(plot_dir, 'vbgf/calale_mesh_lfq_sel_mosaic.png'),
       width=7, height=5, scale=1.3)
ggsave(plot=devResidPlot, filename=file.path(plot_dir, 'vbgf/select_deviance_resid_plot.png'),
       width=7, height=5, scale=1)


# Data
saveobjs <- list(
  'selmodel' = sel_model,
  'plot_sel_func' = plot_sel_curve,
  'rel_power' = rel_power,
  'est_selectivity' = est_selectivity
)
saveRDS(saveobjs, file.path(out_dir, 'calale_selectivity.rds'))
