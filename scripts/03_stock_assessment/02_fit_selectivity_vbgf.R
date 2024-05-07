# Author: Saeesh Mangwani
# Date: 2024-04-10

# Description: Estimating von Bertallanfy growth curve with and without
# selectivity correction for the entire fishing period

# ==== libraries ====
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(purrr)
library(TropFishR)
library(anomalize)

# ==== Paths and global variables ====

# Species
species <- 'lane_snapper'

# Full-observation database (i.e not aggregated by isoweek) since we're
# interested in all observations for which the mesh-size v/s length relationship
# is available)
fdb_path <- 'data/stock_assessment/lane_snapper_total.csv'

# Output directory
out_dir <- 'data/stock_assessment'

# ==== Reading data ====
fdb_og <- read_csv(fdb_path)

# ==== Preparing data  ====
fdb <- fdb_og |> 
  # Keeping only data for Tela
  filter(region == 'Tela') |>
  # Converting month to factor
  mutate(month = factor(month, levels=month.abb)) |> 
  # Combining all dates to a single year - basically setting all the years to be
  # the same, so that data from the same month across all the years are pooled
  # into 1 bin
  mutate(fecha = ymd(paste('2024', month, '15', sep='-'))) |> 
  # Removing rows with missing length information
  filter(!is.na(longitud)) |> 
  # Fixing precision of total length and mesh size
  mutate(longitud = round(longitud, 2)) |>
  mutate(luz_malla = floor(luz_malla)) |>
  # mutate(luz_malla = round(luz_malla, 1)) |>
  # mutate(luz_malla = case_when(
  #   luz_malla == 2.5 ~ 2,
  #   luz_malla == 3.3 ~ 3.5,
  #   T ~ luz_malla
  # )) |>
  # Keeping only relevant columns
  dplyr::select(year, month, fecha, peso, longitud, horas_pesca, luz_malla)

# Database for fitting the selectivity curve, only keeping observations with
# available mesh-size data
seldat <- fdb |> 
  # Removing rows with missing mesh-size info
  filter(!is.na(luz_malla)) |>
  # Removing outlier mesh size - has only 1 data point
  filter(luz_malla < 6)

# Mesh size samples per year
table(seldat |> dplyr::select(year, month))

# Cleaning outlier lengths from the length database
fdb <- fdb |> 
  mutate(isanom = anomalize::iqr(longitud, alpha=0.05)) |>
  filter(isanom == 'No')

# ==== Creating base lfq object ====
range(fdb$longitud)
hist(fdb$longitud, breaks=30)
binsize <- 2
lfq <- lfqCreate(fdb, Lname = 'longitud', Dname = 'fecha', 
          bin_size = binsize, species='Lutjanus synagris',
          plot=T, Lmin=6)

# Assigned each lengths from the selectivity curve dataset to a specific
# midLength by classification (using the closest one to each length)
seldat <- seldat |> 
  rowwise() |> 
  mutate(midLengths = lfq$midLengths[which.min(abs(longitud - lfq$midLengths))]) |> 
  ungroup()

range(seldat$midLengths)

# ==== Basic descriptives ====

# Hours-fished per gear - an indication of the relative fishing intensity of
# each mesh size
mesh_hours <- seldat |> 
  dplyr::select(fecha, horas_pesca, luz_malla) |> 
  group_by(fecha) |> 
  distinct() |> 
  group_by(luz_malla) |> 
  summarize(eff = sum(horas_pesca, na.rm=T))
rel_power <- (mesh_hours$eff/sum(mesh_hours$eff)) |> 
  round(3) |> 
  setNames(mesh_hours$luz_malla)

# Mesh-size distibutions by year - 2017-18 had no mesh size info
seldat |> 
  dplyr::select(year, luz_malla) |> 
  table()

# Plots of length-distributions by mesh-size - seems to add up. Clearly more
# data with larger mesh sizes
seldat |> 
  ggplot(aes(x = midLengths)) +
  geom_histogram(stat='count') +
  facet_wrap('luz_malla', scales = 'fixed')

# Mean, standard-deviation and skewness of lengths per mesh size
seldat |> 
  group_by(luz_malla) |> 
  summarize(
    'mean' = mean(midLengths),
    'sd' = sd(midLengths),
    'skew' = moments::skewness(midLengths)
  ) |> 
  tidyr::pivot_longer(2:4, names_to='var', values_to='val') |> 
  ggplot(aes(x = luz_malla, y = val)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap('var', nrow=3, scales='free')

# ==== Selectivity-curve fitting ====

# Creating input object for selectivity curve estimation
params <- list(
  'midLengths' = sort(unique(seldat$midLengths)),
  'meshSizes' = sort(unique(seldat$luz_malla, na.rm=T)),
  'CatchPerNet_mat' = with(seldat, table(midLengths, luz_malla))
)

# Fitting selectivity models - using multiple models and comparing AIC
mods <- c('norm.loc', 'norm.sca', 'lognorm')
mods <- setNames(mods, mods)
seltests <- map(mods, \(mod){
  seltest <- select_Millar(params, rtype=mod, plot=F, rel.power = rel_power)
  print(mod)
  # print(round(seltest$out, 2))
  print(seltest$out[4,])
  return(seltest)
  # print(seltest$estimates)
})

# Visualizing fits for the ones with the best deviance - all have a very poor
# deviance pattern, so choosing the one with the lowest deviance
plot(seltests$norm.loc)
plot(seltests$norm.sca)
plot(seltests$lognorm)

# Chosen best model
bestmod <- 'norm.sca'
seltest <- seltests[[bestmod]]

# Getting the base model result using the gillnet fit function - this function
# returns the parameters k1 and k2, which can be used to re-estimate the
# selectivity at any length (for some reason the select_Millar function does not
# return these)
sel_model <- gillnetfit(
  dat=matrix(c(params$midLengths, params$CatchPerNet_mat),
             byrow = FALSE, ncol=(dim(params$CatchPerNet_mat)[2]+1)), 
  meshsizes=params$meshSizes, 
  rel.power=rel_power,
  rtype=bestmod,
  details=F
)

# A function that estimates the proportional selectivity for a given input
# length and mesh size, taking a selectivity model as an input and an optional
# proportional indication of relative power (relevant in this case) - this
# function is specific to a normal-proportional-variance selectivity model
est_selectivity <- function(length, meshsize, smdl=sel_model, rel_power=NULL){
  v1 <- (length - smdl$gear.pars['k1',][1]*meshsize)^2
  v2 <- (2*smdl$gear.pars['k2',][1] * (meshsize^2))
  res <- exp(-(v1/v2))
  if(!is.null(rel_power)){
    res <- res*rel_power
  }
  return(res)
}

# Function to plot selectivity
plot_sel_curve <- function(plotdf, cum_col_name){
  ggplot() +
    # Cumulative retention curve 
    geom_point(data= plotdf |> filter(curve_type == cum_col_name), 
               aes(x = Length, y = val, colour=curve_type)) +
    geom_line(data= plotdf |> filter(curve_type == cum_col_name), 
              aes(x = Length, y = val), 
              colour='black',
              linewidth=0.8) +
    # Individual mesh retention curves
    geom_line(data= plotdf |> filter(curve_type != cum_col_name), 
              aes(x = Length, y = val, colour=curve_type), 
              linetype='dashed') +
    scale_colour_brewer(palette='Dark2') +
    # geom_hline(yintercept = 1) +
    theme_minimal(12, 'serif') +
    labs(
      y='Retention probability',
      x=NULL,
      colour='Mesh size (cm)'
    )
}

# Visualizing the cumulative selection ogive
round(seltest$selection_ogive_mat, 3) |> 
  as.data.frame() |>
  # mutate(across(2:3, \(x){ifelse(x==0, NA_real_, x)})) |> 
  rowwise() |> 
  mutate(cumulative = sum(c_across(!Length), na.rm=T)) %>%
  mutate(cumulative = cumulative/max(.$cumulative)) |> 
  tidyr::pivot_longer(cols=!Length, names_to='curve_type', values_to='val') |> 
  plot_sel_curve(cum_col_name='cumulative')

# ==== Correcting lfq-object for selectivity ====

# Using the function to calculate the cumulative selectivity for every length in
# the input length-frequency data (accounting for relative fishing intensity of
# each gear)
sel_msize_2 <- est_selectivity(lfq$midLengths, meshsize = 2, 
                               smdl = sel_model, rel_power = rel_power[1]) |> 
  round(3)
sel_msize_3 <- est_selectivity(lfq$midLengths, meshsize = 3, 
                               smdl = sel_model, rel_power = rel_power[2]) |> 
  round(3)

# Calculating cumulative selectivity
selprob <- list('length' = lfq$midLengths, 'sel_2' = sel_msize_2, 'sel_3' = sel_msize_3) |> 
  as_tibble() |> 
  rowwise() |> 
  mutate(cum_sel = sum(c_across(!length), na.rm=T)) %>%
  mutate(cum_sel = round(cum_sel/max(.$cum_sel), 3)) |> 
  pull(cum_sel)
names(selprob) <- lfq$midLengths

# Cutoff limit for selectivity correction (any selection probabilities below
# this are left unchanged (i.e set to 1 in the ogive before correction))
cutoff_prob <- 0.01
selprob <- ifelse(selprob <= cutoff_prob, 1, selprob)

# Correcting lfq data for gear selectivity (vector dividing each column of the
# catch curve by the associated catch probability (scales up the catches to
# reflect the estimate of "true" catch under non-selectivity))
corrected_catch_mx <- round((1/selprob)*lfq$catch, 0)
lfq_cor <- lfq
lfq_cor$catch <- corrected_catch_mx

# Visualizing corrected catch versus raw
# par(mfrow=c(2,1))
plot(x=lfq, Fname = "catch", date.axis = "modern", ylim = c(6, 44))
plot(x=lfq_cor, Fname = "catch", date.axis = "modern", ylim = c(6, 44))
# dev.off()

# ==== Fitting VBGF growth function ====

# Restructing LFQ using the ELEFAN point-system
ma <- 5
lfq_res <- lfqRestructure(lfq_cor, MA = ma, addl.sqrt = F)
# Plotting restructured
plot(lfq_res, Fname = "rcounts", date.axis = "modern")

# Setting initial parameters ----------

# Linf initial guesses 
# Taken from fishbase
# linf_guess <- 60
# linf_guess
# Using 95% of the largest reported size
linf_guess_maxlength <- max(lfq_res$midLengths) / 0.95
linf_guess_maxlength
# Froese and Binohlan (2000) method 
linf_guess_fb <- exp(0.044+0.9841*log(max(lfq_res$midLengths)))
linf_guess_fb

# Powell-wetherall function
linf_guess <- powell_wetherall(param=lfq_res,
                               catch_columns = 1:ncol(lfq_res$catch),
                               reg_int = c(3, 15))
linf_guess$Linf_est
linf_guess$confidenceInt_Linf

## lower search space bounds
low_par <- list(Linf = linf_guess$Linf_est,
                K = 0.01,
                t_anchor = 0)

## upper search space bounds
up_par <- list(Linf = linf_guess_maxlength,
               K = 1,
               t_anchor = 1)

# Fitting ELEFAN ----------

# Simulated annealing
res_SA <- ELEFAN_SA(lfq_res, 
                    SA_time = 60*2,
                    SA_temp = 6e5,
                    MA = ma, seasonalised = F, 
                    addl.sqrt = FALSE,
                    init_par = list(Linf = linf_guess_fb,
                                    K = 0.5,
                                    t_anchor = 0.5),
                    low_par = low_par,
                    up_par = up_par)

# Genetic algorithm
res_GA <- ELEFAN_GA(lfq_res, 
                    MA = ma, seasonalised = F, 
                    addl.sqrt = FALSE,
                    low_par = low_par,
                    up_par = up_par)

## show results
res_SA$par
res_GA$par

# Fit is not great for either
res_SA$Rn_max
res_GA$Rn_max
plot(res_SA)
plot(res_GA)

# ==== VPA to get a different estimate of selectivity and FM ====

# This assumes that the entire catch time-series represents a steady state,
# which may not be the case as the selection pattern and thus the applicable FM
# has changed since 2018/2019 to higher mesh-sized gears

# Adding VBGF parameters to lfq object
lfq_res <- lfqModify(lfq_res, par = res_GA$par)

# Define plus group as largest length class smaller than Linf
midx <- max(which(lfq_res$midLengths < lfq_res$par$Linf))
# Adjusting the index to select the right plus-group midlength if needed (based
# on the catch matrix)
lfq_res$catch
plus_group <- lfq_res$midLengths[midx-1]
plus_group

# Summarise catch matrix into vector and add plus group
lfq_catch_vec <- lfqModify(lfq_res, vectorise_catch = TRUE, plus_group = plus_group)

# Calculating length-weight parameters
lw_model <- (lm(log(peso)~log(longitud), data = fdb))
a_mod <- exp(coef(lw_model)[1])
b_mod <- (coef(lw_model)[2])

# Adding to object
lfq_catch_vec$par$M <- M_pauly <- M_empirical(
  Linf = res_GA$par$Linf, 
  K_l = res_GA$par$K, 
  temp=28.3, 
  method = "Pauly_Linf", 
  schooling = T)
lfq_catch_vec$par$a <- a_mod
lfq_catch_vec$par$b <- b_mod

# VPA
vpa_res <- VPA(
  param = lfq_catch_vec, 
  terminalF = selprob[as.character(plus_group)],
  analysis_type = "VPA", catch_unit = "'000",
  plot=TRUE, algorithm = 'new'
)

# ==== Saving results: gear-selectivity function and growth parameters ====
saveobjs <- list(
  'base_lfq' = lfq,
  'sel_model_Millar' = seltest,
  'selectivity_model' = sel_model,
  'plot_sel_curve' = plot_sel_curve,
  'est_selectivity' = est_selectivity,
  'lfq_base' = lfq,
  'lfq_corrected' = lfq_cor,
  'ma_window' = ma,
  'lfq_elefan_restructure' = lfq_res,
  'elefan_sa_result' = res_SA,
  'elefan_ga_result' = res_GA,
  'overall_vpa' = vpa_res
)

# Saving to disk
saveRDS(saveobjs, file.path(out_dir, 'growth_selectivity.rds'))
