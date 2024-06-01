# Author: Saeesh Mangwani
# Date: 2024-05-10

# Description: Fitting VBGF to estimate vital growth parameters for the Tela
# Calale fishery

# ==== libraries ====
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(TropFishR)
library(anomalize)
source('scripts/03_stock_analysis/utils/_general_utils.R')

# ==== User variables ====

# Scenario being tested - assessment for all data together, with only
# selectivity correction applied to the mesh data, or just mesh data with
# selectivity correction applied
scn <- 'all'
# scn <- 'mesh'

# ==== Paths and global variables ====

# Calale LFQ-adjusted database path
lfq_adj_path <-  'data/stock_analysis/calale_lfq_adjust.csv'
# Path to selectivity model data
sel_model_path <- 'data/stock_analysis/ypr_calale/calale_selectivity.rds'

# Output directory (for data outputs - used for subsequent analysis)
out_dir <- 'data/stock_analysis/ypr_calale'
# Plot directory (for reporting outputs)
plot_dir <- 'output/stock_analysis/ss_calale/vbgf'
# dir.create(out_dir)
# dir.create(plot_dir)

# ==== Reading data ====
lfqAdj_og <- read_csv(lfq_adj_path)
# Selectivity model objects
sel <- readRDS(sel_model_path)

# ==== Preparing data ====
lfqAdj <- lfqAdj_og |> 
  # Removing data from gears with very little info
  filter(!tipo_arte %in% c('Chinchorro')) |> 
  # Removing rows with missing length information
  filter(!is.na(long_bin)) |> 
  # Creating year and month columns
  mutate(year = year(fecha)) |> 
  mutate(month = month.abb[month(fecha)]) |> 
  mutate(month = factor(month, levels=month.abb)) |> 
  # Combining all dates to a single year - basically setting all the years to be
  # the same, so that data from the same month across all the years are pooled
  # but monthly information is kept
  mutate(fecha = ymd(paste('2024', month, '15', sep='-')))

# ==== Building base objects for VBGF fit ====

# Removing outlier lengths ----------

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

# Removing outliers
vbgfDat <- lfqAdj |> 
  # Removing outliers
  filter(!long_bin %in% outLens)

# Correcting mesh observations for selectivity ----------

# Cutoff limit for selectivity correction (any selection probabilities below
# this are left unchanged (i.e set to 1 in the ogive before correction))
cutoff_prob <- 0.01

# Filtering mesh data from the rest
ltabs <- vbgfDat |> 
  group_by(month, tipo_arte, long_bin) |> 
  summarize(
    'adjCount' = sum(adjCount)
  ) |> 
  mutate(hasMesh = ifelse(str_detect(tipo_arte, 'Trasmallo'), 'Yes', 'No')) |> 
  group_by(hasMesh) |> 
  group_split()

# Calculating a table of cumulative selection probabilities (scaled by overall
# relative power) for the entire length range
ulengths <- sort(unique(ltabs[[2]]$long_bin))
sel_2 <- sel$est_selectivity(ulengths, 2, mesh1=2, sel$selmodel, sel$rel_power)
sel_3 <- sel$est_selectivity(ulengths, 3, mesh1=2, sel$selmodel, sel$rel_power)
cum_sel_tab <- tibble(
  'long_bin' = ulengths,
  'sel_2' = round(sel_2, 3),
  'sel_3' = round(sel_3, 3),
) |> 
  rowwise() |> 
  mutate(cum_sel = sum(c_across(!long_bin), na.rm=T)) %>%
  mutate(cum_sel = cum_sel/max(.$cum_sel)) |> 
  dplyr::select(long_bin, cum_sel)

# For the net data, correcting the frequencies based on the selectivity model
# appropriate for each mesh-size used
meshCor <- ltabs[[2]] |> 
  mutate(luz_malla = case_when(
    tipo_arte == 'Trasmallo - 2cm'~  2, 
    tipo_arte == 'Trasmallo - 3cm'~  3, 
    T ~ NA_integer_, 
  )) |> 
  # Getting selection probabilities for rows where mesh size info is available
  mutate(selprob = sel$est_selectivity(
    length=long_bin,
    meshsize=luz_malla,
    # mesh1 = 2,
    smdl = sel$selmodel,
    rel_power = NULL
  )) |> 
  # Where mesh size is unknown, using the cumulative selection probability
  # scaled by overall relative power
  left_join(cum_sel_tab) |> 
  mutate(selprob = ifelse(is.na(selprob), cum_sel, selprob)) |>
  mutate(selprob=cum_sel) |>
  # Removing probabilities below the cutoff
  mutate(selprob = ifelse(selprob <= cutoff_prob, 1, selprob)) |>
  # Correcting counts based on selectivity
  mutate(corCount = round((1/selprob)*adjCount, 0))

# Visualizing corrected counts
selCorPlot <- meshCor |> 
  ggplot() +
  geom_bar(aes(x = long_bin, weight=corCount),
           fill='darkgreen', colour='black', alpha=0.8) +
  geom_bar(aes(x = long_bin, weight=adjCount),
           fill='orange', colour='black', alpha=1) +
  facet_wrap('tipo_arte') +
  qtheme() +
  labs(
    x = "Length (cm)",
    y = 'Frequency'
  )
selCorPlot

# ==== Preparing LFQ object for VBGF fitting ====

# Getting the relevant dataset (either the corrected mesh data combined with the
# data from the other gears - really just Linea de Mano - or just the mesh data)
relvLfq <- if(scn == 'all'){
  print("Using all data")
  ltabs[[1]] |> 
    mutate(corCount = adjCount) |> 
    bind_rows(meshCor)
}else{
  print("Using only mesh data")
  meshCor
}

# Creating the catch per length-bin matrix
cpn_mat <- relvLfq |> 
  mutate(relvCount = corCount) |>
  # mutate(relvCount = adjCount) |> 
  # Creating a date column that refers to the middle of each month
  mutate(fecha = ymd(paste0('2024-',month,'15'))) |> 
  # Getting total counts per length class
  group_by(fecha, long_bin) |> 
  summarize(count = sum(relvCount, na.rm=T)) |> 
  tidyr::pivot_wider(names_from='fecha', values_from='count') |> 
  arrange(long_bin) |> 
  # Replacing NAs with 0
  mutate(across(!contains('long_bin'), \(x){
    ifelse(is.na(x), 0, x)
  })) |> 
  as.data.frame()

# Adding length bins with 0 observations where there are gaps
full_range <- seq(min(relvLfq$long_bin), max(relvLfq$long_bin), 2)
missing_nums <- full_range[!full_range %in% unique(relvLfq$long_bin)]
missing_rows <- map_dfr(missing_nums, \(x){
  setNames(c(x, rep(0, 12)), names(cpn_mat))
})
cpn_mat <- cpn_mat |> 
  bind_rows(missing_rows) |> 
  arrange(long_bin)

# Converting to a matrix that TropfishR can parse
rownames(cpn_mat)  <- cpn_mat$long_bin
cpn_mx <- cpn_mat |> 
  dplyr::select(-long_bin) |> 
  as.matrix()

# Building LFQ object
lfq <- list(
  dates = as.Date(names(cpn_mat)[2:ncol(cpn_mat)]),
  midLengths = cpn_mat$long_bin,
  catch=cpn_mx
)
class(lfq) <- 'lfq'

# Visualizing lfq object
plot(x=lfq, Fname = "catch", date.axis = "modern")

# ==== Fitting VBGF growth function ====

# Restructing LFQ using the ELEFAN point-system
ma <- 5
lfq_res <- lfqRestructure(lfq, MA = ma, addl.sqrt = F)
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
# linf_guess <- powell_wetherall(param=lfq_res, catch_columns = 1:12, reg_int=c(2,8))
# linf_guess$Linf_est
# linf_guess$confidenceInt_Linf

range_fact <- 0.1
## lower search space bounds
low_par <- list(Linf = linf_guess_fb-linf_guess_fb*range_fact,
                K = 0.01,
                t_anchor = 0)

## upper search space bounds
up_par <- list(Linf = linf_guess_fb+linf_guess_fb*range_fact,
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

# ==== Estimating an overall fishing mortality based on this LFQ data ====

# Chosen elefan result for computing the rest of the stops
elefan_res <- res_GA

# Estimate of M - using Pauly's empirical equation
est_m <- M_empirical(
  Linf = elefan_res$par$Linf, 
  K_l = elefan_res$par$K, 
  temp=26, 
  method = "Pauly_Linf", 
  schooling = F
)
est_m

# Adding VBGF growth parameters to the length-frequency object
lfq_cc <- lfqModify(lfq, par = elefan_res$par)
# lfq_cc <- lfqModify(lfq, par = res_SA$par)

# Visualizing the catch matrix to double-check if the plus group needs to be
# adjusted
lfq_cc$midLengths
lfq_cc$catch
# Define plus group as largest length class smaller than Linf
midx <- max(which(lfq_cc$midLengths < lfq_cc$par$Linf))
# Adjusting the index to select the right plus-group midlength if needed (based
# on the catch matrix)
# plus_group <- lfq_cc$midLengths[midx-2]
plus_group <- lfq_cc$midLengths[midx-1]
plus_group

# Summarise catch matrix into vector and add plus group
lfq_catch_vec <- lfqModify(lfq_cc, vectorise_catch = TRUE, 
                           plus_group = plus_group)

# Catch-curve analysis to estimate total mortality
res_cc <- catchCurve(
  lfq_catch_vec, 
  # reg_int = c(9, 17),
  reg_int = c(9, 18),
  calc_ogive = F,
  plot = F)

# Visualizing
res_cc$Z
plot(res_cc)

# Estimated overall F
est_f <- res_cc$Z - est_m
# Exploitation rate!
est_f/res_cc$Z

# # ==== VPA to get a different estimate of selectivity and FM ====
# 
# # This assumes that the entire catch time-series represents a steady state,
# # which may not be the case as the selection pattern and thus the applicable FM
# # has changed since 2018/2019 to higher mesh-sized gears
# 
# # Adding VBGF parameters to lfq object
# lfq_res <- lfqModify(lfq_res, par = res_GA$par)
# 
# # Define plus group as largest length class smaller than Linf
# midx <- max(which(lfq_res$midLengths < lfq_res$par$Linf))
# # Adjusting the index to select the right plus-group midlength if needed (based
# # on the catch matrix)
# lfq_res$catch
# plus_group <- lfq_res$midLengths[midx-1]
# plus_group
# 
# # Summarise catch matrix into vector and add plus group
# lfq_catch_vec <- lfqModify(lfq_res, vectorise_catch = TRUE, plus_group = plus_group)
# 
# # Calculating length-weight parameters
# lw_model <- (lm(log(peso)~log(longitud), data = fdb))
# a_mod <- exp(coef(lw_model)[1])
# b_mod <- (coef(lw_model)[2])
# 
# # Adding to object
# lfq_catch_vec$par$M <- M_pauly <- M_empirical(
#   Linf = res_GA$par$Linf, 
#   K_l = res_GA$par$K, 
#   temp=28.3, 
#   method = "Pauly_Linf", 
#   schooling = T)
# lfq_catch_vec$par$a <- a_mod
# lfq_catch_vec$par$b <- b_mod
# 
# # VPA
# vpa_res <- VPA(
#   param = lfq_catch_vec, 
#   terminalF = selprob[as.character(plus_group)],
#   analysis_type = "VPA", catch_unit = "'000",
#   plot=TRUE, algorithm = 'new'
# )

# ==== Exporting results ====

# Plot results 
ggsave(plot=selCorPlot, filename=file.path(plot_dir, 'lfq_sel_cor_mesh.png'),
       width=7, height=5, scale=1.3)

# Data results
saveobjs <- list(
  'inputDat' = relvLfq,
  'lfq_base' = lfq,
  'ma' = ma,
  'lfq_elefan_restructure' = lfq_res,
  'elefan_sa' = res_SA,
  'elefan_ga' = res_GA,
  'cc' = res_cc,
  'm_pauly' = est_m
)
saveRDS(saveobjs, file.path(out_dir, paste0('vbgf_', scn,'.rds')))
