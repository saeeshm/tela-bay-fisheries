# Author: Saeesh Mangwani
# Date: 2024-03-23

# Description: Calculating Yield-per-recruit reference points for each year of
# available length-frequency data for this species. Analysis on length-frequency
# data

# ==== libraries ====
library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(tidyr)
library(anomalize)
library(TropFishR)
library(ggplot2)
library(patchwork)
source('scripts/03_stock_analysis/utils/_general_utils.R')
# source('scripts/03_stock_analysis/utils/05_ypr_utils.R')

# ==== User variables ====

# Period being analyzed
# currperiod <- 'before'
currperiod <- 'after'

# Scenario being tested - assessment for all data together, with only
# selectivity correction applied to the mesh data, or just mesh data with
# selectivity correction applied
scn <- 'all'
# scn <- 'mesh'

# Use selectivity correction or not
useSelCor <- T
# useSelCor <- F

# Use overall total mortality or estimate separate total mortalities for each
# period
# useOverallZM <- T
useOverallZM <- F

# ==== Paths and global variables ====

# Calale full database path
fdb_path <- 'data/stock_analysis/calale_sa_raw.csv'
# Calale LFQ-adjusted database path
lfq_adj_path <-  'data/stock_analysis/calale_lfq_adjust.csv'
# Path to selectivity model data
sel_model_path <- 'data/stock_analysis/ypr_calale/calale_selectivity.rds'
# Path to VBGF model data
growth_model_path <- paste0('data/stock_analysis/ypr_calale/vbgf_', scn, '.rds')

# Output directory (for data outputs - used for subsequent analysis)
out_dir <- 'data/stock_analysis/ypr_calale/ypr'
# Plot directory (for reporting outputs)
plot_dir <- 'output/stock_analysis/ss_calale'

# Output directory to store YPR results
# out_dir <- 'output/stock_assessment/ypr/annual_tables_selcor'
# out_dir <- 'output/stock_assessment/ypr/annual_tables_noselcor'

# ==== Reading data ====
fdb_og <- read_csv(fdb_path)
lfqAdj_og <- read_csv(lfq_adj_path)
# Selectivity model objects
sel <- readRDS(sel_model_path)
# Growth model objects
growth <- readRDS(growth_model_path)

# ==== Preparing data ====

# Basic data preparation
lfqAdj <- lfqAdj_og |> 
  # Removing data from gears with very little info
  filter(!tipo_arte %in% c('Chinchorro')) |> 
  # Removing rows with missing length information
  filter(!is.na(long_bin)) |> 
  # Creating year and month columns
  mutate(year = year(fecha)) |> 
  mutate(month = month.abb[month(fecha)]) |> 
  mutate(month = factor(month, levels=month.abb)) |> 
  # Adding a mesh size variable
  mutate(luz_malla = case_when(
    tipo_arte == 'Trasmallo - 2cm' ~ 2,
    tipo_arte == 'Trasmallo - 3cm' ~ 3,
    T ~ NA
  )) |> 
  # Adding a period variable to designate before and after periods
  mutate(period = case_when(
    # year %in% 2015:2017 ~ 'before',
    # year %in% 2021:2023 ~ 'after',
    year %in% 2015:2018 ~ 'before',
    year %in% 2019:2023 ~ 'after',
    T ~ NA_character_
  )) |> 
  # Keeping only before/after data
  filter(!is.na(period))

fdb <- fdb_og |> 
  # Removing data from gears with very little info
  filter(!tipo_arte %in% c('Chinchorro')) |> 
  # Removing rows with missing length information
  filter(!is.na(long_bin)) |> 
  # Creating year and month columns
  mutate(year = year(fecha)) |> 
  mutate(month = month.abb[month(fecha)]) |> 
  mutate(month = factor(month, levels=month.abb)) |> 
  # Adding a mesh size variable
  mutate(luz_malla = case_when(
    tipo_arte == 'Trasmallo - 2cm' ~ 2,
    tipo_arte == 'Trasmallo - 3cm' ~ 3,
    T ~ NA
  )) |> 
  # Adding a period variable to designate before and after periods
  mutate(period = case_when(
    # year %in% 2015:2017 ~ 'before',
    # year %in% 2021:2023 ~ 'after',
    year %in% 2015:2018 ~ 'before',
    year %in% 2019:2023 ~ 'after',
    T ~ NA_character_
  )) |> 
  # Keeping only before/after data
  filter(!is.na(period))

# Removing outlier length observations ----------

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
outLens

# Removing outliers
lfqAdj <- lfqAdj |> 
  filter(!long_bin %in% outLens)
fdb <- fdb |> 
  filter(!long_bin %in% outLens)

# Sampling coverage per year a great amount of samples per year
table(lfqAdj |> dplyr::select(period, month))

# Splitting datasets by periods
pdats <- lfqAdj |> 
  group_by(period) |> 
  group_split()
names(pdats) <- rev(unique(lfqAdj$period))

# Combining all years for each period to reflect only 1 year - i.e this returns
# month-level variability but collapses data for each month across all the years
# in the period
pdats <- imap(pdats, \(x, y){
  if(y=='before'){
    x |> 
      mutate(fecha = ymd(paste('2016', month, '15', sep='-')))
  }else{
    x |> 
      mutate(fecha = ymd(paste('2022', month, '15', sep='-')))
  }
})

# ==== Initial parameters - period, binsize, gear relative-power ====

# Getting data for the current year
cdb <- pdats[[currperiod]]

# Using a shared binsize of 2 - this is what we used to aggregate the data in
# the initial level, so best to stick with it
binsize <- 2

# Number of days the gear was used - an indication of the relative fishing
# intensity of each mesh size
mesh_eff <- cdb |> 
  group_by(luz_malla) |> 
  summarize(eff = length(unique(fecha))) |> 
  filter(!is.na(luz_malla))
# summarize(eff = sum(adjWt, na.rm=T))
rel_power <- (mesh_eff$eff/sum(mesh_eff$eff)) |> 
  round(2) |> 
  setNames(mesh_eff$luz_malla)
rel_power
# Rescaling to be centered at 1
scaled_min <- round(min(rel_power)/max(rel_power), 3)
rel_power[which.max(rel_power)] <- 1.000
rel_power[which.min(rel_power)] <- scaled_min
if(length(rel_power)==1) rel_power <- c('2'=0, rel_power) 
rel_power

# ==== Correcting mesh observations for selectivity ====

# Cutoff limit for selectivity correction (any selection probabilities below
# this are left unchanged (i.e set to 1 in the ogive before correction))
cutoff_prob <- 0.01

# Filtering mesh data from the rest
ltabs <- cdb |> 
  group_by(month, tipo_arte, long_bin) |> 
  summarize(
    'adjCount' = sum(adjCount)
  ) |> 
  mutate(hasMesh = ifelse(str_detect(tipo_arte, 'Trasmallo'), 'Yes', 'No')) |> 
  group_by(hasMesh) |> 
  group_split()

# Calculating a table of cumulative selection probabilities (scaled by overall
# relative power) for the entire length range
lrange <- range(ltabs[[2]]$long_bin)
ulengths <- seq(lrange[1], lrange[2], 2)
cum_sel_tab <- tibble(
  'long_bin' = ulengths,
  'sel_2' = sel$est_selectivity(ulengths, 2, mesh1=2, sel$selmodel, rel_power),
  'sel_3' = sel$est_selectivity(ulengths, 3, mesh1=2, sel$selmodel, rel_power),
) |> 
  rowwise() |> 
  mutate(cum_sel = sum(c_across(!long_bin), na.rm=T)) %>%
  mutate(cum_sel = cum_sel/max(.$cum_sel))

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
    mesh1 = 2,
    smdl = sel$selmodel,
    rel_power = NULL
  )) |> 
  # Where mesh size is unknown, using the cumulative selection probability
  # scaled by overall relative power
  left_join(cum_sel_tab |> 
              dplyr::select(long_bin, cum_sel)) |> 
  mutate(selprob = ifelse(is.na(selprob), cum_sel, selprob)) |>
  mutate(selprob=cum_sel) |>
  # Removing probabilities below the cutoff
  mutate(selprob = ifelse(selprob <= cutoff_prob, 1, selprob)) |>
  # Correcting counts based on selectivity
  mutate(corCount = round((1/selprob)*adjCount, 0))

# Visualizing corrected counts
corCatchPlot <- meshCor |> 
  ggplot() +
  geom_bar(aes(x = long_bin, weight=corCount),
           fill='darkgreen', colour='black', alpha=0.8) +
  geom_bar(aes(x = long_bin, weight=adjCount),
           fill='orange', colour='black', alpha=1) +
  # facet_wrap('tipo_arte') +
  qtheme() +
  labs(
    x = "Length (cm)",
    y = 'Frequency'
  )
corCatchPlot

# Visualizing the selection curve for this period
selCurvePlot <- cum_sel_tab |>
  setNames(c('Length', '2 cm', '3 cm', 'Cumulative')) |> 
  tidyr::pivot_longer(cols=!Length, names_to='curve_type', values_to='val') |>
  sel$plot_sel_func(cum_col_name = 'Cumulative') +
  xlim(c(min(cum_sel_tab$long_bin)-1, max(cum_sel_tab$long_bin)+1))
# selCurvePlot

# Visualzing both as a mosaic
lfqCorMosaic <- (corCatchPlot/selCurvePlot)
lfqCorMosaic

# ==== Preparing base LFQ object for YPR ====

# Choosing the appropriate mesh dataset depending on if we're using selectivity
# correction or not
meshDat <- if(useSelCor){
  print("Using selectivity-corrected mesh data")
  meshCor
}else{
  print("Not correcting mesh data for selectivity")
  ltabs[[2]] |> 
    mutate(corCount=adjCount)
}

# Getting the relevant dataset (either the corrected mesh data combined with the
# data from the other gears - really just Linea de Mano - or just the mesh data)
relvLfq <- if(scn == 'all'){
  print("Using all data")
  ltabs[[1]] |> 
    mutate(corCount = adjCount) |> 
    bind_rows(meshDat)
}else{
  print("Using only mesh data")
  meshDat
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
  setNames(c(x, rep(0, (ncol(cpn_mat)-1))), names(cpn_mat))
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

# ==== Estimating Z using the catch-curve ====

# Choosing an elefan result to work with
elefan_res <- growth$elefan_ga

# Adding VBGF growth parameters to the length-frequency object
# lfq_cc <- lfqModify(lfq, par = sg$elefan_sa_result$par)
lfq_cc <- lfqModify(lfq, par = elefan_res$par)

# Visualizing the catch matrix to double-check if the plus group needs to be
# adjusted
lfq_cc$midLengths
lfq_cc$catch
# Define plus group as largest length class smaller than Linf
midx <- max(which(lfq_cc$midLengths < lfq_cc$par$Linf))
# Adjusting the index to select the right plus-group midlength if needed (based
# on the catch matrix)
plus_group <- lfq_cc$midLengths[midx-1]
plus_group

# Summarise catch matrix into vector and add plus group
lfq_catch_vec <- lfqModify(lfq_cc, vectorise_catch = TRUE, plus_group = plus_group)

#List of regression intervals (defined by manual investigation, pre-set here to
#make re-running of the script easier) 
reg_ints <- list(
  # 'before-all' = c(7, 16),
  'before-all' = c(8, 16),
  # 'before-mesh' = c(5, 12),
  'before-mesh' = c(8, 16),
  'after-all' = c(8, 12),
  'after-mesh' = c(8, 12)
)

#run catch curve (intervals defined after an initial interactive review
#lfq_catch_vec$catch <- ifelse(lfq_catch_vec$catch == 0, 1, lfq_catch_vec$catch)
res_cc <- catchCurve(
  lfq_catch_vec, 
  reg_int = reg_ints[[paste(currperiod, scn, sep='-')]],
  calc_ogive = F,
  plot = F)

# Visualizing
res_cc$Z
plot(res_cc)

# ==== Estimating mortalities specific to this period ====

# Natural mortality (always shared)
curr_m <- as.numeric(growth$m_pauly)

# Choosing the total mortality to use
if(useOverallZM){
  print("Using a shared overall Z")
  curr_z <- growth$cc$Z
}else{
  print("Using the Z estimated specifically for this period")
  curr_z <- res_cc$Z
}
# Fishing mortality
curr_f <- as.numeric(curr_z - curr_m)
# Exploitation rate
curr_e <- curr_f/curr_z

# Printing for review
# print("Current total mortality rate is:")
# print(curr_z)
# print("Current natural mortality rate is:")
# print(curr_m)
# print("Current overall fishing mortality rate is:")
# print(curr_f)
print("Current exploitation rate is:")
print(curr_e)

# Saving them as a list
mortalities <- list('z' = curr_z, 'm' = curr_m, 'f'=curr_f, 'e'=curr_e)

# Getting other required input parameters ----------

# Length-weight a/b parameters (from Fishbase) a_fb <-  0.0269 b_fb <- 2.85
# Fitting a log length-weight relationship to the overall data (coefficients are
# thus the same between the 2 periods)
lw_model <- (lm(log(peso)~log(longitud), data = fdb))
lw <- list('a'=exp(coef(lw_model)[1]), 'b'= coef(lw_model)[2])

# Assign length-weight parameters to the data list
lfq_catch_vec$par$a <-  lw$a
lfq_catch_vec$par$b <- lw$b

# Assumgin a terminal F for the plus group as 10% of the estimated overall
# fishing mortality (relatively very low exploitation)
termF <- round(mortalities$f*0.10, 3)
# Virtual-population analysis - to get the F-per-length array
lfq_catch_vec$par$M <- mortalities$m
vpa_res <- VPA(param = lfq_catch_vec, terminalF = termF,
               analysis_type = "VPA", catch_unit = "'000",
               plot=TRUE, algorithm = 'new')

# Calculating an alternative F-array based on the theoretical cumulative
# gear-selection pattern
selprobs <- setNames(cum_sel_tab$cum_sel, cum_sel_tab$long_bin)
selprobs <- selprobs[names(selprobs) %in% lfq_catch_vec$midLengths]

# Number of recruits to use
num_recruits <- 1000

# ==== Yield-per-recruit ====

# Getting results from the thompson and bell length-based YPR
ypr_input <- lfq_catch_vec
ypr_input$par$Z <- mortalities$z

# VPA-base F-array
ypr_input$par$FM <- vpa_res$FM_calc
# Fitting VPA-based model
TB1 <- predict_mod(ypr_input, type = "ThompBell",
                   FM_change = seq(0,10,0.05),
                   stock_size_1 = num_recruits,
                   curr.E = curr_e,
                   plot = T, hide.progressbar = TRUE)

# Theoretical F-array based on cumulative gear selectivity
ypr_input$par$FM <- selprobs*mortalities$f
# Fitting gear-selectivity based model
TB2 <- predict_mod(ypr_input, type = "ThompBell",
                   FM_change = seq(0,10,0.05),
                   stock_size_1 = num_recruits,
                   curr.E = curr_e,
                   plot = T, hide.progressbar = TRUE)

# ==== Exporting results ====

# Building identifier string
zm_id <- if(useOverallZM) 'sharedz' else 'sep_z'
sel_id <- if(useSelCor) 'selcor' else ''
fpath <- paste('calale_ypr', currperiod, scn, zm_id, sel_id, sep='_')
fpath

# Exporting results
saveobjs <- list(
  'cdb' = cdb,
  'rel_power' = rel_power,
  'seltable' = cum_sel_tab,
  'corCatchPlot' = corCatchPlot,
  'selCurvePlot' = selCurvePlot,
  'lfqCorMosaic' = lfqCorMosaic,
  'base_lfq' = lfq,
  'cc' = res_cc,
  'lfq_vec' = lfq_catch_vec,
  'vpa_term_f' = termF,
  'vpa' = vpa_res,
  'lw_params' = lw,
  'mortalities' = mortalities,
  'num_recruits' = num_recruits,
  'ypr_vpa' = TB1,
  'ypr_sel' = TB2
)
saveRDS(saveobjs, file.path(out_dir, paste0(fpath, '.rds')))

