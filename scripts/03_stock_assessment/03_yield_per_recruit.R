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
source('scripts/03_stock_assessment/_02_ypr_utils.R')

# ==== Paths and global variables ====

# Stock database path
fdb_path <- 'data/stock_assessment/lane_snapper_total.csv'
# fdb_day_path <- 'data/stock_assessment/lane_snapper_daily.csv'
# fdb_iwk_path <- 'data/stock_assessment/lane_snapper_isoweek.csv'

# Selectivity and growth models
selgrowth_path <- 'data/stock_assessment/growth_selectivity.rds'

# Output directory to store YPR results
out_dir <- 'output/stock_assessment/ypr/annual_tables_selcor'
# out_dir <- 'output/stock_assessment/ypr/annual_tables_noselcor'

# ==== Reading data ====
fdb_og <- read_csv(fdb_path)
sg <- readRDS(selgrowth_path)

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

# Length range
hist(fdb$longitud, breaks='Sturges', freq=T)
range(fdb$longitud)
# Not a great amount of samples per year
table(fdb |> dplyr::select(year, month))

# Splitting datasets by year
yds <- fdb |> 
  group_by(year) |> 
  group_split()
names(yds) <- unique(fdb$year)

# ==== Initial parameters - year, binsize, gear relative-power ====

# Current year whose data to analyze
curryear <- '2021'

# Getting data for the current year
cdb <- yds[[curryear]]

# Setting a binsize - using the most common bin size across all the datasets
# based on Sturges' rule (want to keep the bin size consistent across all the
# datasets)
sturge_bins <- function(xmax, xmin, n){
  round((xmax-xmin) / (1 + 1.44*log(n)), 0)
}
# Which is the most common binsize?
map_int(yds, \(x){
  sturge_bins(max(x$longitud), min(x$longitud), nrow(x))
}) |> table()
# Setting that for all years
binsize <- 2

# Getting the relative fishing intensity of the gears
table(cdb$luz_malla, useNA = 'ifany')
mesh_hours <- cdb |> 
  filter(!is.na(luz_malla)) |> 
  dplyr::select(fecha, horas_pesca, luz_malla) |> 
  group_by(fecha) |> 
  distinct() |> 
  group_by(luz_malla) |> 
  summarize(eff = sum(horas_pesca, na.rm=T))
rel_power <- (mesh_hours$eff/sum(mesh_hours$eff)) |> 
  round(3) |> 
  setNames(mesh_hours$luz_malla)

# Vectors storing the relative fishing intensity used for defining the
# selectivity curve for each year. This is based on the mix of gears used. For
# 2017-2019 no mesh information was available, so I defaulted to the pattern of
# 2016, as there was evidence for the presence of 2mm mesh sizes in the catch
# distribution)
# 2016-2019:
if(curryear %in% c(2016:2019)){
  rel_power <- setNames(c(0.209, 0.791), c(2, 3))
}else{
  rel_power <- setNames(c(0, 1), c(2, 3))
}
rel_power
# rel_power <- setNames(c(0.209, 0.791), c(2, 3))
# 2020-2023:
# rel_power <- setNames(c(0, 1), c(2, 3))

# ==== Constructing basic length-frequency dataset ====

# Aggregating data by month, and summarizing across years to create a
# representative year dataset (MAKES THE ASSUMPTION THAT THE GROWTH PATTERN IS
# CONSTANT ACROSS YEARS)
range(cdb$longitud)
hist(cdb$longitud)
lfq <- cdb |> 
  # Keeping only relevant columns
  lfqCreate(Lname = 'longitud', Dname = 'fecha', 
            bin_size = binsize, Lmin = floor(range(cdb$longitud)[1]),
            aggregate_dates = T, plot=T)

# Getting the estimated selection probabilty for each length class, using the
# global selectivity function, corrected for by the relative fishing intensity
# of this year
seltab <- tibble('Length' = lfq$midLengths) |> 
  # Selectivity for mesh size 2
  mutate(sel_2 = sg$est_selectivity(
    Length, meshsize=2,
    smdl = sg$selectivity_model, 
    rel_power = rel_power['2'])
  ) |> 
  # For mesh-size 3
  mutate(sel_3 = sg$est_selectivity(
    Length, meshsize=3,
    smdl = sg$selectivity_model, 
    rel_power = rel_power['3'])
  ) |> 
  # Cumulative selectivity
  rowwise() |> 
  mutate(cum_sel = sum(c_across(!Length), na.rm=T)) %>%
  mutate(cum_sel = round(cum_sel/max(.$cum_sel), 3))

# Plotting - using the helper function that was loaded with the
# selectivity/growth information
selplot <- seltab |>
  setNames(c('Length', '2 cm', '3 cm', 'Cumulative')) |> 
  tidyr::pivot_longer(cols=!Length, names_to='curve_type', values_to='val') |>
  sg$plot_sel_curve(cum_col_name = 'Cumulative') +
  xlim(c(min(seltab$Length)-1, max(seltab$Length)+1))
selplot

# Classifying lengths into classes using the provided bin size (ranges defined
# manually based on range of the length data)
range(lfq$midLengths)

# Getting cumulative-selectivity scaling factors for each midlength
selprob <- seltab$cum_sel |> setNames(seltab$Length)

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
plot(x=lfq, Fname = "catch", date.axis = "modern", ylim = c(6, 44))
plot(x=lfq_cor, Fname = "catch", date.axis = "modern", ylim = c(6, 44))

# Creating a database for making a presentation plot for corrected catch vs raw
make_lfq_plotdat <- function(lfqobj, type){
  catches <- lfqobj$catch |> 
    as.data.frame() |> 
    rowSums()
  tibble('length' = lfq$midLengths, 'catch' = catches, 'type'=type)
}
lfqPlotDf <- make_lfq_plotdat(lfq, 'Raw') |> 
  bind_rows(make_lfq_plotdat(lfq_cor, 'Corrected'))

# Presentation plot
corr_catch_plot <- lfqPlotDf |> 
  ggplot(aes(x = length, y=catch, alpha=type, fill=type)) +
  geom_col(colour='black', fill='firebrick', position='identity') +
  scale_alpha_manual(values=c(0.4, 0.9)) +
  theme_minimal(12, 'serif') +
  xlim(c(min(lfqPlotDf$length)-1, max(lfqPlotDf$length)+1)) +
  labs(y = 'Catch (numbers)',
       x = NULL,
       alpha='Frequency')

# Viewing the corrected catch over the selection curve
(corr_catch_plot/selplot)

# ==== Estimating Z using the catch-curve ====

# Catch-curve analysis for total mortality ----------

# Adding VBGF growth parameters to the length-frequency object
# lfq_cc <- lfqModify(lfq, par = sg$elefan_sa_result$par)
lfq_cc <- lfqModify(lfq_cor, par = sg$elefan_sa_result$par)

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
  '2016' = c(6, 10),
  '2017' = c(6, 12),
  '2018' = c(10, 16),
  '2019' = c(3, 12),
  '2020' = c(5, 7),
  '2021' = c(8, 12),
  '2022' = c(4, 8),
  '2023' = c(3, 9)
)

#run catch curve (intervals defined after an initial interactive review
#lfq_catch_vec$catch <- ifelse(lfq_catch_vec$catch == 0, 1, lfq_catch_vec$catch)
res_cc <- catchCurve(
  lfq_catch_vec, 
  reg_int = reg_ints[[curryear]],
  calc_ogive = F,
  plot = F)

# Visualizing
res_cc$Z
plot(res_cc)

# Empirical estimates of natural mortality ----------

# Then 2015 equation
M_then <- M_empirical(
  Linf = sg$elefan_sa_result$par$Linf, 
  K_l = sg$elefan_sa_result$par$K, 
  method = "Then_growth")
M_then

# Pauly method
M_pauly <- M_empirical(
  Linf = sg$elefan_sa_result$par$Linf, 
  K_l = sg$elefan_sa_result$par$K, 
  temp=28.3, 
  method = "Pauly_Linf", 
  schooling = F)
M_pauly

# Fish-base derived natural mortality
# lfq_res$par$M <- as.numeric(0.21)

# Getting other required input parameters ----------

# Length-weight a/b parameters (from Fishbase)
a_fb <-  0.0269
b_fb <- 2.85
# From the length-weight logged relationship in this year's data
lw_model <- (lm(log(peso)~log(longitud), data = fdb))
a_mod <- exp(coef(lw_model)[1])
b_mod <- (coef(lw_model)[2])
a_mod
b_mod
# assign length-weight parameters to the data list (from Fishbase)
lfq_catch_vec$par$Z <- res_cc$Z
lfq_catch_vec$par$a <-  a_mod
lfq_catch_vec$par$b <- b_mod

# Calculating terminal F-probabilities for the plus group based on each M
# estimator
termF_pauly <- as.numeric(res_cc$Z - M_pauly) * selprob[as.character(plus_group)]
termF_then <- as.numeric(res_cc$Z - M_then) * selprob[as.character(plus_group)]

# Virtual-population analysis
lfq_catch_vec$par$M <- M_pauly
vpa_pauly <- VPA(param = lfq_catch_vec, terminalF = 0.5,
               analysis_type = "VPA", catch_unit = "'000",
               plot=TRUE, algorithm = 'new')
lfq_catch_vec$par$M <- M_then
vpa_then <- VPA(param = lfq_catch_vec, terminalF = 0.5,
                 analysis_type = "VPA", catch_unit = "'000",
                 plot=TRUE, algorithm = 'new')

# ==== Yield-per-recruit ====

# Preparing function and inputs ----------

# Helper function that calculates a tabular YPR model given a set of input
# parameters:
# 1. A vector of length intervals
# 2. A list containing vbgf parameters labelled as 'Linf' and 'K'
# 3. A list containing length-weight parameters labelled as 'a' and 'b'
# 4. An input initial estimate of M (natural mortality)
# 5. An input initial estimate for F (fishing mortality)
# 6. An optional vector of selectivity/retention probabilities of the same
# length as the length-interval vector (if missing, all length classes are
# assumed to be fished at the same level of fishing mortality set equal to the F
# specified above)
calc_ypr <- function(lengths, vbgf, lw, f_scaling_factor, init_m, f_array, num_recruits=1000, full_tab=F){
  # Calculating relevant input parameters for each length class
  ypr_tab <- tibble('lengths' = lengths) |> 
    # Calculating delta_t
    mutate(delta_t = calc_delta_t(lengths, params=vbgf)) |> 
    # Fishing mortality per length class (the overall level of fishing scaled by
    # selectivity)
    mutate(fm = f_array*f_scaling_factor) |> 
    # Total mortality as the sum of FM and M
    mutate(z = fm+c(init_m)) |> 
    # Survival
    mutate(surv_rate = exp(-z*delta_t)) |> 
    mutate(surv_rate = ifelse(is.na(surv_rate), fm/z, surv_rate)) |> 
    # Exploitation rate/fishing effort
    mutate(expl_rate = fm/z) |> 
    # Number of individuals in the cohort (assuming constant recruitment of
    # 1000)
    mutate(prop_surviving_num = calc_num_cohort(surv_rate, num_recruits)) |> 
    # Number of deaths per age-class
    mutate(death_num = prop_surviving_num*(1-surv_rate)) |> 
    # Number of catch (deaths * the exploitation rate)
    mutate(catch_num = death_num*expl_rate) |> 
    # Mean number of individuals in each length class
    mutate(mean_num_per_lc = death_num/z) |> 
    # Average weight per length-interval (calculated on the age difference
    # between each length class and the next one)
    mutate(mwt = calc_mean_weight(lengths, lw$a, lw$b)) |> 
    # Mean biomass per length interval
    mutate(bms = prop_surviving_num*mwt) |> 
    # Mean yield/catch
    mutate(yield = catch_num*mwt)
  
  # Calculating summary totals of YPR for this set of inputs
  restab <- tibble(
    'FM_scaling' = f_scaling_factor,
    'catch_num' = sum(ypr_tab$catch_num, na.rm=T),
    'num_indivs' = sum(ypr_tab$mean_num_per_lc, na.rm=T),
    'biomass' = sum(ypr_tab$bms, na.rm=T),
    'yield' = sum(ypr_tab$yield, na.rm=T)
  ) |> 
    mutate(mean_wt_catch = yield/catch_num)
  if(full_tab){
    return(list('ypr_calc' = ypr_tab, 'res_tab' = restab))
  }
  return(restab)
}

# Preparing input data objects ----------

# Lengths
lengths <- lfq_catch_vec$midLengths
# VBGF parameters
vbgf <- sg$elefan_sa_result$par
# Length/weight parameters
lw <- list('a' = a_mod, 'b' = b_mod)
# lw <- list('a' = a_fb, 'b' = b_fb)
# Number of recruits
num_recruits <- 10
# Range of F-scaling factors to compare
f_seq <- seq(0, 10, by=0.05)
# Overall estimate of fishing mortality level (difference between Z and M) -
# used as the starting value for for fishing mortality, that is then scaled per
# length-class based on the selection curve
pauly_fm <- as.numeric(res_cc$Z - M_pauly)
then_fm <- as.numeric(res_cc$Z - M_then)

# Calculating YPR ----------

# Calculating YPR for all given input F-scaling factors, across all scenarios
ypr <- map_dfr(f_seq, \(f_scaling_factor){
  # F-array using the gear selection curve - Pauly M-estimator
  scn1 <- calc_ypr(
    lengths, vbgf, lw, f_scaling_factor, 
    init_m = M_pauly, 
    f_array = pauly_fm * (seltab |> 
                              filter(Length %in% lengths) |> 
                              pull(cum_sel)),
    num_recruits = num_recruits, full_tab = F) |> 
    mutate(scenario = 'gear_sel_pauly')
  # F-array using the gear selection curve - Then M-estimator
  scn2 <- calc_ypr(
    lengths, vbgf, lw, f_scaling_factor, 
    init_m = M_then, 
    f_array = then_fm * (seltab |> 
                              filter(Length %in% lengths) |> 
                              pull(cum_sel)),
    num_recruits = num_recruits, full_tab = F) |> 
    mutate(scenario = 'gear_sel_then')
  # F-array using the VPA selection curve - Pauly M-estimator
  scn3 <- calc_ypr(
    lengths, vbgf, lw, f_scaling_factor, 
    init_m = M_pauly, 
    f_array =  vpa_pauly$FM_calc,
    num_recruits = num_recruits, full_tab = F) |> 
    mutate(scenario = 'vpa_sel_pauly')
  # F-array using the VPA selection curve - Then M-estimator
  scn4 <- calc_ypr(
    lengths, vbgf, lw, f_scaling_factor, 
    init_m = M_then, 
    f_array =  vpa_then$FM_calc,
    num_recruits = num_recruits, full_tab = F) |> 
    mutate(scenario = 'vpa_sel_then')
  bind_rows(scn1, scn2, scn3, scn4)
})

# ==== Exporting results ====

# Exporting results
saveobjs <- list(
  'rel_power' = rel_power,
  'seltable' = seltab,
  'selplot' = selplot,
  'corrcatchplot' = corr_catch_plot,
  'catchcurve' = res_cc,
  'm_pauly' = M_pauly,
  'm_then' = M_then,
  'vpa_pauly' = vpa_pauly,
  'vpa_then' = vpa_then,
  'lw_params' = lw,
  'num_recruits' = num_recruits,
  'ypr' = ypr
)
saveRDS(saveobjs, file.path(out_dir, paste0('ypr_', curryear, '.rds')))

# Calculating extra parameters - %-unfished biomass and U-function
b0 <- ypr[1,]$biomass
ypr <- ypr |> 
  # % unfished-biomass
  mutate(perc_unfished_bms = biomass/b0) |> 
  # U-function (number of recruits = 1000) |> 
  mutate(u_01 = 0.1*(b0/num_recruits)*FM_scaling) |> 
  mutate(u_05 = 0.5*(b0/num_recruits)*FM_scaling)
  

# Extracting reference points as a separate dataframe
overall_fm <- as.numeric(res_cc$Z - M_pauly)
currypr <- ypr |> 
  filter(scenario == 'vpa_sel_pauly')
  
# Extracting reference points
curr_f <- currypr[which.min(abs(currypr$FM_scaling - overall_fm)), ]
fmax <- currypr[which.max(currypr$yield), ]
f_01 <- currypr[which.max(currypr$yield - currypr$u_01), ]
# f_05 <- currypr[which.max(currypr$yield - currypr$u_05), ]
# f_05
ref_pts <- bind_rows(curr_f, fmax, f_01) |> 
  mutate('type' = factor(c('F-current', 'F-MSY', 'F-0.1'), 
                         levels=c('F-current', 'F-MSY', 'F-0.1')))
# Plotting
p1 <- currypr |> 
  ggplot(aes(x=FM_scaling)) +
  geom_line(aes(y = yield), colour='darkgreen') +
  geom_point(data=ref_pts, aes(x=FM_scaling, y=yield, shape=type, colour=type),
             size=3.2, alpha=0.8) +
  scale_colour_manual(values=c('firebrick', 'orange', 'darkgreen')) +
  # geom_vline(xintercept = 2.54, linetype='dashed') +
  # geom_vline(xintercept = fmax$FM, colour='firebrick') +
  # geom_vline(xintercept = f_ref$FM, colour='firebrick') +
  # geom_line(aes(y = biomass), colour='darkorange') +
  # geom_line(aes(y = u_func), colour='darkgrey') +
  theme_minimal(12, 'serif') +
  labs(
    y='Yield/Recruit',
    shape='Reference Point',
    colour='Reference Point',
    x=NULL
  )

p2 <- currypr |> 
  ggplot(aes(x=FM_scaling)) +
  # geom_line(aes(y = yield), colour='darkgreen') +
  geom_line(aes(y = biomass), colour='blue3') +
  geom_point(data=ref_pts, aes(x=FM_scaling, y=biomass, shape=type, colour=type),
             size=3.2, alpha=0.8) +
  scale_colour_manual(values=c('firebrick', 'orange', 'darkgreen')) +
  # geom_vline(xintercept = curr_f$FM, linetype='dashed') +
  # geom_vline(xintercept = fmax$FM, colour='firebrick') +
  # geom_vline(xintercept = f_01$FM, colour='firebrick') +
  # geom_line(aes(y = u_func), colour='darkgrey') +
  theme_minimal(12, 'serif') +
  labs(
    y='Biomass/Recruit',
    shape='Reference Point',
    colour='Reference Point',
    x=NULL
  )
mosaic <- p1/p2 + plot_layout(guides = 'collect')
mosaic


