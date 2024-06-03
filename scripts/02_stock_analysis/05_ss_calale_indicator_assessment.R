# Author: Saeesh Mangwani
# Date: 2024-05-07

# Description: Single-species assessment plots of the calale fishery

# ==== libraries ====
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(ggpmisc)
library(ggplot2)
library(purrr)
# library(TropFishR)
library(anomalize)
source('scripts/02_stock_analysis/utils/_general_utils.R')
source('scripts/02_stock_analysis/utils/05_ss_indicator_utils.R')

# ==== Paths and global variables ====

# RDS file containing results from the VBGF fitting (contains info for the
# data-estimated Linf) - using the full data for this
vbgf_res_path <- paste0('data/stock_analysis/ypr_calale/vbgf_mesh.rds')
# Calale database path
fdb_path <- 'data/stock_analysis/calale_sa_raw.csv'
# Calale LFQ-adjusted data path
lfq_adj_path <-  'data/stock_analysis/calale_lfq_adjust.csv'

# Output directory
out_dir <- 'output/stock_analysis/ss_calale/indicator'

# ==== Reading data ====
growth <- readRDS(vbgf_res_path)
fdb_og <- read_csv(fdb_path)
lfqAdj_og <- read_csv(lfq_adj_path)

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
    tipo_arte == 'Gillnet - 2in' ~ 2,
    tipo_arte == 'Gillnet - 3in' ~ 3,
    T ~ NA
  )) |> 
  # Adding a period variable to designate before and after periods
  mutate(period = case_when(
    year %in% 2015:2018 ~ 'Before',
    year %in% 2019:2023 ~ 'After',
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
    tipo_arte == 'Gillnet - 2in' ~ 2,
    tipo_arte == 'Gillnet - 3in' ~ 3,
    T ~ NA
  )) |> 
  # Adding a period variable to designate before and after periods
  mutate(period = case_when(
    year %in% 2015:2018 ~ 'Before',
    year %in% 2019:2023 ~ 'After',
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

# Removing outliers
lfqAdj <- lfqAdj |> 
  filter(!long_bin %in% outLens)
fdb <- fdb |> 
  filter(!long_bin %in% outLens)

# ==== Calculating Froese indicators ====

# Starting Linf - from the fitted growth function
Linf_est <- growth$elefan_ga$par$Linf
# From Froese and Binholan's empirical equation using the max length in the data
Linf_emp <- exp(0.044 + 0.9841*log(max(fdb$longitud, na.rm=T)))

# Calculating indicators
# froese <- calc_froese_indics(Linf = Linf_est)
froese <- calc_froese_indics(Linf = Linf_est)

# ==== Length-Frequency ====

# Length-frequencies by gear against Froese indicators ----------

# Plot settings
ylim_max <- 400
labelSize <- 3
labelFont <- 'sans'

lfqByGear <- lfqAdj |> 
  filter(tipo_arte != "Gillnet - Unknown") |> 
  mutate(period=factor(period, levels=c('Before', 'After'))) |> 
  mutate(tipo_arte = factor(tipo_arte, levels = c(
    "Handline", "Gillnet - 2in", "Gillnet - 3in")
  )) |> 
  ggplot() +
  geom_bar(aes(x=long_bin, weight=adjCount), 
           fill='white',
           colour='black',
           alpha=0.8,
           linewidth=0.4,
           show.legend=F) +
  scale_fill_brewer(palette = 'Dark2') +
  # Setting scale limits for clean viewing
  coord_cartesian(ylim=c(0, ylim_max+50), 
                  xlim=c(
                    floor(min(lfqAdj$long_bin))-1, 
                    ceiling(froese$LMspawn)+10
                  )) +
  scale_y_continuous(expand = c(0, 1)) +
  # Optimal length range for maximum yield
  ggplot2::annotate('rect', 
           xmin = froese$opt_range[1], xmax = froese$opt_range[2], 
           ymin = -1, 
           ymax = ylim_max+100,
           fill='darkgreen',
           alpha=0.2) +
  # Minimum catch length
  geom_vline(xintercept = 23, 
             linetype='dashed', colour='firebrick') +
  ggplot2::annotate('text', 
           # label='L-Min',
           label='1.',
           x=23, 
           y=ylim_max,
           fontface = 'bold',
           family=labelFont,
           hjust=-0.8,
           # hjust=1.2,
           size=labelSize) +
  # Length-of maturity
  geom_vline(xintercept = froese$Lmat, linetype='dashed') +
  ggplot2::annotate('text', 
           label='2.',
           x=froese$Lmat, 
           y=ylim_max,
           fontface = 'bold',
           family=labelFont,
           hjust=-0.8,
           # hjust=1.2,
           size=labelSize) +
  # Length of mega-spawners
  geom_vline(xintercept = froese$LMspawn, 
             linetype='dashed', colour='darkorange') +
  ggplot2::annotate('text', 
           label='3.',
           # label='L-Mspawn',
           family=labelFont,
           fontface = 'bold',
           x=froese$LMspawn, 
           y=ylim_max,
           hjust=-0.8,
           # hjust=1.2,
           size=labelSize) +
  qtheme() +
  facet_grid(rows=vars(tipo_arte), cols=vars(period), scales = 'fixed', drop=F) +
  labs(
    y = 'Count of individuals',
    x = 'Length (cm)'
  )
lfqByGear |> 
  myggsave(file.path(out_dir, 'calale_lfq_froese_by_gear.png'), w=7, h=5, scale = 1)

# Numerical summaries for reporting ----------

# Total caught weight in each year
lfqByGear$data |> 
  filter(tipo_arte == 'Gillnet - 3in') |> 
  filter(period=='Before') |> 
  summarize('wt'=sum(adjWt)) |> pull(wt)

# Average size of capture
lfqByGear$data |> 
  group_by(period) |> 
  summarize('mlen' = weighted.mean(long_bin, adjWt))

# ==== CPUE ====

# Plotting dataframe ----------
cpueDat <- lfqAdj |> 
  filter(tipo_arte != 'Gillnet - Unknown') |> 
  mutate(tipo_arte = factor(tipo_arte, 
                            levels = c('Chinchorro', 'Handline', 
                                       'Gillnet - 2in', 'Gillnet - 3in'))) |> 
  mutate(cpue = adjWt/tot_boats) |> 
  mutate(fecha = ymd(paste(year, month, '01', sep='-'))) |>
  # Summarizing total catches by trip, per-gear
  group_by(fecha, tipo_arte) |> 
  summarize(
    # tot_boats = unique(tot_boats),
    peso = sum(adjWt, na.rm=T),
    cpue = mean(cpue, na.rm=T)
  ) |> 
  ungroup() |> 
  # mutate(cpue = peso/tot_boats) |> 
  mutate(year = year(fecha)) |>
  # Adding identifiers for the before-after periods
  mutate(period = case_when(
    year %in% 2015:2018 ~ 'Before',
    year %in% 2019:2023 ~ 'After',
    T ~ NA
  ))

# CPUE time-trend per gear ----------
tsCpueGear <- cpueDat |> 
  mutate(period=factor(period, levels=c('Before', 'After'))) |> 
  mutate(tipo_arte = factor(tipo_arte, levels = c(
    "Gillnet - Unknown", "Handline", 
    "Gillnet - 2in", "Gillnet - 3in")
  )) |> 
  # filter(period == 'before') |> 
  # filter(!is.na(period)) |> 
  ggplot(aes(x = fecha, y=cpue)) +
  geom_point(alpha=0.4, show.legend = F) +
  geom_line(alpha=0.8, show.legend = F) +
  # stat_poly_line() +
  # stat_poly_eq(use_label(c("P", "R2"))) +
  scale_y_continuous(limits = c(0, 3)) +
  facet_wrap('tipo_arte', nrow=3) +
  qtheme() +
  labs(
    x = NULL,
    y = 'CPUE (kg/day)'
  )
tsCpueGear |> 
  myggsave(file.path(out_dir, 'ts_cpue_by_gear.png'), w=7, h=5)

# Trend in overall CPUE over time ----------
overallCPUE <- cpueDat |> 
  ggplot(aes(x = fecha, y=cpue)) +
  geom_point(alpha=0.4, show.legend = F) +
  geom_line(alpha=0.8, show.legend = F) +
  scale_y_continuous(limits = c(0, 5)) +
  geom_smooth(method='lm', colour='firebrick', linewidth=0.6) +
  ggpmisc::stat_poly_eq(use_label(c("P", "R2"))) +
  # facet_wrap('tipo_arte', nrow=2) +
  qtheme() +
  labs(
    x = NULL,
    y = 'CPUE (kg/day)'
  )
overallCPUE |> 
  myggsave(file.path(out_dir, 'overall_cpue_trend.png'))

# Supplementary analysis: CPUE per length class per period ----------

# Preparing dataframe for CPUE per length class per period
cpue_pl_tab <- lfqAdj |> 
  mutate(period = factor(period, levels=c('Before', 'After'))) |> 
  filter(tipo_arte != 'Gillnet - Unknown') |> 
  mutate(tipo_arte = factor(tipo_arte, 
                            levels = c('Chinchorro', 'Handline', 
                                       'Gillnet - 2in', 'Gillnet - 3in'))) |> 
  mutate(cpue = adjWt/tot_boats) |> 
  mutate(fecha = ymd(paste(year, month, '01', sep='-'))) |>
  # Summarizing total catches by trip, per-gear
  group_by(period, long_bin) |> 
  summarize(
    # tot_boats = unique(tot_boats),
    peso = sum(adjWt, na.rm=T),
    mn_cpue = mean(cpue, na.rm=T),
    sd_cpue = sd(cpue, na.rm=T),
    n_cpue = length(!is.na(cpue))
  ) |> 
  ungroup() |> 
  mutate(se = sd_cpue/sqrt(n_cpue))

# Getting the overall mean cpue per period
pp_cpue <- cpue_pl_tab |> 
  group_by(period) |> 
  summarize(
    mn = mean(mn_cpue),
    sd = sd(mn_cpue),
    n = length(!is.na(mn_cpue))
  ) |> 
  mutate(se = sd/sqrt(n)) |> 
  mutate(cimin = mn - 1.96*se, cimax = mn + 1.96*se)

# Plotting Mean CPUE per length class + overall CPUE per period overlaid
cpuePLPlot <- cpue_pl_tab |> 
  left_join(pp_cpue |> dplyr::select(period, cimin, cimax), 
            by='period') |> 
  ggplot(aes(x = long_bin, y = mn_cpue, colour=period)) +
  geom_hline(data=pp_cpue, aes(yintercept = mn, colour=period),
             linetype='dashed') +
  geom_ribbon(
    aes(
      ymin=cimin, ymax=cimax, 
      fill=period,
      colour=NULL,
    ), 
    alpha=0.2
  ) +
  scale_colour_manual(values=c('darkorange', '#008080')) +
  scale_fill_manual(values=c('darkorange', '#008080')) +
  geom_point() +
  geom_errorbar(aes(ymin = (mn_cpue-1.96*se), ymax=(mn_cpue+1.96*se)), width=0.2) +
  geom_line() +
  # facet_wrap('period', nrow=2) +
  qtheme() +
  theme(legend.position = 'top') +
  labs(
    x = 'Length',
    y = 'CPUE (kg/day)',
    colour = "Period:",
    fill = 'Period:'
  )
cpuePLPlot |> 
  myggsave(file.path(out_dir, 'cpue_per_length_class.png'))

# ==== Extra/misc analyses (not used) ====

# Distribution of hours fished for gillnets
fdb |> 
  filter(str_detect(tipo_arte, 'Gillnet')) |> 
  mutate(isanom = anomalize::iqr(horas_pesca)) |> 
  filter(isanom == 'No') |> 
  ggplot(aes(x = horas_pesca)) +
  # geom_point() +s
  geom_histogram() +
  geom_vline(xintercept = 5) +
  facet_wrap('period')

# Mean number of hours per period
fdb |> 
  filter(str_detect(tipo_arte, 'Gillnet')) |> 
  mutate(isanom = anomalize::iqr(horas_pesca)) |> 
  filter(isanom == 'No') |>
  group_by(period) |> 
  mutate(uthresh = horas_pesca <= 5) |> 
  summarize(mn = mean(horas_pesca, na.rm=T),
            nobs = sum(!is.na(horas_pesca)),
            prop_u7 = sum(uthresh)/nobs)


    
