# Author: Saeesh Mangwani
# Date: 2023-11-17

# Description: Basic EDA and description plots for the full landings TS dataset

# ==== Libraries ====
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)

# ==== Paths and global variables ====

# Path to the cleaned landings timeseries dataset
ts_path <- 'data/landings_full_ts.csv'

# Path to output directory for eda plots
out_dir <- 'output/eda'

# Helper function that creates a comlete-timeseries dataset given a range of
# dates and a time-step unit
complete_ts_df <- function(drange, unit, datecol = 'fecha', floor = T){
  mindate <- if(floor) floor_date(min(drange), unit=unit) else min(drange)
  maxdate <- if(floor) floor_date(max(drange), unit=unit) else max(drange)
  # Creating a full ts with the required timestep
  outdf <- tibble('dates' = seq(mindate, maxdate, by = unit))
  # Setting the date column name
  names(outdf) <- datecol
  return(outdf)
}

# A function that given a complete timeseries (just date range), replicates all
# levels of a specified factor for each date (factors specified as a vector)
rep_factor_by_date <- function(dtab, factlevs, colname){
  dtab %>% 
    bind_cols(setNames(rep(list(NA), length(factlevs)), factlevs)) %>% 
    pivot_longer(
      cols=all_of(factlevs),
      names_to = colname, 
      values_to = 'value'
    ) %>% 
    select(-value)
}

# Plotting theme
qtheme <- function(size=12, font='serif'){
  theme_minimal(size, font) +
    theme(plot.background = element_rect(fill='white', colour=NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
}

# ==== Reading data ====

# Master-timeseries dataset (specifying column types)
ldg <- read_csv(ts_path, col_types = paste0(
  'cccc', 'Dcic', 'ccnn','nicn',
  'cccn', 'nccc', 'cccn', 'nnnn',
  'nnni', 'iccn', 'ccnn', 'nnnn',
  'nnnn','nn'
))

# A vector with dates of year changes
yrange <- year(range(ldg$fecha))
yseq <- yrange[1]:yrange[2]
year_change <- ymd(paste0(yseq[-1], '-01-01'))

# ==== Basic descriptors ====

# What is the relative amount of fish catches
catch_dist <- ldg %>% 
  group_by(nombre_comun) %>% 
  summarize('n' = n()) %>% 
  arrange(desc(n))

# The 5 most common fisheries
esp_comun <- catch_dist %>% head(6) %>% pull(nombre_comun)

# ==== Global descriptives for main 6 species ====

# Preparing data ----------

# Getting a complete TS dataset for all factor levels being plotted
ref_ts <- ldg$fecha %>% 
  complete_ts_df(unit='month') %>% 
  rep_factor_by_date(factlevs = unique(ldg$nombre_comun), colname = 'nombre_comun')

# The plotting dataset - TS of catches and effort by month and species
pdf <- ldg %>% 
  # Extracting year-month from date
  mutate(yrmn = format_ISO8601(fecha, precision = 'ym')) %>% 
  group_by(yrmn, nombre_comun) %>% 
  # Getting total daily weight and number (per species)
  summarize(
    'fecha' = floor_date(min(fecha), unit='month'),
    'num_pescas' = n(),
    'sum_peso_g' = sum(peso, na.rm=T),
    'mean_peso_g' = mean(peso, na.rm=T),
    'sd_peso_g' = sd(peso, na.rm=T),
    'num_hours' = sum(horas_pesca, na.rm=T),
    'mean_hours' = mean(horas_pesca, na.rm=T),
    'sd_hours' = sd(horas_pesca, na.rm=T),
    'num_hour_obs' = sum(!is.na(horas_pesca)),
    'mean_length' = mean(longitud_horquilla, na.rm = T),
    'sd_length' = sd(longitud_horquilla, na.rm=T),
    'n_length_obs' = sum(!is.na(longitud_horquilla))
  ) %>% 
  ungroup() %>% 
  select(-yrmn) %>% 
  # Creating a complete TS dataframe for each date/species combination using the
  # helper functions
  right_join(ref_ts,by=c('fecha', 'nombre_comun')) %>% 
  # Filtering only the most commonly caught fish
  filter(nombre_comun %in% esp_comun) %>% 
  # Replacing NAs with 0s for the count and weight columns (a valid count for
  # dates where no activity occured)
  mutate(across(c('num_pescas', 'sum_peso_g', 'mean_peso_g','num_hours'), ~replace_na(.x, 0)))

# TS plots ----------

# TS of total caught weight (per month)
pdf %>% 
  ggplot() +
  geom_vline(xintercept=(year_change), 
             linewidth = 0.1, 
             linetype='dashed',
             colour='black',  alpha=0.7) +
  geom_line(aes(x = fecha, y = sum_peso_g, colour=nombre_comun), show.legend=F) +
  facet_wrap('nombre_comun') +
  qtheme() +
  labs(x = NULL, y = 'Total landed weight (grams)')
ggsave(file.path(out_dir, 'ts_main_6_total_weight_month.png'))

# TS of average caught weight (per month)
pdf %>% 
  ggplot() +
  geom_vline(xintercept=(year_change), 
             linewidth = 0.1, 
             linetype='dashed',
             colour='black',  alpha=0.7) +
  geom_line(aes(x = fecha, y = mean_peso_g, colour=nombre_comun), show.legend=F) +
  facet_wrap('nombre_comun') +
  qtheme() +
  labs(x = NULL, y = 'Mean landed weight (grams)')
ggsave(file.path(out_dir, 'ts_main_6_mean_weight_month.png'))

# Plotting caught number - not that useful, basically the same trend as above
pdf %>% 
  ggplot() +
  geom_vline(xintercept=(year_change), 
             linewidth = 0.1, 
             linetype='dashed',
             colour='black',  alpha=0.7) +
  geom_line(aes(x = fecha, y = num_pescas, colour=nombre_comun), show.legend=F) +
  facet_wrap('nombre_comun') +
  qtheme() +
  labs(x = NULL, y = 'Number of fish landed')

# TS of total hours of fishing effort (per month)
pdf %>% 
  ggplot() +
  geom_vline(xintercept=(year_change), 
             linewidth = 0.1, 
             linetype='dashed',
             colour='black',  alpha=0.7) +
  geom_line(aes(x = fecha, y = num_hours, colour=nombre_comun), show.legend=F) +
  facet_wrap('nombre_comun') +
  qtheme() +
  labs(x = NULL, y = 'Number of fishing hours')
ggsave(file.path(out_dir, 'ts_main_6_hours_month.png'))

# Weight/number correlation in the monthly-aggregated dataset
pdf %>% 
  ggplot(aes(y = sum_peso_g, x = num_pescas, colour=nombre_comun)) +
  geom_point(show.legend=F, alpha=0.5) +
  geom_smooth(method = 'lm', colour='black', linetype='dashed', linewidth=0.2) +
  facet_wrap('nombre_comun', scales='free') +
  qtheme() +
  labs(y = 'Total landed weight (grams)', x = 'Number of fish') 
ggsave(file.path(out_dir, 'monthly_weight_number_correlation.png'))

# TS of CPUE (per month) - using number of fish, not ideal but the more complete
# of the 2
pdf %>% 
  mutate(cpue = num_pescas/num_hours) %>% 
  mutate(cpue = ifelse(is.infinite(cpue), NA, cpue)) %>% 
  View()
  ggplot() +
  geom_vline(xintercept=(year_change), 
             linewidth = 0.1, 
             linetype='dashed',
             colour='black',  alpha=0.7) +
  geom_line(aes(x = fecha, y = cpue, colour=nombre_comun), show.legend=F) +
  facet_wrap('nombre_comun') +
  qtheme() +
  labs(x = NULL, y = 'CPUE (Number of fish per hour)')
ggsave(file.path(out_dir, 'ts_main_6_cpue_month.png'))

# TS of mean-length per species
pdf %>% 
  ggplot() +
  geom_vline(xintercept=(year_change), 
             linewidth = 0.1, 
             linetype='dashed',
             colour='black',  alpha=0.7) +
  geom_line(aes(x = fecha, y = mean_length, colour=nombre_comun), show.legend=F) +
  # geom_line(aes(x = fecha, y = mean_length, colour=nombre_comun), show.legend=F) +
  facet_wrap('nombre_comun') +
  qtheme() +
  labs(x = NULL, y = 'Mean fork length (cm)')
ggsave(file.path(out_dir, 'ts_main_6_mean_length_month.png'))

# Length-weight correlation
ldg %>% 
  filter(nombre_comun %in% esp_comun) %>% 
  select(nombre_comun, peso, longitud_horquilla) %>% 
  ggplot(aes(x = peso, y = longitud_horquilla, colour=nombre_comun)) +
  geom_point(alpha=0.4, show.legend = F) +
  geom_smooth(method = 'lm', colour='black', linetype='dashed', linewidth=0.2) +
  facet_wrap('nombre_comun', scales = 'free') +
  qtheme() +
  labs(x = 'Landed weight (grams)',
       y = 'Fork length (cm)')
ggsave(file.path(out_dir, 'main_6_length_weight_corr.png'))

