# Author: Saeesh Mangwani
# Date: 2024-05-08

# Description: Prepare the tela database for multispecies and single-species
# stock assessment

# ==== libraries ====
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# ==== Paths and global variables ====

# Database (path) - Tela Bay
tela_path <- 'data/dbase_tela_clean.csv'

# Output directory where prepared data files used for subsequent analysis will
# be stored
out_dir <- 'data/stock_analysis'

# ==== Reading data ====
tela_og <- read_csv(tela_path) |> 
  mutate(year = year(fecha)) |> 
  mutate(month = factor(month.abb[month(fecha)], levels=month.abb)) |> 
  # Weight to kg
  mutate(peso = peso/1000)

# Index of only relevant columns
colidx <- c(
  'year', 'month', 'fecha', 'codigo', 
  'peso', 'longitud', 
  'horas_pesca', 
  'tipo_arte', 'luz_malla',
  'nombre_comun_cln', 'nombre_cientifico',
  'class', 'order', 'family',
  'genus', 'species',
  'troph_diet', 'troph_food', 'troph_match_level',
  'numero_embarcaciones_salieron_pescar', 
  'numero_embarcaciones_muestreadas',
  'comunidad', 'zona_pesca'
)

# Index of queries used to filter data for Calale (used for single species
# assessment):

# Calale cientific names
fam <- 'Lutjanidae'
gen <- 'Lutjanus'
spc <- 'synagris'
# Calale common name query
cmn <- 'calale|lane'

# ==== Basic cleaning and reclassifications ====

# Cleaning filters
tela <- tela_og |> 
  # Removing spear fishing, only 19 observations
  filter(!tipo_arte %in% 'Arpon') |> 
  # Joining Nasa and trasmallo into a single net-type fishing activity
  mutate(tipo_arte = ifelse(tipo_arte == 'Nasa', 'Trasmallo', tipo_arte)) |> 
  # Reclassifying gear types for more informative names
  mutate(tipo_arte = case_when(
    tipo_arte == 'Trasmallo' & is.na(luz_malla) ~ 'Gillnet - Unknown',
    tipo_arte == 'Trasmallo' & luz_malla >= 3 ~ 'Gillnet - 3in',
    tipo_arte == 'Trasmallo' & luz_malla < 3 ~ 'Gillnet - 2in',
    tipo_arte == 'Linea De Mano' ~ 'Handline',
    T ~ tipo_arte
  )) |> 
  # Removing missing lengths
  filter(!is.na(longitud)) |> 
  # Removing missing weights
  filter(!is.na(peso)) |> 
  # Removing missing dates
  filter(!is.na(fecha)) |>
  # Removing missing gear info
  filter(!is.na(tipo_arte)) |> 
  # Only relevant columns
  dplyr::select(all_of(colidx)) |> 
  rename('nombre_comun' = nombre_comun_cln)

# ==== Creating a length/weight frequency table by date and species ====

# Classifying lengths into 2cm length bins
range(tela$longitud)
binBreaks <- seq(6, 160, 2)
binLabs <- seq(7, 159, 2)
tela <- tela |> 
  mutate(long_bin = cut(longitud, breaks = binBreaks, labels = binLabs), 
         .after=longitud) |> 
  # Converting length bin to numeric
  mutate(long_bin = as.numeric(as.character(long_bin)))

# Calculating average weight per species per length
lw_cwalk <- tela |> 
  group_by(nombre_cientifico, long_bin) |> 
  summarize(
    # 'count' = n(),
    'mean_wt' = mean(peso, na.rm=T)
  ) |> 
  ungroup()

# Where present, scaling the catches by the number of fishers left unfished in
# the day
lfqDat <- tela |> 
  mutate(no_muest = numero_embarcaciones_salieron_pescar - numero_embarcaciones_muestreadas) |> 
  # filter(fecha=='2021-08-30')
  # filter(numero_embarcaciones_muestreadas > 1) |> 
  # dplyr::select(year, no_muest) |> 
  # table()
  mutate(grpvar=fecha) |> 
  group_by(fecha) |> 
  group_modify(\(x, ...){
    print(unique(x$grpvar))
    # Gear-type for this trip (all trips seem to use only 1 gear, but one
    # erroneously reports 2 so dealing with it this way)
    arte <- names(table(x$tipo_arte)[which.max(table(x$tipo_arte))])
    # Number of boats unsampled
    nosamp <- na.omit(unique(x$no_muest))
    # If this information is unavailable, returning the table as-is
    if(length(nosamp)==0) {
      print(paste("No scaling data for date:", unique(x$grpvar)))
      nosamp <- 0
    }
    # print(nosamp)
    # Number of boats sampled today
    sampled <- length(unique(x$codigo))
    # Getting the CPUE for today partitioned proportionally into species/length
    # bins (i.e summed weight per length/species divided by the number of boats)
    spLfq <- x |> 
      group_by(nombre_cientifico, long_bin) |> 
      summarize(
        'count' = n(),
        'wt' = sum(peso),
        'cpue'=(wt/sampled),
        .groups='keep',
      ) |> 
      ungroup() |> 
      # Joining average weight information for each species/length
      left_join(lw_cwalk, by=c('nombre_cientifico', 'long_bin')) |> 
      # Based on this, we calculate the weight addition as the contributing
      # weight for each species/length multiplied by the number of unsampled
      # boats
      mutate(addWt = cpue*nosamp) |> 
      # And an estimated count addition as the weight addition divided by the
      # average weight of individuals of this species/length class
      mutate(addCount = round(addWt/mean_wt, 0)) |> 
      # Calculating adjusted weights and counts
      mutate(adjWt = wt+addWt, adjCount=count+addCount) |> 
      # Adding gear information
      mutate(tipo_arte = arte) |> 
      # Adding total number of fishers (including those that were unsampled -
      # used to calculate CPUE)
      mutate(samp_boats = sampled,
             tot_boats = sampled+nosamp) |> 
      dplyr::select(-cpue, -mean_wt)
  }) |> 
  ungroup()

# ==== Isolating lane snapper data from both datasets ====

# Lane snapper full data
calale_full <- tela |> 
  filter((str_detect(genus, gen) & str_detect(species, spc)) |
           str_detect(nombre_comun, cmn))
  
# Lane snapper length-frequency adjusted data
calale_lfq_adj <- lfqDat |> 
  filter((str_detect(nombre_cientifico, gen) & str_detect(nombre_cientifico, spc)))

# ==== Writing to disk ====

# Full prepared datasets
write_csv(tela, file.path(out_dir, 'tela_sa_raw.csv'))
write_csv(calale_full, file.path(out_dir, 'calale_sa_raw.csv'))

# LFQ-adjusted datasets
write_csv(lfqDat, file.path(out_dir, 'tela_lfq_adjust.csv'))
write_csv(calale_lfq_adj, file.path(out_dir, 'calale_lfq_adjust.csv'))

  

