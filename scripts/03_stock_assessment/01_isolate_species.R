# Author: Saeesh Mangwani
# Date: 2024-03-23

# Description: A script for isolating landings t/s data for a given species

# ==== libraries ====
library(dplyr)
library(stringr)
library(lubridate)
library(readr)
library(ggplot2)

# ==== Paths and global variables ====

# Output path
out_dir <- 'data/stock_assessment'

# Database (path) - Tela Bay
tela_path <- 'data/dbase_tela_clean.csv'
trujillo_path <- 'data/dbase_trujillo_clean.csv'

# Scientific names
fam <- 'Lutjanidae'
gen <- 'Lutjanus'
spc <- 'synagris'

# Common name
cmn <- 'calale|lane'

# Index of essential columns (useful for viewing subsets of tables)
idx <- c('id', 'codigo', 'fecha', 'comunidad', 'zona_pesca', 'nombre_comun',
         'nombre_cientifico', 'peso', 'longitud', 'horas_pesca', 
         'horas_intp_dos', 'horas_intp_uno',
         'tipo_arte', 'luz_malla', 'longitud_total_arte')
  
# ==== Reading data ====
tela <- read_csv(tela_path)
trujillo <- read_csv(trujillo_path)

# ==== Filtering only for the species of interest ====
d1 <- tela |> 
  # Is the hour fished fully interpolated? Returning an error if yes
  mutate(horas_intp_dos = salida_intp & regreso_intp) |> 
  mutate(horas_intp_uno = salida_intp | regreso_intp) |> 
  # Filtering where the scientific name or common name matches
  filter((str_detect(genus, gen) & str_detect(species, spc)) |
           str_detect(nombre_comun_cln, cmn)) |> 
  # Removing data with missing dates (can't do anything with this)
  filter(!is.na(fecha)) |> 
  # Excluding data before 2016 due to erroneous and seriously disproportionate
  # sampling effort
  filter(fecha > ymd('2015-12-31')) |> 
  # Selecting only relevant columns
  dplyr::select(any_of(idx)) |> 
  mutate(region = 'Tela')

d2 <- trujillo |> 
  filter(ecosistema=='Bahia') |> 
  # Filtering where the scientific name or common name matches
  filter((str_detect(genus, gen) & str_detect(species, spc)) |
           str_detect(nombre_comun_cln, cmn)) |> 
  # Removing data with missing dates (can't do anything with this)
  filter(!is.na(fecha)) |> 
  # Excluding data before 2016 due to erroneous and seriously disproportionate
  # sampling effort
  filter(fecha > ymd('2015-12-31')) |> 
  # Selecting only relevant columns
  dplyr::select(any_of(idx)) |> 
  mutate(region = 'Trujillo')
  
fdb <- bind_rows(d1, d2) |> 
  # Adding year and month columns
  mutate(year = year(fecha)) |> 
  mutate(month = factor(month.abb[month(fecha)], levels=month.abb))

# ==== Summarizing catches and effort per-week and per-day ===

# The goal of this is to minimize the effect of sampling effort variability,
# while balancing this against ensuring there is enough granularity and contrast
# in the data. The problem stems from the fact that the sampling effort is
# wildly inconsistent each year, leading to biased estimates of catch and effor
# towards the years that just have more data. Averaging by week assigns a single
# value to each week, reducing the variability per year while still ensuring
# that the temporal unit is small enough to see seasonality patterns and detect
# contrasts in catches by year. This however makes the assumption that all
# samples in low-effort years are representative of the potential catch for that
# week, which may not necessarily be the case

# Visualizing sampling effort variability at the beginning - really pronounced
# for both regions
fdb |> 
  mutate(month=factor(month, levels=month.abb)) |>
  dplyr::select(year, month, region) |>
  table()

# Averaging catches, effort and lengths by day
fdb_day <- fdb |> 
  # Converting weight to kg
  mutate(peso = peso/1000) |> 
  # Getting the isoweek for each year
  # mutate(isoweek = isoweek(fecha), .after='fecha') |> 
  # Summarizing mean catch and mean effort by isoweek (helps account for the high
  # level of sampling variability distorting the estimate of total catch per
  # year)
  group_by(region, year, month, fecha) |>
  summarize(
    peso = mean(peso, na.rm=T),
    horas_pesca = mean(horas_pesca, na.rm=T),
    longitud = mean(longitud, na.rm=T),
    luz_malla = mean(luz_malla, na.rm=T),
    n_samples = n()
  ) |> 
  ungroup()

# Effort variability - reduced
fdb_day |> 
  mutate(month=factor(month, levels=month.abb)) |>
  dplyr::select(year, month, region) |>
  table()

# Averaging catches, effort and lengths by isoweek
fdb_iwk <- fdb |> 
  # Converting weight to kg
  mutate(peso = peso/1000) |> 
  # Getting the isoweek for each year
  mutate(isoweek = isoweek(fecha), .after='fecha') |>
  # Summarizing mean catch and mean effort by isoweek (helps account for the high
  # level of sampling variability distorting the estimate of total catch per
  # year)
  group_by(region, year, month, isoweek) |>
  summarize(
    peso = mean(peso, na.rm=T),
    horas_pesca = mean(horas_pesca, na.rm=T),
    longitud = mean(longitud, na.rm=T),
    luz_malla = mean(luz_malla, na.rm=T),
    n_samples = n()
  ) |> 
  ungroup()

# Effort variability - reduced even more
fdb_iwk |> 
  mutate(month=factor(month, levels=month.abb)) |>
  dplyr::select(year, month, region) |>
  table()

# ==== Writing to disk ====
write_csv(fdb, file.path(out_dir, 'lane_snapper_total.csv'))
write_csv(fdb_day, file.path(out_dir, 'lane_snapper_daily.csv'))
write_csv(fdb_iwk, file.path(out_dir, 'lane_snapper_isoweek.csv'))

# ==== Basic descriptives (to understand nature of the data) ====

# Temporal coverage of sampling effort
fdb$fecha |> range()

# No sampling every month - variability in sampling effort clear
fdb |> 
  # mutate(month = str_pad(month(fecha), side='left', pad='0', width=2)) |> 
  # mutate(myear = paste(month, year, sep='-')) |> 
  group_by(year, month) |> 
  summarize('n' = n()) |> 
  ggplot(aes(x = month, y = n)) +
  geom_col() +
  facet_wrap(facets='year', scales = 'fixed', nrow=4)

# Viewing length distributions per year
fdb$longitud |> range(na.rm=T)
hist(fdb$longitud, breaks=40)
# VERY different length distibution
hist(fdb_day$longitud, breaks=40)
hist(fdb_iwk$longitud, breaks=40)

# Split by year
fdb |> 
  ggplot(aes(x = longitud)) +
  geom_histogram() +
  facet_wrap(facets='year', scales = 'fixed', nrow=4)
fdb_day|> 
  ggplot(aes(x = longitud)) +
  geom_histogram() +
  facet_wrap(facets='year', scales = 'fixed', nrow=4)
fdb_iwk |> 
  ggplot(aes(x = longitud)) +
  geom_histogram() +
  facet_wrap(facets='year', scales = 'fixed', nrow=4)
