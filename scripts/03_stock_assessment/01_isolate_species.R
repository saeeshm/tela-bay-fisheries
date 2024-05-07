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
         'nombre_cientifico', 'peso', 'longitud', 'horas_pesca', 'numero_pescadores',
         'horas_pesca_intp','salida_intp', 'regreso_intp',
         'tipo_arte', 'luz_malla', 'longitud_total_arte')
  
# ==== Reading data ====
tela <- read_csv(tela_path)
trujillo <- read_csv(trujillo_path)

# ==== Examining overall sampling variability ====
tela |> 
  filter((str_detect(genus, gen) & str_detect(species, spc)) |
           str_detect(nombre_comun_cln, cmn)) |>
  mutate(year = year(fecha), month=month.abb[month(fecha)]) |> 
  group_by(year, tipo_arte) |> 
  summarize('n'=length(unique(codigo, na.rm=T))) |> 
  tidyr::pivot_wider(names_from='tipo_arte', values_from='n')

tela |> 
  # filter(comunidad=='Miami') |> 
  filter((str_detect(genus, gen) & str_detect(species, spc)) |
           str_detect(nombre_comun_cln, cmn)) |>
  mutate(year = year(fecha), month=month.abb[month(fecha)]) |> 
  select(year, 'var' = tipo_arte, codigo) |> 
  group_by(year, var) |> 
  summarize('n'=length(unique(codigo, na.rm=T))) |> 
  tidyr::pivot_wider(names_from='var', values_from='n')
  # table()

trujillo |> 
  filter((str_detect(genus, gen) & str_detect(species, spc)) |
           str_detect(nombre_comun_cln, cmn)) |> 
  mutate(year = year(fecha), month=month.abb[month(fecha)]) |> 
  group_by(year, month) |> 
  summarize('n'=length(unique(codigo, na.rm=T))) |> 
  tidyr::pivot_wider(names_from='month', values_from='n') |> 
  relocate(all_of(c('year', month.abb)))

# ==== Filtering only for the species of interest ====
d1 <- tela |> 
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

# ==== Writing to disk ====
write_csv(fdb, file.path(out_dir, 'lane_snapper_total.csv'))

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

# Viewing length distributions
fdb$longitud |> range(na.rm=T)
hist(fdb$longitud, breaks=40)

# Split by year
fdb |> 
  ggplot(aes(x = longitud)) +
  geom_histogram() +
  facet_wrap(facets='year', scales = 'fixed', nrow=4)

