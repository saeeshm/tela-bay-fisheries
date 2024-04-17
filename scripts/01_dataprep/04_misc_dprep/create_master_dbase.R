# Author: Saeesh Mangwani
# Date: 2024-04-16

# Description: Creating a joined master database merging cleaned landings data
# from all the North coast regions

# ==== libraries ====
library(readr)
library(dplyr)

# ==== Reading data ====
bi_og <- read_csv('data/dbase_bica_clean.csv')
tela_og <- read_csv('data/dbase_tela_clean.csv')
truj_og <- read_csv('data/dbase_trujillo_clean.csv')

# ==== Wrangling ====

# Cleaning column types or removing columns not relevant for joining with other
# landings datasets
bi <- bi_og |> 
  mutate(region = 'Islas de la Bahía', .before='uid') |> 
  select(-id) |> 
  mutate(sexo = as.character(sexo)) |> 
  mutate(ancho_cuerda = as.character(ancho_cuerda)) |> 
  rename('isla' = zona)
tela <- tela_og |> 
  mutate(region = 'Tela', .before='uid') |> 
  select(-id) |>
  select(-matches('salida|regreso'))
truj <- truj_og |> 
  mutate(region = 'Trujillo', .before='uid') |> 
  select(-id) |> 
  mutate(sexo = as.character(sexo)) 

# ==== Merging and exporting ====
master <- bind_rows(tela, truj, bi)
# Output path - using the last date of data available in the database as time
# flag
max(master$fecha, na.rm=T)
out_path <- 'data/master_landings_cln_20231213.csv'
write_csv(master, out_path)

