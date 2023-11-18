# Author: Saeesh Mangwani
# Date: 2023-11-15

# Description: Preparing a synthesized full time-series dataset of fish landings
# for Tela Bay

# ==== Libraries ====
library(dplyr)
library(readxl)
library(readr)
library(stringr)
library(stringi)
library(lubridate)
library(hms)

# ==== Paths and Global Variables ====

# 2023 timeseries data path
path_2023 <- 'data/raw/2023 base de datos monitoreo desembarcos Trujillo actualizada Mayo .xlsx'
sheet_2023 <- 'Base de datos'

# 2021 timeseries data path
path_2021 <- 'data/raw/20210802 Base de datos marinos monitoreos pesqueros por desembarque.xlsx'
sheet_2021 <- '4 marzo 2015 - 18 mayo 2021'

# Output directory where cleaned data files will be saved
out_dir <- 'data'

# A helper function that removes any text from a vector before converting to
# numeric
my_as_numeric <- function(x, clean=T, round=T, sfigs=3){
  if(clean){
    x <- str_remove_all(x, '[^0-9\\.]+')
  }
  out <- as.numeric(x)
  if(round) out <- round(out, sfigs)
  return(out)
}
  
# ==== Reading data ====

# Reading both time-period datasets
d23_og <- read_excel(path_2023, sheet=sheet_2023, na = 'NA', col_types = 'text')
d21_og <-  read_excel(path_2021, sheet=sheet_2021, na = 'NA', col_types = 'text')

# ==== Cleaning 2023 data ====

# Creating copies for cleaning
d23 <- d23_og
d21 <- d21_og

# Cleaning names ----------
clean_names <- function(nmvec){
  nmvec %>% 
    # Removing all bracketed content
    str_remove_all('\\(.+\\)') %>% 
    tolower() %>% 
    # Replacing whitespaces
    str_trim() %>% 
    str_replace_all('\\s+', '_') %>% 
    # Replacing all "a" and "de"  - makes names easier
    str_replace_all('_de_', '_') %>% 
    str_replace_all('_a_', '_') %>% 
    # Removing all "_diario_" from columns (easier to be consistent)
    str_replace_all('_diario_', '_') %>% 
    # Removing all "_lbs_" from columns (easier to be consistent)
    str_replace_all('(_lbs_?)$', '')
}
names(d23) <- clean_names(names(d23))
names(d21) <- clean_names(names(d21))

# Column formatting 2023 ----------

# Specifying columns that need to be converted to numeric/integer
num_cols <- c('numero_pescadores', 'longitud_horquilla', 
              'longitud_total', 'longitud_total_arte',
              'horas_pesca', 'peso', 'madurez', 
              'peso_gonadas', 'profundidad', 'luz_malla', 
              'numero_embarcaciones_salieron_pescar', 
              'numero_embarcaciones_muestreadas')

# Converting column types
d23 <- d23 %>% 
  # Converting dates from the excel number storage format to dttms
  mutate(fecha = (ymd('1900-jan-1') - 2 + as.integer(fecha))) %>% 
  # Converting all numeric columns
  mutate(across(all_of(num_cols),  my_as_numeric)) %>% 
  # Converting to numeric all columns relating to catches - character values
  # converted to NA
  mutate(across(contains('captura'), my_as_numeric)) %>%
  # Converting to numeric all columns relating to prices
  mutate(across(contains('precio'), my_as_numeric)) %>%
  # Converting to numeric all columns relating to costs
  mutate(across(contains('gasto'), my_as_numeric))
  
# Cleaning 2021 data ----------

# Specifying columns that need to be converted to numeric/integer
num_cols <- c('numero_pescadores', 'longitud_horquilla', 
              'longitud_total', 'longitud_total_arte',
              'horas_pesca', 'peso', 'madurez', 
              'peso_gonadas', 'profundidad', 'luz_malla', 
              'numero_embarcaciones_salieron_pescar', 
              'numero_embarcaciones_muestreadas')

# Converting column types
d21 <- d21 %>% 
  # Converting dates from the excel number storage format to dttms
  mutate(fecha = (ymd('1900-jan-1') - 2 + as.integer(fecha))) %>% 
  # Converting all numeric columns
  mutate(across(all_of(num_cols),  my_as_numeric)) %>% 
  # Converting to numeric all columns relating to catches - character values
  # converted to NA
  mutate(across(contains('captura'), my_as_numeric)) %>%
  # Converting to numeric all columns relating to prices
  mutate(across(contains('precio'), my_as_numeric)) %>%
  # Converting to numeric all columns relating to costs
  mutate(across(contains('gasto'), my_as_numeric))

# ==== Further cleaning to enable joining ====

# Creating an average price variable for 2023 (consistent with 2021 data format)
d23$precio_medio_por_libra <- d23 %>% 
  select(contains('precio')) %>% 
  rowMeans(na.rm=T)

# Creating a total captures variable (consistent with 2021 data format)
d23$capturas_totales <- d23 %>% 
  select(contains('captura')) %>% 
  rowSums(na.rm=T)

# Combining bait costs with other costs in 2021, to be consistent with 2023
d21 <- d21 %>% 
  mutate(gasto_otros_sin_carnada = gasto_otros) %>% 
  mutate(gasto_otros = gasto_carnada + gasto_otros)

# 2021 - Calculating hours fished from date of departure/arrival ----------

# Hour of departure - investigating the column to see what needs to be cleaned
table(d21$hora_salida)

# Cleaning
salidas <- d21 %>% 
  select(id, fecha, hora_salida) %>% 
  # Cleaning strings for easier parsing
  mutate(hora_salida = stri_trans_general(hora_salida, id = "Latin-ASCII")) %>% 
  mutate(hora_salida = tolower(hora_salida)) %>% 
  # Separating out ones with just numbers
  mutate(horas_num = as.numeric(hora_salida)) %>% 
  # Converting these to time-periods (lubridate format)
  mutate(horas_num = hms::hms(days=horas_num)) %>% 
  mutate(horas_num = lubridate::hms(as.character(horas_num))) %>% 
  # Replacing the rows where dia-anterior is in brackets with a bracket-free
  # version
  mutate(hora_salida = str_replace_all(hora_salida, '\\(dia anterior\\)', 'dia anterior')) %>% 
  # Removing all bracketed content (not useful)
  mutate(hora_salida = str_remove_all(hora_salida, '\\(.*\\)')) %>% 
  # Getting a count of days for rows that involved a prior-day departure
  mutate(dias_antes = case_when(
    # 1-day before
    str_detect(hora_salida, '(((1|un)\\s*dia\\s*antes)|(dia\\s*anterior))') ~ 1,
    # 2-days before
    str_detect(hora_salida, '((2|dos)\\s*dias?\\s*antes)') ~ 2,
    # 3-days before
    str_detect(hora_salida, '((3|tres)\\s*dias?\\s*antes)') ~ 3,
    # 4-days before
    str_detect(hora_salida, '((4|cuatro)\\s*dias?\\s*antes)') ~ 4,
    # Otherwise, 0
    T ~ 0
  )) %>% 
  # At-night or not
  mutate(es_noche = str_detect(hora_salida, '(la\\s*noche)')) %>% 
  # Extracting hours stored as strings if present
  mutate(str_hora = str_extract(hora_salida, '(\\d+(:|\\s)?\\d+)\\s*(a|p)(\\.?)m')) %>% 
  # Cleaning the string-hours
  mutate(str_hora = str_hora %>% 
           str_remove_all('\\.') %>% 
           str_squish()) %>% 
  # Hard-code replacement of "10 pm" strings with 22:00 (easier to parse)
  mutate(str_hora = ifelse(str_detect(str_hora, '10\\s?pm'), '22:00', str_hora)) %>% 
  # Converting to time ranges
  mutate(str_hora = lubridate::hm(str_hora)) %>% 
  # Getting a combined time-of-departure column by synthesizing all available
  # information
  mutate(hora_final = case_when(
    # if there a given time, using that
    !is.na(horas_num) ~ horas_num,
    # if there was a string-time, using that
    !is.na(str_hora) ~ str_hora,
    # If it is a night, defaulting to 10pm (most common nighttime departure)
    es_noche ~ lubridate::hms('22:00:00'),
    # Otherwise, defaulting to 4:30am (most common morning departure)
    T ~ lubridate::hms('04:30:00')
  )) %>% 
  # Getting the effective date of departure by subtracting dias antes
  mutate(fecha_salida = fecha - dias_antes) %>% 
  # Getting a combined date/time of departure
  mutate(salida = fecha_salida + hora_final) %>% 
  select(id, salida)

# Hour of arrival/return - investigating the column to see what needs to be
# cleaned
d21$hora_regreso %>% table()
regresos <- d21 %>% 
  # Selecting relevant columns
  select(id, fecha, hora_regreso) %>%
  # Cleaning strings for easier parsing
  mutate(hora_regreso = stri_trans_general(hora_regreso, id = "Latin-ASCII")) %>% 
  mutate(hora_regreso = tolower(hora_regreso)) %>% 
  # Separating out ones with just numbers
  mutate(horas_num = as.numeric(hora_regreso)) %>% 
  # Converting these to time-periods (lubridate format)
  mutate(horas_num = hms::hms(days=horas_num)) %>% 
  mutate(horas_num = lubridate::hms(as.character(horas_num))) %>% 
  # Column indicating next day returns
  mutate(dia_sig = str_detect(hora_regreso, 'dia\\s*siguiente')) %>% 
  mutate(dia_sig = ifelse(is.na(dia_sig), 0, dia_sig)) %>% 
  # Cleaning text hour columns for parsing
  mutate(hora_regreso = str_squish(str_remove_all(hora_regreso, '\\.'))) %>% 
  # Extracting which columns are pm
  mutate(ispm = str_detect(hora_regreso, 'pm')) %>% 
  # Extracting the text chunk that contains the time
  mutate(str_hora = str_extract(hora_regreso, '(\\d+(:|\\s)?\\d+)\\s*(a|p)(\\.?)m')) %>% 
  # Converting to time ranges
  mutate(str_hora = lubridate::hm(str_hora)) %>% 
  # Adding 12 hours to those that say PM
  mutate(str_hora = if_else(ispm, str_hora+hours(12), str_hora)) %>% 
  # Getting a combined time-of-arrival column by synthesizing all available
  # information
  mutate(hora_final = case_when(
    # if there a given time, using that
    !is.na(horas_num) ~ horas_num,
    # if there was a string-time, using that
    !is.na(str_hora) ~ str_hora,
    # Otherwise, defaulting to 1 pm
    T ~ lubridate::hms('13:00:00')
  )) %>% 
  # Getting the effective date of departure by adding dias siguientes
  mutate(fecha_regreso = fecha + dia_sig) %>% 
  # Getting a combined date/time of departure
  mutate(regreso = fecha_regreso + hora_final) %>% 
  select(id, regreso)
  
# Calculating hours fished by joining these datasets
d21_horas_pesca <- salidas %>% 
  left_join(regresos, by='id') %>% 
  mutate(horas_pesca = difftime(regreso, salida, units='hours')) %>% 
  select(id, 
         'hora_salida_cln' = salida, 
         'hora_regreso_cln'= regreso, 
         'horas_pesca_nueva' = horas_pesca)

# Joining to the d21 dataset (keeping any existing data for fished hours if
# relevant)
d21 <- d21 %>% 
  left_join(d21_horas_pesca, by='id') %>% 
  mutate(horas_pesca = ifelse(is.na(horas_pesca), horas_pesca_nueva, horas_pesca)) %>% 
  select(-horas_pesca_nueva)

# Writing cleaned individual datasets to disk ----------
write_csv(d21, file.path(out_dir, 'landings_2021_cleaned.csv'))
write_csv(d23, file.path(out_dir, 'landings_2023_cleaned.csv'))

# ==== Joining to create a master time-series table ====

# Which names in 2021 are not in 2023
extra_names_21 <- names(d21)[!(names(d21) %in% names(d23))]
# And vice versa
extra_names_23 <- names(d23)[!(names(d23) %in% names(d21))]

# Removing rows where everything is empty except id (many such in 2023) - I
# apply the filter using only a check for empty dates, as this is the most
# important field and only rows missing everything are also missing dates
d23 <- d23 %>% 
  filter(!is.na(fecha))

# Checking for date-overlap (undoubtedly there is some)
range(d21$fecha)
range(d23$fecha)

# Which rows occur in both - joining and checking for duplication - seems to be
# no duplication! Overlapping dates from both datasets contain unique
# information
d23 %>% 
  filter(fecha %in% d21$fecha) %>% 
  mutate(dbase_year = '2023', .before='id') %>% 
  bind_rows(d21 %>% 
              filter(fecha %in% d23$fecha) %>% 
              mutate(dbase_year = '2021', .before='id')) %>% 
  arrange(fecha, muestreador, comunidad) %>% 
  distinct()

# Joining both datasets to create a master timeseries - using the 2023 table as
# the reference for columns, and removing all unnecessary columns from 2021 as
# identified from the extra-column vector above
master <- d21 %>% 
  select(-all_of(extra_names_21)) %>% 
  bind_rows(d23) %>% 
  arrange(fecha, codigo)

# Writing the full timeseries to disk
write_csv(master, file.path(out_dir, 'landings_full_ts.csv'))


