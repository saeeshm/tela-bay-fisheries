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
path_2023 <- 'data/coral/raw/2023 base de datos monitoreo desembarcos Trujillo actualizada Mayo .xlsx'
sheet_2023 <- 'Base de datos'

# 2021 timeseries data path
path_2021 <- 'data/coral/raw/20210802 Base de datos marinos monitoreos pesqueros por desembarque.xlsx'
sheet_2021 <- '4 marzo 2015 - 18 mayo 2021'

# Output directory where cleaned data files will be saved
out_dir <- 'data/coral'

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

# ==== Initial cleaning - names ====

# Creating copies for cleaning
d23 <- d23_og
d21 <- d21_og

# Cleaning names ----------
clean_names <- function(nmvec){
  nmvec |>  
    # To lowercase and standard ascii
    tolower() |> 
    stri_trans_general(id = "Latin-ASCII") |> 
    # Removing all bracketed content
    str_remove_all('\\(.+\\)') |>  
    # Replacing whitespaces
    str_trim() |>  
    str_replace_all('\\s+', '_') |>  
    # Replacing all "a" and "de"  - makes names easier
    str_replace_all('_de_', '_') |>  
    str_replace_all('_a_', '_') |>  
    # Removing all "_diario_" from columns (easier to be consistent)
    str_replace_all('_diario_', '_') |>  
    # Removing all "_lbs_" from columns (easier to be consistent)
    str_replace_all('(_lbs_?)$', '')
}
names(d23) <- clean_names(names(d23))
names(d21) <- clean_names(names(d21))

# ==== Type conversion of relevant columns ====

# Makes it easier to do the further cleaning/joining processes to do this first

# Specifying columns that need to be converted to numeric/integer - not
# converting longitud_total_arte yet because it needs more manual cleaning
# before conversion
num_cols <- c('numero_pescadores', 'longitud_horquilla', 
              'longitud_total', 
              'horas_pesca', 'peso', 'madurez', 
              'peso_gonadas', 'profundidad', 'luz_malla', 
              'numero_embarcaciones_salieron_pescar', 
              'numero_embarcaciones_muestreadas')

# Converting column types
d23 <- d23 |>  
  # Converting dates from the excel number storage format to dttms
  mutate(fecha = (ymd('1900-jan-1') - 2 + as.integer(fecha))) |>  
  # Converting all numeric columns
  mutate(across(all_of(num_cols),  my_as_numeric)) |>  
  # Converting to numeric all columns relating to catches - character values
  # converted to NA
  mutate(across(contains('captura'), my_as_numeric)) |> 
  # Converting to numeric all columns relating to prices
  mutate(across(contains('precio'), my_as_numeric)) |> 
  # Converting to numeric all columns relating to costs
  mutate(across(contains('gasto'), my_as_numeric))
  
# Cleaning 2021 data ----------

# Specifying columns that need to be converted to numeric/integer
num_cols <- c('numero_pescadores', 'longitud_horquilla', 
              'longitud_total',
              'horas_pesca', 'peso', 'madurez', 
              'peso_gonadas', 'profundidad', 'luz_malla', 
              'numero_embarcaciones_salieron_pescar', 
              'numero_embarcaciones_muestreadas')

# Converting column types
d21 <- d21 |>  
  # Converting dates from the excel number storage format to dttms
  mutate(fecha = (ymd('1900-jan-1') - 2 + as.integer(fecha))) |>  
  # Converting all numeric columns
  mutate(across(all_of(num_cols),  my_as_numeric)) |>  
  # Converting to numeric all columns relating to catches - character values
  # converted to NA
  mutate(across(contains('captura'), my_as_numeric)) |> 
  # Converting to numeric all columns relating to prices
  mutate(across(contains('precio'), my_as_numeric)) |> 
  # Converting to numeric all columns relating to costs
  mutate(across(contains('gasto'), my_as_numeric))

# ==== Cleaning to enable joining ====

# Minor hard-coded changes for consistency ----------

# Creating an average price variable for 2023 (consistent with 2021 data format)
d23$precio_medio_por_libra <- d23 |>  
  select(contains('precio')) |>  
  rowMeans(na.rm=T)

# Creating a total captures variable (consistent with 2021 data format)
d23$capturas_totales <- d23 |>  
  select(contains('captura')) |>  
  rowSums(na.rm=T)

# Combining bait costs with other costs in 2021, to be consistent with 2023
d21 <- d21 |>  
  mutate(gasto_otros_sin_carnada = gasto_otros) |>  
  mutate(gasto_otros = gasto_carnada + gasto_otros_sin_carnada)

# 2021 - Calculating hours fished from date of departure/arrival ----------

# Hour of departure - investigating the column to see what needs to be cleaned
# table(d21$hora_salida)

# Cleaning
salidas <- d21 |>  
  select(id, fecha, hora_salida) |>  
  # Cleaning strings for easier parsing
  mutate(hora_salida = stri_trans_general(hora_salida, id = "Latin-ASCII")) |>  
  mutate(hora_salida = tolower(hora_salida)) |>  
  # Separating out ones with just numbers
  mutate(horas_num = as.numeric(hora_salida)) |>  
  # Converting these to hour-stamps-periods (lubridate format)
  mutate(horas_num = round(horas_num*24)) |>  
  mutate(salida_cln = ymd_hms(paste0(fecha,' ', horas_num,':00:00'))) |> 
  # Replacing the rows where dia-anterior is in brackets with a bracket-free
  # version
  mutate(hora_salida = str_replace_all(hora_salida, '\\(dia anterior\\)', 'dia anterior')) |>  
  # Removing all bracketed content (not useful)
  mutate(hora_salida = str_remove_all(hora_salida, '\\(.*\\)')) |>  
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
  )) |>  
  # At-night or not
  mutate(es_noche = str_detect(hora_salida, 'noche')) |> 
  # Extracting hours stored as strings if present
  mutate(str_hora = str_extract(hora_salida, '(\\d+(:|\\s)?\\d+)\\s*(a|p)(\\.?)m')) |> 
  # Cleaning the string-hours
  mutate(str_hora = str_hora |>  
           str_remove_all('\\.') |>  
           str_squish() |> 
           str_replace_all('10', '10:00')) |>  
  # Converting to date-time ranges
  mutate(str_hora = (paste(as.character(fecha), str_hora))) |> 
  mutate(str_hora = parse_date_time(str_hora, 'Y:m:d I:M p')) |> 
  # Getting a combined time-of-departure column by synthesizing all available
  # information
  mutate(salida_cln = case_when(
    # if the time has already been parsed, using that directly
    !is.na(salida_cln) ~ salida_cln,
    # if there was a string-time that could be parsed, using that
    !is.na(str_hora) ~ str_hora,
    # If it is a night, defaulting to 10pm (most common nighttime departure)
    es_noche ~ ymd_hms(paste(fecha, '22:00:00')),
    # Otherwise, defaulting to 4:30am (most common morning departure)
    T ~ ymd_hms(paste(fecha, '04:30:00'))
  )) |> 
  # Getting the effective date of departure by subtracting dias antes
  mutate(salida_cln = salida_cln - days(dias_antes)) |> 
  # Column to indicate which ones are complete interpolations (no info at all)
  mutate(salida_es_intrp = is.na(hora_salida)) |> 
  # Selecting only finalized columns
  select(id, hora_salida, 'salida' = salida_cln, salida_es_intrp)

# Hour of arrival/return - investigating the column to see what needs to be
# cleaned
# d21$hora_regreso |>  table()
regresos <- d21 |>  
  # Selecting relevant columns
  select(id, fecha, hora_regreso) |> 
  # Cleaning strings for easier parsing
  mutate(hora_regreso = stri_trans_general(hora_regreso, id = "Latin-ASCII")) |>  
  mutate(hora_regreso = tolower(hora_regreso)) |>  
  # Separating out ones with just numbers
  mutate(horas_num = as.numeric(hora_regreso)) |>  
  # Converting these to time-periods (lubridate format)
  mutate(horas_num = round(horas_num*24)) |>  
  mutate(regreso_cln = ymd_hms(paste0(fecha,' ', horas_num,':00:00'))) |> 
  # Column indicating if it was a next day returns - setting to 0 if there is no
  # such info available, even if the column is NA
  mutate(dia_sig = str_detect(hora_regreso, 'dia\\s*siguiente')) |> 
  mutate(dia_sig = if_else(is.na(dia_sig), 0, dia_sig)) |> 
  # Cleaning text hour columns for parsing
  mutate(hora_regreso = str_squish(str_remove_all(hora_regreso, '\\.'))) |>
  # Extracting the text chunk that contains the time
  mutate(str_hora = str_extract(hora_regreso, '(\\d+(:|\\s)?\\d+)\\s*(a|p)(\\.?)m')) |> 
  # Converting these date-time ranges
  mutate(str_hora = (paste(as.character(fecha), str_hora))) |>
  mutate(str_hora = parse_date_time(str_hora, 'Y:m:d I:M p')) |>
  # Getting a combined time-of-arrival column by synthesizing all available
  # information
  mutate(regreso_cln = case_when(
    # if the time has already been parsed, using that directly
    !is.na(regreso_cln) ~ regreso_cln,
    # if there was a string-time, using that
    !is.na(str_hora) ~ str_hora,
    # Otherwise, defaulting to 1 pm
    T ~ ymd_hms(paste(fecha, '13:00:00'))
  )) |>  
  # Getting the effective date of departure by adding dias siguientes
  mutate(regreso_cln = regreso_cln + days(dia_sig)) |> 
  # Column to indicate which ones are complete interpolations (no info at all)
  mutate(regreso_es_intrp = is.na(hora_regreso)) |> 
  select(id, hora_regreso, 'regreso' = regreso_cln, regreso_es_intrp)
  
# Calculating hours fished by joining these datasets
d21_horas_pesca <- salidas |>  
  left_join(regresos, by='id') |>  
  mutate(horas_pesca = difftime(regreso, salida, units='hours')) |>  
  mutate(horas_pesca = as.numeric(horas_pesca)) |> 
  # Further cleaning - some hours are negative, and if there is available data
  # for both departure and arrival, this issue is likely due to the two columns
  # being inverted while reporting. Resolving this
  mutate(temp = salida) |> 
  # Inverting the departure hour if the hours fished is negative and both raw
  # columns had data
  mutate(salida = if_else(
    (horas_pesca < 0) & (!is.na(hora_salida) & !is.na(hora_regreso)), 
    regreso,
    salida
  )) |> 
  # Same for the return hour, using the temp column for help
  mutate(regreso = if_else(
    (horas_pesca < 0) & (!is.na(hora_salida) & !is.na(hora_regreso)), 
    temp,
    regreso
  )) |> 
  # Re-calculating fished hours with this change
  mutate(horas_pesca = difftime(regreso, salida, units='hours')) |>  
  mutate(horas_pesca = as.numeric(horas_pesca)) |> 
  # Keeping only relevant columns
  select(id, 
         'hora_salida_cln' = salida, 
         salida_es_intrp,
         'hora_regreso_cln'= regreso, 
         regreso_es_intrp,
         'horas_pesca_nueva' = horas_pesca)

# Joining to the d21 dataset (keeping any existing data for fished hours if
# relevant)
d21 <- d21 |>  
  left_join(d21_horas_pesca, by='id') |>
  # Adding a flag column indicating which hours were calculated and not present
  mutate(horas_pesca_es_calc = is.na(horas_pesca), .after=horas_pesca) |> 
  # Replacing hours-fished with the calculated hours, if present
  mutate(horas_pesca = ifelse(is.na(horas_pesca), horas_pesca_nueva, horas_pesca)) |>
  mutate(hora_salida_bruto = hora_salida, .before='hora_salida') |> 
  mutate(hora_regreso_bruto = hora_regreso, .before='hora_regreso') |> 
  mutate(hora_salida = hora_salida_cln) |> 
  mutate(salida_intrp = salida_es_intrp, .after = hora_salida) |> 
  mutate(hora_regreso = hora_regreso_cln) |> 
  mutate(regreso_intrp = regreso_es_intrp, .after = hora_regreso) |> 
  select(-horas_pesca_nueva, 
         -hora_salida_cln, -hora_regreso_cln, 
         -salida_es_intrp, -regreso_es_intrp)

# Writing cleaned individual datasets to disk ----------
write_csv(d21, file.path(out_dir, 'landings_2021_cleaned.csv'))
write_csv(d23, file.path(out_dir, 'landings_2023_cleaned.csv'))

# ==== Joining to create a master time-series table ====

# Which fields are extra in 2021 - all for 2021 are unnecessary columns
extra_names_21 <- names(d21)[!(names(d21) %in% names(d23))]
# For 2023 - all are informative columns, just not available for the 2021 data
extra_names_23 <- names(d23)[!(names(d23) %in% names(d21))]

# Removing rows where everything is empty except id (many such in 2023) - I
# apply the filter using only a check for empty dates, as this is the most
# important field and only rows missing everything are also missing dates
d23 <- d23 |>  
  filter(!is.na(fecha))

# Checking for date-overlap
range(d21$fecha)
range(d23$fecha)

# Joining rows that have the same dates between the 2 datasets to test for
# duplication
overlap <- d23 |>  
  filter(fecha %in% d21$fecha) |>  
  mutate(dbase_year = '2023', .before='id') |>  
  bind_rows(d21 |>  
              filter(fecha %in% d23$fecha) |>  
              mutate(dbase_year = '2021', .before='id')) |>  
  arrange(fecha, muestreador, comunidad)
# Seems to be no duplication! Overlapping dates from both datasets contain
# unique information
nrow(overlap)
nrow(distinct(overlap))

# Joining both datasets to create a master timeseries - using the 2023 table as
# the reference for columns, and removing all extra columns from 2021
# (identified from the extra-column vector above) as these are un-necessary
master <- d21 |>  
  select(-all_of(extra_names_21)) |>  
  bind_rows(d23) |>  
  arrange(fecha, codigo)

# ==== Cleaning and standardizing individual columns ====

# Fecha column - all good

# horas_pesca column - all good

# Weight/peso column - all good, but potential error with values that are
# unreasonably small (less than 10-20 grams, which seems unreasonable for a
# caught fight. It's possible these were accidentally recorded as pounds not
# grams)

# Creating a standardized length column, with a flag indicating which type of
# length it is (there is always only 1 type of length, mostly horquilla)
master <- master |> 
  mutate(longitud = if_else(is.na(longitud_total), longitud_horquilla, longitud_total)) |> 
  mutate(tipo_longitud = if_else(is.na(longitud_total), 'horquilla', 'total'), .after=longitud) |> 
  select(-longitud_total, -longitud_horquilla)

# Madurez - all okay

# Sexo
master <- master |> 
  mutate(sexo = if_else(sexo == 'F', 'H', sexo))

# Zona pesca - too many variations to reasonably clean now, just made a minor
# changes (probably best to clean in more detail when I need to use it)
master <- master |> 
  mutate(zona_pesca = str_to_title(str_squish(tolower(zona_pesca))))

# Tipo-arte
master <- master |> 
  mutate(tipo_arte = stri_trans_general(tipo_arte, id = "Latin-ASCII")) |> 
  mutate(tipo_arte = if_else(tipo_arte == 'Nasas', 'Nasa', tipo_arte)) |> 
  mutate(tipo_arte = str_to_title(tipo_arte))

# Tipo bote
master <- master |> 
  mutate(tipo_bote = str_squish(tolower(tipo_bote))) |> 
  mutate(tipo_bote = str_remove_all(tipo_bote, 'de vidrio')) |> 
  mutate(tipo_bote = str_to_title(tipo_bote))

# Oleaje
master <- master |> 
  mutate(oleaje = str_squish(tolower(oleaje))) |> 
  mutate(oleaje = str_to_title(oleaje))

# Tiempo
master <- master |> 
  mutate(tiempo = str_squish(tolower(tiempo))) |> 
  mutate(tiempo = case_when(
    str_detect(tiempo, 'nublado') ~ 'nublado',
    str_detect(tiempo, 'soleado') ~ 'soleado',
    str_detect(tiempo, 'lluvioso|llovizna') ~ 'llovizna',
    str_detect(tiempo, 'moderadas') ~ 'lluvia moderada',
    T ~ NA_character_
  )) |> 
  mutate(tiempo = str_to_title(tiempo))

# Eviscerado - all good

# Capturas totales -  all good

# Individual captura and precio columns - all good. Any errors were reviewed and
# confirmed to not be breaking before applying the numeric conversion function
# above. So these ones have no issues to further fixed (I manually reviewed this
# to double-check)

# Gasto and ganacia columns - all good (same reason as above for precios and
# capturas)

# Longitud total arte
master <- master |> 
  # Converting range to average
  mutate(longitud_total_arte = if_else(
    longitud_total_arte == '200 - 300', 
    '250', longitud_total_arte
  )) |> 
  mutate(longitud_total_arte = my_as_numeric(longitud_total_arte))

# Comunidad
master <- master |> 
  mutate(comunidad = str_to_title(tolower(comunidad))) |> 
  mutate(comunidad =  stri_trans_general(comunidad, id = "Latin-ASCII"))

# Writing the full timeseries to disk
write_csv(master, file.path(out_dir, 'coral_master_dbase_clean.csv'))


