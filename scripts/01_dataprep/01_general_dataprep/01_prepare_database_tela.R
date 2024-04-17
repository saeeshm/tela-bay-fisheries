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

# ==== Paths ====

# 2023 timeseries data path
raw_path <- 'data/raw/tela-feb2024.xlsx'
sheet_name <- 'Base de datos'

# Output directory where cleaned data files will be saved
out_dir <- 'data/temp_tidy'

# ==== Global variables ====

# Index of most-useful column names arranged in the best-readable order - used
# to re-arrange all columns for easiest use when exporting
colidx <- c('uid', 'id', 'codigo', 'fecha', 
            'comunidad', 'zona_pesca', 
            'nombre_cientifico', 'nombre_comun', 
            'peso', 'longitud', 'tipo_longitud',
            'horas_pesca','numero_pescadores', 'tipo_arte')

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
tela_og <- read_excel(raw_path, sheet=sheet_name, na = c('NA', '', 'N/C'), col_types = 'text')

# ==== Initial cleaning - names ====

# Creating copies for cleaning
tela <- tela_og

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
names(tela) <- clean_names(names(tela))

# ==== Converting relevant column types ====

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
tela <- tela |>  
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

# ==== In-filling hours fished from departure and arrival times ====

# Departure times ----------

# Investigating column to see text-cleaning requirements
tela$hora_salida |> table(useNA='ifany')

# Creating a separate table for cleaned departure times
salidas <- tela |>
  select(id, fecha, hora_salida) |>   
  # Cleaning strings for easier parsing
  mutate(hora_salida = stri_trans_general(hora_salida, id = "Latin-ASCII")) |>   
  mutate(hora_salida = tolower(hora_salida)) |>   
  # Separating out ones with just numbers
  mutate(horas_num = as.numeric(hora_salida)) |>   
  # Converting these to time-periods (lubridate format)
  mutate(horas_num = hms::hms(days=horas_num)) |>   
  mutate(horas_num = lubridate::hms(as.character(horas_num))) |>   
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
  mutate(es_noche = str_detect(hora_salida, '(la\\s*noche)'))  |>  
  # Extracting hours stored as strings if present
  mutate(str_hora = str_extract(hora_salida, '(\\d+(:|\\s)?\\d+)\\s*(a|p)(\\.?)m')) |>   
  # Cleaning the string-hours
  mutate(str_hora = str_hora |>   
           str_remove_all('\\.') |>   
           str_squish()) |> 
  # Hard-code replacement of "10 pm" strings with 22:00 (easier to parse)
  mutate(str_hora = ifelse(str_detect(str_hora, '10\\s?pm'), '22:00', str_hora)) |>   
  # Converting to time ranges
  mutate(str_hora = lubridate::hm(str_hora)) |> 
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
  )) |> 
  # Getting the effective date of departure by subtracting dias antes
  mutate(fecha_salida = fecha - dias_antes) |>   
  # Getting a combined date/time of departure
  mutate(salida = fecha_salida + hora_final) |>
  # Adding a column to indicate which values were interpolated (guessed)
  mutate(salida_intp = ifelse((is.na(hora_salida) & is.na(str_hora)), TRUE, FALSE)) |> 
  # Selecting only needed columns
  select(id, hora_salida, salida, salida_intp)

# Arrival times ----------
tela$hora_regreso |>   table(useNA = 'ifany')
regresos <- tela |>
  # Selecting relevant columns
  select(id, fecha, hora_regreso) |>  
  # Cleaning strings for easier parsing
  mutate(hora_regreso = stri_trans_general(hora_regreso, id = "Latin-ASCII")) |>   
  mutate(hora_regreso = tolower(hora_regreso)) |>   
  # Separating out ones with just numbers
  mutate(horas_num = as.numeric(hora_regreso)) |>   
  # If any are more than 1, these are errors due to the automatic addition of a
  # 1980 date in excel to the timestamp (-_-). Cleaning this by subtracting 8,
  # to bring it back into range
  mutate(horas_num = if_else(horas_num > 1, horas_num - 8, horas_num)) |> 
  # Converting numeric time-stamps to time-periods (lubridate format)
  mutate(horas_num = hms::hms(days=horas_num)) |>   
  mutate(horas_num = lubridate::hms(as.character(horas_num))) |>   
  # Column indicating next day returns
  mutate(dia_sig = str_detect(hora_regreso, 'dia\\s*siguiente')) |>   
  mutate(dia_sig = ifelse(is.na(dia_sig), 0, dia_sig)) |>   
  # Cleaning text hour columns for parsing
  mutate(hora_regreso = str_squish(str_remove_all(hora_regreso, '\\.'))) |>   
  # Extracting which columns are pm
  mutate(ispm = str_detect(hora_regreso, 'pm')) |>   
  # Extracting the text chunk that contains the time
  mutate(str_hora = str_extract(hora_regreso, '(\\d+(:|\\s)?\\d+)\\s*(a|p)(\\.?)m')) |> 
  # Converting to time ranges
  mutate(str_hora = lubridate::hm(str_hora)) |>   
  # Adding 12 hours to those that say PM
  mutate(str_hora = if_else(ispm, str_hora+hours(12), str_hora)) |>   
  # Getting a combined time-of-arrival column by synthesizing all available
  # information
  mutate(hora_final = case_when(
    # if there a given time, using that
    !is.na(horas_num) ~ horas_num,
    # if there was a string-time, using that
    !is.na(str_hora) ~ str_hora,
    # Otherwise, defaulting to 1 pm
    T ~ lubridate::hms('13:00:00')
  )) |>   
  # Getting the effective date of departure by adding dias siguientes
  mutate(fecha_regreso = fecha + dia_sig) |>   
  # Getting a combined date/time of departure
  mutate(regreso = fecha_regreso + hora_final) |> 
  # Adding a column to indicate which values were interpolated (guessed)
  mutate(regreso_intp = ifelse((is.na(hora_regreso) & is.na(str_hora)), TRUE, FALSE)) |> 
  select(id, hora_regreso, regreso, regreso_intp)

# Threshold of minimum fishing hours to consider reasonable (values below this
# should be treated as interpolation or inversion errors)
min_horas_thresh <- 2

# Calculating hours fished by joining these datasets
tab_horas_pesca <- salidas |>   
  left_join(regresos, by='id') |>   
  mutate(horas_pesca = difftime(regreso, salida, units='hours')) |> 
  # Where the calculated result is less than a reasonable threshold (including
  # negative calculations) and there was no interpolation for either time, I
  # assume the salida and regreso are switched (based on manual review of the
  # times and dates)
  mutate(temp = salida) |> 
  mutate(toInvert = (horas_pesca < min_horas_thresh) & !salida_intp & !regreso_intp) |> 
  mutate(salida = if_else(toInvert, regreso, salida)) |> 
  mutate(regreso = if_else(toInvert, temp, regreso)) |> 
  # Recalculating hours fished
  mutate(horas_pesca = difftime(regreso, salida, units='hours')) |> 
  mutate(horas_pesca = as.numeric(horas_pesca)) |> 
  # Where the calculated result is less than the minimum threshold, setting
  # calculated hours to NA as these values are not reliable
  mutate(horas_pesca = if_else((horas_pesca < min_horas_thresh), NA_real_, horas_pesca)) |> 
  # Selecting only relevant columns for addition to the full table
  select(id, 
         'hora_salida_cln' = salida, 
         'hora_regreso_cln' = regreso, 
         'horas_pesca_calc' = horas_pesca, 
         salida_intp, regreso_intp)

# In-filling calculated hours fished to the full data (keeping any existing data
# for fished hours if relevant)
tela <- tela |> 
  left_join(tab_horas_pesca, by='id') |> 
  mutate(horas_pesca = ifelse(is.na(horas_pesca), horas_pesca_calc, horas_pesca)) |> 
  select(-horas_pesca_calc)

# ==== Cleaning and standardizing individual columns ====

# Creating an average price variable for 2023 (consistent with 2021 data format)
tela$precio_medio_por_libra <- tela |>  
  select(contains('precio')) |>  
  rowMeans(na.rm=T)

# Fecha column - all good
tela |> 
  filter(!is.na(fecha)) |> 
  pull(fecha) |> 
  range()

# horas_pesca column - all good
tela$horas_pesca |> table(useNA = 'ifany')

# Weight/peso column - all good, but potential error with values that are
# unreasonably small (less than 10-20 grams, which seems unreasonable for a
# caught fish It's possible these were accidentally recorded as pounds not
# grams)
is.na(tela$peso) |> table()
range(tela$peso, na.rm=T)
tela |> 
  filter(peso > 5000)

# Creating a standardized length column, with a flag indicating which type of
# length it is (there is always only 1 type of length, mostly horquilla) - these
# will be converted at a later cleaning step
tela <- tela |> 
  mutate(longitud = if_else(is.na(longitud_total), longitud_horquilla, longitud_total)) |> 
  mutate(tipo_longitud = if_else(is.na(longitud_total), 'horquilla', 'total'), .after=longitud) |> 
  select(-longitud_total, -longitud_horquilla)

# Madurez - all okay
tela$madurez |> table()
tela$madurez |> is.na() |> table()
tela |> filter(!is.na(madurez)) |> pull(fecha) |> range()
tela |> filter(!is.na(madurez)) |> pull(nombre_comun) |> table()

# Sexo - all okay
tela$sexo |> table()
tela$sexo |> is.na() |> table()
tela |> filter(!is.na(sexo)) |> pull(fecha) |> range()
tela |> filter(!is.na(sexo)) |> pull(nombre_comun) |> table()

# Zona pesca - too many variations to reasonably clean now, just made minor text
# changes (further cleaning will happen at a later stage with more contextual
# information)
tela$zona_pesca |> table(useNA = 'ifany')
tela <- tela |> 
  mutate(zona_pesca = str_to_title(str_squish(tolower(zona_pesca))))

# Tipo-arte
tela$tipo_arte |> table(useNA = 'ifany')
tela <- tela |> 
  mutate(tipo_arte = stri_trans_general(tipo_arte, id = "Latin-ASCII")) |> 
  mutate(tipo_arte = if_else(tipo_arte == 'Nasas', 'Nasa', tipo_arte)) |> 
  mutate(tipo_arte = str_to_title(tipo_arte))

# Tipo bote - all okay
tela$tipo_bote |> table(useNA = 'ifany')
tela <- tela |> 
  mutate(tipo_bote = str_to_title(str_squish(tolower(tipo_bote))))

# Oleaje - all good
tela$oleaje |> table(useNA = 'ifany')

# Tiempo - all good
tela$tiempo |> table(useNA = 'ifany')

# Eviscerado - all good
tela$eviscerado |> table(useNA = 'ifany')

# Capturas totales -  all good
tela$capturas_totales_escama |> table(useNA = 'ifany')

# Individual captura and precio columns - all good. Any errors were reviewed and
# confirmed to not be breaking before applying the numeric conversion function
# above. So these ones have no issues to further fixed (I manually reviewed this
# to double-check)
tela$capturas_blanco |> table(useNA = 'ifany')
tela$capturas_rojo |> table(useNA = 'ifany')
tela$capturas_negro |> table(useNA = 'ifany')
tela$capturas_jaiba |> table(useNA = 'ifany')
tela$capturas_robalo |> table(useNA = 'ifany')

# Gasto and ganacia columns - all good (same reason as above for precios and
# capturas)
tela$gasto_arte |> table(useNA = 'ifany')
tela$gasto_hielo |> table(useNA = 'ifany')
tela$gasto_combustible |> table(useNA = 'ifany')
tela$gasto_aceite |> table(useNA = 'ifany')
tela$gasto_alimentos |> table(useNA = 'ifany')
tela$gasto_otros |> table(useNA = 'ifany')
tela$ganancias_esperadas |> table(useNA = 'ifany')

# Longitud total arte
tela$longitud_total_arte |> table(useNA = 'ifany')
tela <- tela |> 
  # Converting range to average
  mutate(longitud_total_arte = if_else(
    longitud_total_arte == '200 - 300', 
    '250', longitud_total_arte
  )) |> 
  mutate(longitud_total_arte = my_as_numeric(longitud_total_arte))

# Comunidad
tela$comunidad |> table(useNA = 'ifany')
tela <- tela |> 
  mutate(comunidad = str_to_title(tolower(comunidad))) |> 
  mutate(comunidad =  stri_trans_general(comunidad, id = "Latin-ASCII"))

# Adding an improved ID column
tela <- tela |> 
  mutate(uid = paste0('TLA', str_pad(id, 3, 'left', '0')), .before=id)

# ==== Finalizing and exporting ====

# Ordering column names by moving the identified most relevant ones to the front
other_names <- names(tela)[!names(tela) %in% colidx]
tela <- tela |> select(all_of(c(colidx, other_names)))

# Writing the full timeseries to disk
write_csv(tela, file.path(out_dir, 'tela_tidied.csv'))


