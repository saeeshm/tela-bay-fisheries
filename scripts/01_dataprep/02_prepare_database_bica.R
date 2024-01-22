# Author: Saeesh Mangwani
# Date: 2024-01-16

# Description: Preparing a master BICA database for fishery analysis

# ==== libraries ====
library(dplyr)
library(purrr)
library(readxl)
library(readr)
library(stringr)
library(stringi)
library(lubridate)

# ==== Paths and global variables ====

# Paths to raw excel files
path_roatan <- 'data/bica/raw/Desembarco Roatan.xlsx'
path_guanaja <- 'data/bica/raw/Desembarcos Guanaja.xlsx'
path_utila <- 'data/bica/raw/Desembarcos Utila.xlsx'

# Path to where cleaned outputs will be stored
out_dir <- 'data/bica'

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

# Custom function for reading all sheets in a file
read_excel_all <- function(path, exclude_string){
  # Getting sheet names
  sheets <- readxl::excel_sheets(path)
  # Excluding based on the passed string
  sheets <- sheets[!str_detect(tolower(sheets), exclude_string)]
  # Reading each into a list
  tabs <- map(sheets, ~{
    read_excel(path, sheet=.x, na=c('', 'NA'), col_types='text')
  })
  return(setNames(tabs, sheets))
}

# Reading
exclude_string <- 'listados|pivot'
roatan <- read_excel_all(path_roatan, exclude_string)
utila <- read_excel_all(path_utila, exclude_string)
guanaja <- read_excel_all(path_guanaja, exclude_string)

# ==== Pre-treatment of individual tables to allow them to join ====

# First, cleaning column names
clean_names <- function(nmvec){
  nmvec |>  
    # To lowercase and standard ascii
    tolower() |> 
    stri_trans_general(id = "Latin-ASCII") |> 
    # Removing all bracketed content
    str_remove_all('\\(.+\\)') |>  
    # Replacing whitespaces with underscores
    str_trim() |>  
    str_replace_all('\\s+', '_') |>  
    # Replacing all "a" and "de"  - makes names easier
    str_replace_all('_de_', '_') |>  
    str_replace_all('_a_', '_') |>  
    # Removing all "_diario_" from columns (easier to be consistent)
    str_replace_all('_de?iario_', '_') |>  
    # Removing all "_lbs_" from columns (easier to be consistent)
    str_replace_all('(_?lbs?_?)$', '')
}

# Applying generic names cleaning
roatan <- map(roatan, ~{names(.x) <- clean_names(names(.x)); return(.x)})
utila <- map(utila, ~{names(.x) <- clean_names(names(.x)); return(.x)})
guanaja <- map(guanaja, ~{names(.x) <- clean_names(names(.x)); return(.x)})

# Ancilliary pre-cleaning steps ----------

# Fixing issue with number of fishers column for diamond-rock
roatan$`Diamond Rock` <- roatan$`Diamond Rock` |> 
  rename('numero_pescadores' = numero_pescadores_por_dia)

# Individually cleaning "captura" names for Santa Elena, Roatan
roatan$`Santa Elena` <- roatan$`Santa Elena` |> 
  rename('capturas_jaiba' = `capturas_biajaiba/ronco/jurel`,
         'capturas_negro' = capturas_cubera,
         'capturas_robalo' = `capturas_reef_snapper/_motin_snapper/baracuda`,
         'capturas_blanco' = capturas_cola_amarilla,
         'precio_jaiba' = precio_biajaiba,
         'precio_negro' = precio_cubera,
         'precio_robalo' = `precio_reef_snapper/motin`,
         'precio_blanco' = precio_cola_amarilla)

# Unlisting Utila, since there is only 1 relevant sheet
utila <- utila$`Base de datos`

# Combining the nombre-comun columns into just 1
utila <- utila |> 
  # Making length columns
  mutate(n1_len = str_length(nombre_comun_1), 
         n2_len = str_length(nombre_comun_2)) |> 
  # Making "cleaned" versions of the columns
  mutate(nombre_comun = case_when(
    # Where the names are the same, using the first one
    nombre_comun_1 == nombre_comun_2 ~ nombre_comun_1,
    # Otherwise, using the longer one
    n1_len > n2_len ~ nombre_comun_1,
    T ~ nombre_comun_2
  )) |> 
  select(-n1_len, -n2_len, -nombre_comun_1, -nombre_comun_2)

# ==== Joining all tables ====
t1 <- map_dfr(roatan, invisible) |> 
  mutate(zona = 'Roatan', .before='comunidad')
t2 <- map_dfr(guanaja, invisible) |> 
  mutate(zona = 'Guanaja', .before='comunidad')
t3 <- utila |> mutate(zona = 'Utila', .before='comunidad')
master <- bind_rows(t1, t2, t3)

# ==== Cleaning and standardizing individual columns ====

# Cleaning the date column and converting to date-time
master <- master |> 
  mutate(fecha_og = fecha) |> 
  # Replacing commas with points (breaks the date-conversion)
  mutate(fecha_og = str_replace_all(fecha_og, ',', '.')) |> 
  # Replacing some mis-written years
  mutate(fecha_og = str_replace_all(fecha_og, '20221', '2021')) |> 
  mutate(fecha_og = str_replace_all(fecha_og, '/0201', '/2021')) |> 
  # Re-writing one year written as mdy to dmy
  mutate(fecha_og = if_else(fecha_og == '5/30/2021', '30/5/2021', fecha_og)) |> 
  # Converting those which can be converted to dates
  mutate(fecha = dmy(fecha_og)) |> 
  # Converting those which are integer represented (from excel) to dates
  mutate(fecha_num = (ymd('1900-jan-1') - 2 + as.integer(fecha_og))) |> 
  # Converting from numeric to dates by range
  mutate(fecha = if_else(is.na(fecha), fecha_num, fecha)) |> 
  select(-fecha_og, -fecha_num)

# Cleaning the hours-fished column
master <- master |> 
  mutate(hp_og = horas_pesca) |> 
  mutate(horas_pesca = if_else(
    # Detecting which hours are decimal-represented
    str_detect(hp_og, '\\d{1}\\.[1-9]+'), 
    # Converting those with the numeric scaling
    as.numeric(hp_og)*24, 
    # Otherwise, direct text to numeric conversion
    my_as_numeric(hp_og)
  )) |> 
  mutate(horas_pesca = as.integer(horas_pesca)) |> 
  select(-hp_og)

# Clean the weight column
master <- master |>
  # Replacing an commas with decimals
  mutate(peso = str_replace_all(peso, ',', '.')) |> 
  # Replacing multiple points with a single point
  mutate(peso = str_replace_all(peso, '\\.+', '.')) |> 
  # Removing any whitespace
  mutate(peso = str_replace_all(peso, '\\s+', '')) |> 
  # Converting to numeric
  mutate(peso = as.numeric(peso)) |> 
  # Converting the roatan data from lbs to grams
  mutate(peso = if_else(zona == 'Roatan', peso*453.592, peso))

# Clean the length column
master <- master |> 
  # Replacing an commas with decimals
  mutate(longitud = str_replace_all(longitud, ',', '.')) |> 
  # Removing any whitespace
  mutate(longitud = str_replace_all(longitud, '\\s+', '')) |> 
  # Replace 1/2 with 0.5
  mutate(longitud = str_replace_all(longitud, '\\s+1\\/2', '.5')) |>
  # Converting to numeric
  mutate(longitud = as.numeric(longitud))

# - Madurez (based on sampling document)
master <- master |> 
  # Convert "maduro" to category 3 which stands for mature. Likely some
  # confounding going on here - strange that every single fish sampled in roatan
  # Diamond rock was mature??
  mutate(madurez = ifelse(madurez == 'maduro', 3, madurez)) |> 
  # Factorizing, and including all levels (0-5)
  mutate(madurez = factor(madurez, levels=0:5))

# - sexo
master <- master |> 
  mutate(sexo = case_when(
    str_detect(tolower(sexo), 'f') ~ 'F',
    str_detect(tolower(sexo), 'm') ~ 'M',
    T ~ NA
  )) |> 
  mutate(sexo = as.factor(sexo))

# - zona_pesca
master <- master |> 
  # Cleaning text for joining mismatched classes
  mutate(zona_pesca = tolower(zona_pesca)) |> 
  mutate(zona_pesca = str_remove_all(zona_pesca, '\\s')) |> 
  mutate(zona_pesca = str_replace_all(zona_pesca, 'e\\.', 'este')) |> 
  mutate(zona_pesca = str_replace_all(zona_pesca, 's\\.', 'sur')) |> 
  # pull(zona_pesca) |> 
  # table()
  # Manual reclassification
  mutate(zona_pesca = case_when(
    zona_pesca == 'bandanorte' ~ 'Banda Norte',
    zona_pesca == 'bandasur' ~ 'Banda Sur',
    zona_pesca == 'campbay' ~ 'Camp Bay',
    zona_pesca == 'barbareta' ~ 'Barbareta',
    zona_pesca == 'surbarbareta' ~ 'Barbareta Sur',
    zona_pesca == 's-ebarbareta' ~ 'Barbareta Sur-Este',
    zona_pesca == 'deiamonderock' ~ 'Diamond Rock',
    zona_pesca == 'estebarbareta' ~ 'Barbareta Este',
    zona_pesca == 'pumpkinhill' ~ 'Pumpkin Hill',
    zona_pesca == 'puntablanca' ~ 'Punta Blanca',
    zona_pesca == 'puntagorda' ~ 'Punta Gorda',
    zona_pesca == 'puntagordea' ~ 'Punta Gorda',
    zona_pesca == 'santaelena' ~ 'Santa Elena',
    zona_pesca == 'sursantaelena' ~ 'Santa Elena Sur',
    zona_pesca == 'surdest.eena' ~ 'Santa Elena Sur-Este',
    zona_pesca == '0' ~ NA_character_,
    zona_pesca == 'n/c' ~ NA_character_,
    T ~ str_to_title(zona_pesca)
  ))

# - Tipo de arte
master <- master |> 
  mutate(tipo_arte = str_squish(tolower(tipo_arte))) |> 
  mutate(tipo_arte = case_when(
    str_detect(tipo_arte, 'anz') ~ 'Anzuelo',
    str_detect(tipo_arte, 'cuerd') ~ 'Cuerda',
    str_detect(tipo_arte, 'arpon') ~ 'Arpon',
    str_detect(tipo_arte, 'buceo') ~ 'Buceo',
    str_detect(tipo_arte, 'carrete') ~ 'Carrete',
    T ~ NA_character_
  )) |> 
  mutate(tipo_arte = as.factor(tipo_arte))
  
# - Tipo bote
master <- master |> 
  mutate(tipo_bote = str_squish(tolower(tipo_bote))) |>
  mutate(tipo_bote = case_when(
    str_detect(tipo_bote, 'fibra') ~ 'Lancha de fibra',
    str_detect(tipo_bote, 'lancha') ~ 'Lancha',
    str_detect(tipo_bote, 'motor') ~ 'Cayuco motor',
    str_detect(tipo_bote, 'cay') ~ 'Cayuco de mano',
    T ~ NA_character_
  )) |>
  mutate(tipo_bote = as.factor(tipo_bote))
  
# - Oleaje
master <- master |> 
  mutate(oleaje = str_remove_all(tolower(oleaje), '\\s')) |>
  mutate(oleaje = case_when(
    str_detect(oleaje, 'muy') ~ str_replace_all(oleaje, 'muy', 'muy '),
    # The values for this and the tiempo column is reversed for some rows.
    # Manually switching them (i reviewed to know what the values are)
    oleaje == 'nublado' ~ 'bajo',
    oleaje == '0' ~ NA_character_,
    T ~ oleaje
  )) |> 
  mutate(oleaje = str_to_title(oleaje)) |> 
  mutate(oleaje = factor(oleaje, levels = c(
    'Muy Bajo', 'Bajo', 'Moderado', 'Alto', 'Muy Alto'
  )))

# - Tiempo
master <- master |> 
  mutate(tiempo = str_squish(tolower(tiempo))) |>
  mutate(tiempo = case_when(
    # Reversing the inverted column values as above
    str_detect(tiempo, 'bajo') ~ 'nublado',
    str_detect(tiempo, 'parcia') ~ 'nublado',
    str_detect(tiempo, 'nublad') ~ 'nublado',
    str_detect(tiempo, 'moderad') ~ 'lluvia moderada',
    str_detect(tiempo, 'fuerte') ~ 'lluvia fuerte',
    str_detect(tiempo, 'liuvias|vizn') ~ 'llovizna',
    tiempo == '0' ~ NA_character_,
    T ~ tiempo
  )) |> 
  mutate(tiempo = str_to_title(tiempo)) |> 
  mutate(tiempo = factor(tiempo, levels=c(
    'Soleado', 'Nublado', 'Llovizna', 'Lluvia Moderada', 'Lluvia Fuerte'
  )))

# Eviscerado
master <- master |> 
  mutate(eviscerado = str_squish(tolower(eviscerado))) |>
  mutate(eviscerado = case_when(
    str_detect(eviscerado, 's') ~ 'SE',
    str_detect(eviscerado, 'e') ~ 'E',
    T ~ NA_character_
  )) |> 
  mutate(eviscerado = factor(eviscerado, levels = c('E', 'SE')))

# Cleaning capturas totales escama
master <- master |> 
  mutate(capturas_totales_escama = my_as_numeric(capturas_totales_escama))

# Capturas totales comunidad
master <- master |> 
  mutate(capturas_totales_comunidad = str_squish(tolower(capturas_totales_comunidad))) |>
  # Separating extra info into new columns - halves
  mutate(medias = str_detect(capturas_totales_comunidad, 'med')*0.5) |> 
  # Cuarters
  mutate(cuartas = str_detect(capturas_totales_comunidad, 'cuart')*0.25) |> 
  # Ounces
  mutate(onzas = str_extract(capturas_totales_comunidad, '\\d+(?=\\s?oz)')) |> 
  mutate(onzas = as.numeric(onzas)/16) |> 
  mutate(onzas = if_else(is.na(onzas), 0, onzas)) |> 
  # Removing all ounce data from the original numbers (confuses the
  # my_as_numeric function)
  mutate(capturas_totales_comunidad = str_remove_all(
    capturas_totales_comunidad, '\\d+\\s?oz')) |> 
  # Replacing any completely empty strings with 0 (these had only ounce info)
  mutate(capturas_totales_comunidad = if_else(
    capturas_totales_comunidad == '', '0', capturas_totales_comunidad)) |> 
  # Cleaning and making numeric the captures column
  mutate(capturas_totales_comunidad = my_as_numeric(capturas_totales_comunidad)) |> 
  # Adding everything to make the final amount
  mutate(capturas_totales_comunidad = capturas_totales_comunidad+medias+cuartas+onzas) |> 
  select(-medias, -cuartas, -onzas)

# Checking and cleaning the individual precio and capturas columns with ranges
# to averages. Only 1 clean to make - UPDATE THIS ONCE WE FIX THE PRECIO
# COLUMNS
master <- master |> 
  mutate(precio_rojo = if_else(precio_rojo == '50-55', '52.5', precio_rojo))

# Cleaning gasto and ganacias columns - most errors can be handled by cusing the
# my_as_numeric function
master |> 
  mutate(gasto_hielo = if_else(str_detect(gasto_hielo, 'bolsa'), 
                               NA_character_, gasto_hielo)) |> 
  mutate(gasto_combustible = if_else(str_detect(gasto_combustible, 'Diesel'), 
                               NA_character_, gasto_combustible)) |> 
  mutate(ganancias_esperadas = if_else(str_detect(ganancias_esperadas, 'lps'), 
                                     NA_character_, ganancias_esperadas))

# Cleaning longitud total arte
master |> 
  # filter(str_detect(longitud_total_arte, 'buceo|cuerda')) |> 
  # select(longitud_total_arte, tipo_arte) |> 
  mutate(longitud_total_arte = my_as_numeric(longitud_total_arte)) |> 
  pull(longitud_total_arte) |> 
  table()

# Finalizing column types ----------

# Index of columns that need to be converted to numeric/integer
num_cols <- c('numero_pescadores', 'longitud', 
              'longitud_total_arte',
              'horas_pesca', 'peso',
              'peso_gonadas', 'profundidad', 'luz_malla', 
              'numero_embarcaciones_salieron_pescar', 
              'numero_embarcaciones_muestreadas')

# Converting column types
master <- master |> 
  # Converting all numeric columns
  mutate(across(all_of(num_cols),  my_as_numeric)) |> 
  # Converting to numeric all columns relating to catches - character values
  # converted to NA
  mutate(across(contains('captura'), my_as_numeric)) |> 
  # Converting to numeric all columns relating to prices
  mutate(across(contains('precio'), my_as_numeric)) |> 
  # Converting to numeric all columns relating to costs
  mutate(across(contains('gasto'), my_as_numeric)) |> 
  mutate(across(contains('ganancia'), my_as_numeric))

# ==== Writing finalized version to disk ====
write_csv(master, file.path(out_dir, 'bica_master_dbase_clean.csv'))

# 
