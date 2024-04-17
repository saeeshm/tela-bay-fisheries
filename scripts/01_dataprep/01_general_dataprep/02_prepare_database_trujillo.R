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

# Trujillo timeseries data path
path_raw <- 'data/raw/trujillo-dec2023.xlsx'
sheet_name <- 'Base de datos'

# Output directory where cleaned data files will be saved
out_dir <- 'data/temp_tidy'

# ==== Global variables ====

# Index of most-useful column names arranged in the best-readable order - used
# to re-arrange all columns for easiest use when exporting
colidx <- c('uid', 'id', 'codigo', 'fecha', 
            'comunidad', 'ecosistema','zona_pesca', 
            'nombre_cientifico', 'nombre_comun', 
            'peso', 'longitud', 'tipo_longitud',
            'horas_pesca', 'numero_pescadores', 'tipo_arte')

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
trujillo_og <- read_excel(path_raw, sheet=sheet_name, na = 'NA', col_types = 'text')

# ==== Initial cleaning - names ====

# Creating copy for cleaning
truj <- trujillo_og

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
names(truj) <- clean_names(names(truj))

# ==== Cleaning and standardizing individual columns ====

# Fixing dates
truj <- truj |> 
  mutate(old = fecha) |> 
  # Where the number was converted to numeric (by excel), converting to integer
  mutate(fecha_int = as.integer(old)) |> 
  # Converting dates from numbers or from text
  mutate(fecha = case_when(
    (!is.na(fecha_int)) ~ (ymd('1900-jan-1') - 2 + fecha_int),
    # Fixing a specific error where the date was reported as Nov 31 - changing
    # to Nov 30
    old=='31/11/2023' ~ dmy('30/11/2023'),
    # Otherwise defaulting to a dmy
    T ~ dmy(old)
  )) |> 
  select(-fecha_int, -old)

# horas_pesca column - can be cleaned numeric conversion directly (done later)
truj$horas_pesca |> table(useNA = 'ifany')

# Weight/peso column - cleaned by numeric conversion
truj$peso |> table(useNA = 'ifany')

# Creating a standardized length column, with a flag indicating which type of
# length it is (there is always only 1 type of length, mostly horquilla)
truj$longitud_total |> table(useNA = 'ifany')
truj$longitud_horquilla |> table(useNA = 'ifany')
truj <- truj |> 
  mutate(longitud = if_else(is.na(longitud_total), longitud_horquilla, longitud_total)) |> 
  mutate(tipo_longitud = if_else(is.na(longitud_total), 'horquilla', 'total'), .after=longitud) |> 
  select(-longitud_total, -longitud_horquilla)

# Madurez - no info
truj$madurez |> table(useNA = 'ifany')

# Sexo - just 1 non-missing data point - effectively no info.
truj$sexo |> table(useNA = 'ifany')

# Zona pesca - too many variations to reasonably clean now, just made a minor
# changes (probably best to clean in more detail when I need to use it)
truj <- truj |> 
  mutate(zona_pesca = str_to_title(str_squish(tolower(zona_pesca))))
truj$zona_pesca |> table(useNA = 'ifany')

# Tipo-arte - fixing 1 typo
truj$tipo_arte |> table(useNA = 'ifany')
truj <- truj |> 
  mutate(tipo_arte = if_else(tipo_arte == 'Cueda', 'Cuerda', tipo_arte))

# Tipo bote - minor text cleaning
truj$tipo_bote |> table(useNA = 'ifany')
truj <- truj |> 
  mutate(tipo_bote = str_to_title(str_squish(tolower(tipo_bote))))

# Oleaje - minor text cleaning
truj$oleaje |> table(useNA = 'ifany')
truj <- truj |> 
  mutate(oleaje = str_to_title(str_squish(tolower(oleaje))))

# Tiempo - fixing typos
truj <- truj |> 
  mutate(tiempo = if_else(tiempo=='moderddo', 'moderado', tiempo)) |> 
  mutate(tiempo = str_to_title(str_squish(tolower(tiempo))))
truj$tiempo |> table(useNA = 'ifany')

# Eviscerado - all good
truj$eviscerado |> table(useNA = 'ifany')

# Individual captura and precio columns - all will be cleaned by numeric
# conversion
truj$capturas_blanco |> table(useNA = 'ifany')
truj$capturas_negro |> table(useNA = 'ifany')
truj$capturas_robalo |> table(useNA = 'ifany')
truj$capturas_rojo |> table(useNA = 'ifany')
# Jaiba has only 2 data points both of which don't make sense, so they will be
# removed upon numeric conversion
truj$capturas_jaiba |> table(useNA = 'ifany')
truj$capturas_otros |> table(useNA = 'ifany')

# Gasto and ganacia columns - cleaned by numeric conversion
truj$gasto_arte |> table(useNA = 'ifany')
truj$gasto_hielo |> table(useNA = 'ifany')
truj$gasto_combustible |> table(useNA = 'ifany')
truj$gasto_aceite |> table(useNA = 'ifany')
truj$gasto_alimentos |> table(useNA = 'ifany')
truj$gasto_otros |> table(useNA = 'ifany')
truj$ganancias_esperadas |> table(useNA = 'ifany')

# Longitud total arte - all good, cleaned by numeric conversion
truj$longitud_total_arte |> table(useNA = 'ifany')

# Comunidad - minor text cleaning
truj <- truj |> 
  mutate(comunidad =  stri_trans_general(comunidad, id = "Latin-ASCII")) |> 
  mutate(comunidad = str_to_title(tolower(comunidad)))
truj$comunidad |> table(useNA = 'ifany')
 
# ==== Type conversion of relevant columns ====

# Makes it easier to do the further cleaning/joining processes to do this first

# Specifying columns that need to be converted to numeric/integer - not
# converting longitud_total_arte yet because it needs more manual cleaning
# before conversion
num_cols <- c('numero_pescadores', 'longitud', 'longitud_total_arte',
              'horas_pesca', 'peso', 'madurez', 
              'peso_gonadas', 'profundidad', 'luz_malla', 
              'numero_embarcaciones_salieron_pescar', 
              'numero_embarcaciones_muestreadas')

# Converting column types
truj <- truj |>  
  # Converting all numeric columns
  mutate(across(all_of(num_cols),  my_as_numeric)) |>  
  # Converting to numeric all columns relating to catches - character values
  # converted to NA
  mutate(across(contains('captura'), my_as_numeric)) |> 
  # Converting to numeric all columns relating to prices
  mutate(across(contains('precio'), my_as_numeric)) |> 
  # Converting to numeric all columns relating to costs
  mutate(across(contains('gasto'), my_as_numeric))

# Adding calculated columns that needed numeric values ----------

# Creating an average price variable for 2023 (consistent with Tela data)
truj$precio_medio_por_libra <- truj |>  
  select(contains('precio')) |>  
  rowMeans(na.rm=T)

# Creating a total captures variable (consistent with Tela data)
truj$capturas_totales <- truj |>  
  select(contains('captura')) |>  
  rowSums(na.rm=T)

# Adding an improve unique ID column
truj <- truj |> 
  mutate(uid = paste0('TRJ', str_pad(id, 3, 'left', '0')), .before=id)

# ==== Finalizing and exporting ====

# Ordering column names by moving the identified most relevant ones to the front
other_names <- names(truj)[!names(truj) %in% colidx]
truj <- truj |> select(all_of(c(colidx, other_names)))

# Writing the full timeseries to disk
write_csv(truj, file.path(out_dir, 'trujillo_tidied.csv'))


