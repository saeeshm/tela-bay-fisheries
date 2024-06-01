# Author: Saeesh Mangwani
# Date: 2024-02-07

# Description: Querying fishbase for fork-length/total-length relationships per
# species, and using that for scaling lengths where needed

# ==== libraries ====
library(rfishbase)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
source('scripts/01_dataprep/03_fishbase_joining/_length_utils.R')

# ==== Paths and global variables ====

# Directory where lenght-length coefficients table
ll_dir <- 'data/fishbase_tables'
dir.create(ll_dir)

# Directory where cleaned tables from this step will be exported
out_dir <- 'data/temp_tidy'

# Path and object list for quick-read
paths <- list(
  # Databases which contain both total and fork-lengths
  'tela' = 'data/temp_tidy/tela_taxa_names_cleaned.csv',
  'trujillo' = 'data/temp_tidy/trujillo_taxa_names_cleaned.csv'
)

# ==== Reading data ====

# Helper function to quickly read a list of paths
qread <- function(obj_names, paths, ...){
  walk2(obj_names, paths, \(x, value){
    print(paste('Created variable:', x))
    assign(x, read_csv(value, ...), pos=globalenv())
  })
}

# Reading data paths
qread(names(paths), paths, show_col_types=F)

# Saving into a list
dats <- list(tela, trujillo) |> setNames(c('tela', 'trujillo'))

# Getting the taxonomy and species ID tables from fishbase
fb <- get_fbase_ll_tabs()
db_disconnect()

# ==== Scaling lengths ====

# Getting tabular scaling factors for all species in each dataframe
lltabs <- map(dats, \(dat){
  # Getting the unique taxa for this database
  unique_taxa <- dat |> 
    select(family, genus, species) |> 
    filter(!is.na(family)) |> 
    distinct()
  
  # For each unique taxa level, getting the best average FL/TL relationship
  ll_table <- pmap_dfr(unique_taxa, \(family, genus, species){
    calc_best_ll_factors(family, genus, species, 
                         # Keeping Fork-length relationships but dropping their
                         # intercepts, setting the minimum number of required
                         # LL-factor data to 1
                         rm_fl=F, n_min=1, drop_fl_intcp=T)
  })
  return(ll_table)
})
  
# Joining them all and removing duplication
ll_full_table <- bind_rows(lltabs) |> 
  distinct() |> 
  arrange(family, genus, species)
  
# Using the coefficients table for scaling lengths in all databases
dats <- map(dats, \(dat){
  # Using factors to lengths for each row in the source table
  dat |> 
    left_join(ll_full_table, by=c('family', 'genus', 'species')) |> 
    mutate(long_original = longitud, .after=longitud) |> 
    # Applying scaling factors for all rows where present
    mutate(longitud = (a + b*long_original)) |> 
    # Cleaing the scaled total length results:
    mutate(longitud = case_when(
      # Where the length is already a total length, using it directly
      tipo_longitud == 'total' ~ long_original, 
      # Where the scaled length is missing (no LL data present, directly using the
      # fork-length assuming it to be equal to total) - not an ideal assumption,
      # but these rows will be removed anyways as the lack of LL data is just
      # because there is no species-ID data present (common or scientific names)
      is.na(longitud) | is.nan(longitud) ~ long_original,
      # Otherwise, keeping what was previously calculated
      T ~ longitud
    )) |> 
    # Removing extra columns
    select(-ll_match_level, -a, -b, -sd_a, -sd_b, -n_a, -n_b)
})

# ==== Writing to disk ====

# Writing the full length-length coefficients table to disk for reference
write_csv(ll_full_table, file.path(ll_dir, 'fl_tl_conversion_coeffs.csv'))

# Overwriting the cleaned databases with the new version including scaled lengths
iwalk(dats, \(dat, name){
  write_csv(dat, file.path(out_dir, paste0(name, '_lengths_reclassified.csv')))
})
