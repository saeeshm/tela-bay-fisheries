# Author: Saeesh Mangwani
# Date: 2024-02-07

# Description: Querying fishbase for special ecology information per species
# (specifically trophic level and functional group if available) and joining to
# the master tables

# ==== libraries ====
library(rfishbase)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
source('scripts/01_dataprep/03_fishbase_joining/_ecology_utils.R')

# ==== Paths and global variables ====

# Directory where trophic-level crosswalk table (result from this script) will
# be stored
troph_dir <- 'data/fishbase_tables'

# Directory where cleaned tables from this step will be exported
out_dir <- 'data'

# Path and object list for quick-read
paths <- list(
  # Databases which contain both total and fork-lengths
  'tela' = 'data/temp_tidy/tela_lengths_reclassified.csv',
  'trujillo' = 'data/temp_tidy/trujillo_lengths_reclassified.csv'
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

# Getting the relevant fishbase tables
fb <- get_relv_fbase_tabs()
db_disconnect()

# ==== Getting trophic levels ====
ecotabs <- map(dats, \(dat){
  # Getting the unique taxa for this database
  unique_taxa <- dat |> 
    select(family, genus, species) |> 
    filter(!is.na(family)) |> 
    distinct()
  
  # For each unique taxa level, getting the best average FL/TL relationship
  eco_table <- pmap_dfr(unique_taxa, \(family, genus, species){
    get_troph_level(family, genus, species, 
                         # Keeping Fork-length relationships but dropping their
                         # intercepts, setting the minimum number of required
                         # LL-factor data to 1
                         n_min=1)
  })
  return(eco_table)
})
  
# Joining them all and removing duplication
eco_full_tab <- bind_rows(ecotabs) |> 
  distinct() |> 
  arrange(family, genus, species)
  
# Joining trophic level information to raw databases
dats <- map(dats, \(dat){
  # Using factors to lengths for each row in the source table
  dat |> 
    left_join(eco_full_tab, by=c('family', 'genus', 'species'))
})

# ==== Writing to disk ====

# Writing the full length-length coefficients table to disk for reference
write_csv(eco_full_tab, file.path(troph_dir, 'taxa_trophic_level_cwalk.csv'))

# Overwriting the cleaned databases with the new version including scaled
# lengths
iwalk(dats, \(dat, name){
  write_csv(dat, file.path(out_dir, paste0('dbase_', name, '_clean.csv')))
})

