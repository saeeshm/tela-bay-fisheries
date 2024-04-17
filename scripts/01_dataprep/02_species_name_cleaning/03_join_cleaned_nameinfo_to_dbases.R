# Author: Saeesh Mangwani
# Date: 2024-02-06

# Description: Joining available matched taxonomic information to the databases,
# and using that as the starting point for an authoritative codebook of species
# names

# ==== libraries ====
library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
source('scripts/01_dataprep/02_species_name_cleaning/_codebook_help_funcs.R')

# ==== Paths ====

# Output directory
out_dir <- 'data'

# Path and object list for quick-read
paths <- list(
  # Main databases
  'tela' = 'data/temp_tidy/tela_tidied.csv',
  'truj' = 'data/temp_tidy/trujillo_tidied.csv',
  'bica' = 'data/temp_tidy/bica_tidied.csv',
  # Databases with cleaned scientific names (produced using existing scientific
  # name information (from script 02) and matched by UID)
  'scn_tela' = 'data/species_name_cleaning/scientific-names-matched/tela_scientific_names_matched.csv',
  'scn_truj' = 'data/species_name_cleaning/scientific-names-matched/trujillo_scientific_names_matched.csv',
  'scn_bica' = 'data/species_name_cleaning/scientific-names-matched/bica_scientific_names_matched.csv'
)

# Excel sheet containing cleaned scientific names assigned manually based on
# available common names
cmn_path <- 'data/species_name_cleaning/species_common_name_codebook_working.xlsx'

# ==== Global variables ====

# Helper function to quickly read a list of paths
qread <- function(obj_names, paths, ...){
  walk2(obj_names, paths, \(x, value){
    print(paste('Created variable:', x))
    assign(x, read_csv(value, ...), pos=globalenv())
  })
}

# ==== Reading data ====

# Common name codebooks for each database
cmn_tela <- read_excel(cmn_path, sheet='tela')
cmn_truj <- read_excel(cmn_path, sheet='trujillo')
cmn_bica <- read_excel(cmn_path, sheet='bica')

# Quick-reading tabular data
qread(names(paths), paths, show_col_types=F)

# Joining input databases into lists
namevec <- c('tela', 'trujillo', 'bica')
dbs <- list(tela, truj, bica) |> setNames(namevec)
scn <- list(scn_tela, scn_truj, scn_bica) |> setNames(namevec)
cmn <- list(cmn_tela, cmn_truj, cmn_bica) |> setNames(namevec)

# ==== Joining taxonomic info to tables ====

# Joining names to rows where some scientific name data are present
dbs <- map2(dbs, scn, \(dat, sci_name_cwalk){
  sci_name_cwalk |> 
    # tidyr::unite(scn_name, genus, species, sep=' ', na.rm=T) |> 
    select(uid, class, order, family, genus, species) |> 
    # mutate(scn_name = ifelse(scn_name=='', NA_character_, scn_name)) |> 
    # Joining
    right_join(dat, by='uid') |> 
    relocate(class, order, family, genus, species, .after='nombre_cientifico')
    # mutate(nombre_cientifico_lmp = scn_name, .after='nombre_cientifico')
})

# Where scientific names are missing, joining based on common names to get a
# table of common name matches
cmn_name_matches <- map2(dbs, cmn, \(dat, cmn_name_cwalk){
  # Filtering only rows where cleaned scientific names are missing
  mtab <- dat |> 
    filter(is.na(family))
  # Joining based on common names
  mtab |> 
    select(uid, nombre_comun) |> 
    mutate(nombre_comun = clean_strings(nombre_comun)) |> 
    left_join(cmn_name_cwalk, by=c('nombre_comun'='nombre_comun_original')) |> 
    select(uid, nombre_comun, class, order, family, genus, species)
})

# Joining these matches to the databases to infill all taxonomic information
dbs <- map2(dbs, cmn_name_matches, \(dat, cmn_match_table){
  cmn_match_table |> 
    # tidyr::unite(scn_name, genus, species, sep=' ', na.rm=T) |>
    # mutate(scn_name = ifelse(scn_name=='', NA_character_, scn_name)) |>
    select(uid, 'cls' = class, 'ord' = order, 'fam' = family, 
           'gen' = genus, 'spc' = species) |> 
    # Joining
    right_join(dat, by='uid') |> 
    # Copying data over to the main columns where relevant
    mutate(class = if_else(!is.na(cls), cls, class)) |> 
    mutate(order = if_else(!is.na(ord), ord, order)) |> 
    mutate(family = if_else(!is.na(fam), fam, family)) |> 
    mutate(genus = if_else(!is.na(gen), gen, genus)) |> 
    mutate(species = if_else(!is.na(spc), spc, species)) |> 
    # Removing extra columns
    select(-cls,-ord, -fam, -gen, -spc)
})

# Writing each table to disk, to get the final cleaned version of the databases
iwalk(dbs, \(x, y){
  # Adding an extra column containing a cleaned version of the common names
  # before writing
  out <- x |> 
    mutate(nombre_comun_cln = clean_strings(nombre_comun), .after='nombre_comun') |> 
    arrange(uid)
  # For BICA, writing a cleaned database directly (no length-scaling required)
  if(y == 'bica'){
    write_csv(out, file.path(out_dir, paste0('dbase_', y, '_clean.csv')))
  # For the other two, writing temporary tables as the length data needs to be
  # scaled before finalizing the database
  }else{
    write_csv(out, file.path(out_dir, 'temp_tidy', paste0(y, '_taxa_names_cleaned.csv')))
  }
})

# ==== Creating an authoritative code-book from all this matching ====

# Getting the full catalogue of species name info across all three datasets
full_cat <- map_dfr(dbs, \(x){
  x |> 
    select(class, order, family, genus, species, nombre_comun) |> 
    # Removing rows not matched at least to the family level
    filter(!is.na(family)) |> 
    mutate(nombre_comun = clean_strings(nombre_comun)) |> 
    distinct()
})

# Function that gets all common names per scientific name grouped at a given
# taxonomic level (i.e allows for creating separate common-name tables for
# matching up to family, genus, and species)
sep_by_group <- function(dtab, taxa_level){
  # Getting the index of the column containing this taxa level
  cidx <- which(names(dtab) == taxa_level)
  # Grouping the dataframe by all taxa columns upto this one
  out_cat <- dtab |> 
    filter(!is.na(!!sym(taxa_level))) |> 
    group_by(across(all_of(1:cidx)))
  # Getting a group-ID vector
  out_cat$grpid <- group_indices(out_cat)
  # Adding the ID vector and summarizing to get all common names associated with
  # this group as separate rows
  out_cat |> 
    group_modify(\(x, y){
      tibble(
        'grpid' = unique(x$grpid),
        'nc_estandar' = unique(x$nombre_comun)
      )
    }) |> 
    ungroup() |> 
    # Relocating the groupid
    relocate(grpid, .before='class')
}

# Creating crosswalk tables for each taxa level
fam_cat <- sep_by_group(full_cat, 'family')
gen_cat <- sep_by_group(full_cat, 'genus')
spec_cat <- sep_by_group(full_cat, 'species')

# Exporting to separate sheets of an excel
outlist <- list(fam_cat, gen_cat, spec_cat) |> setNames(c('family', 'genus', 'species'))
openxlsx::write.xlsx(outlist, file=file.path(out_dir, '../scientific_common_crosswalk_raw.xlsx'))


