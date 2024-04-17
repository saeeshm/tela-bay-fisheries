# Author: Saeesh Mangwani
# Date: 2024-01-31

# Description: Creates a tidied and formatted species catalogues for Tela and
# Trujillo, by tidying and formatting the raw data that was manually scraped
# from the 2 species catalogue reports published by CORAL: 

# Tela Bay catalogue -
# https://coral.org/wp-content/uploads/2021/09/Cata%CC%81logo-de-Especies-en-la-Bahia-de-Tela-20.04.21.pdf
# Trujillo catalogue -
# https://coral.org/wp-content/uploads/2021/05/Cata%CC%81logo-de-Especies-Bahi%CC%81a-de-Trujillo-y-Laguna-de-Guaimoreto-20.04.21-1.pdf

# This script also queries fishbase and sealifebase to obtain higher-order
# taxonomy information for each species. This also serves as a sanity check to
# ensure that none of the provided species data has typos of any sort. Family is
# later used for assigning scientific names to common names, so this process is
# useful for the downstream full codebook

# ==== libraries ====
library(readxl)
library(readr)
source('scripts/01_dataprep/02_species_name_cleaning/_codebook_help_funcs.R')

# ==== Paths and global variables ====

# Directory containing the catalogue (will also be the output directory)
cat_dir <- 'data/species_name_cleaning/coral-species-catalogues'

# Path to raw catalogue excel sheets
in_path <- file.path(cat_dir, 'coral_catalogos_de_especies_raw.xlsx')

# Index vector storing ID pre-fixes for each database
id_prefix <- c('tela' = "TEL", 'trujillo' = 'TRJ')

# ==== Reading data ====

# Reference taxonomic databases - fishbase and sealifebase
rdbs <- get_taxa_dbases()

# Reading sheets into a named list
dats <- list(
  'tela' = read_excel(in_path, sheet='tela', na = c('', 'NA')), 
  'trujillo' = read_excel(in_path, sheet='trujillo', na = c('','NA'))
)

# ==== Initial text cleaning ====

# Main text cleaning
dats <- map(dats, \(tab){mutate(tab, across(everything(), clean_strings))})

# Where there is a corrected scientific name, using that instead (and dropping
# the extra column)
dats <- map(dats, \(tab){
  tab |> 
    mutate(scientific_name = if_else(
      !is.na(scientific_name_fixed),
      scientific_name_fixed,
      scientific_name)
    ) |> 
    select(-scientific_name_fixed)
})

# Splitting genus/species into distinct columns and cleaning
dats <- imap(dats, function(tab, y){
  tab|> 
    tidyr::separate(scientific_name, into=c('genus', 'species')) |> 
    mutate(genus = str_to_title(genus)) |> 
    mutate(family = str_to_title(family_reported), .before='genus') |> 
    select(-family_reported) |> 
    # Removing "sp" from species
    mutate(species = if_else(species == 'sp', NA_character_, species)) %>%
    # Adding a UID column
    mutate(uid = str_pad(1:nrow(.), 'left', width=3, '0')) |> 
    # Removing columns where all scientific values are NA (no matching can be
    # done here)
    filter(!(is.na(family) & is.na(genus) & is.na(species)))
})

# ==== Getting family names (Fishbase) ====

tab <- dats$tela %>%
  mutate(uid = 1:nrow(.), .before='family')
uid <- tab[51,]$uid
family <- tab[51,]$family
genus <- tab[51,]$genus
species <- tab[51,]$species
# family <- NA_character_
# genus <- NA_character_
# species <- NA_character_

# Edit-distance parameters to apply (which method, and what is the maximum
# threshold)
dist_method <- 'lv'
dist_thresh <- 3

# Large helper function that finds appropriate taxa matches for each row of a
# provided catalogue dataframe (structured this way to be called with pmap). The
# logic it follows is to match using the highest available taxa first. It then
# work downwards, restricting the search to any matched upper taxa found. The
# resulting matches at the most granular level are then returned as the true
# match, and if any re-classification is detected (found if matches are
# inconsistentb across all available taxonomic fields), it flags these in an
# error table and returns that separately for manual review (still choosing the
# most granular as the best one)
match_catalogue_to_dbases <- function(uid, family, genus, species, rdbs, ...){
  print(paste(c(family, genus, species), collapse=' - '))
  # Getting the best match for family
  curr_level <- 'family'
  fmatch <- call_match_function(family, rdbs, 'family', 
                                method=dist_method, maxDist=dist_thresh)
  # Genus, if present
  gmatch <- if(!is.na(genus)){
    call_match_function(genus, rdbs, 'genus', fmatch, 'family', 
                        method=dist_method, maxDist=dist_thresh)
  }else{
    tibble()
  }
  # Species, if present
  smatch <- if(!is.na(species)){
    call_match_function(species, rdbs, 'species', gmatch, 'genus', 
                        method=dist_method, maxDist=dist_thresh)
  }else{
    tibble()
  }
 
  # Joining all three match results
  mtab <- bind_rows(fmatch, gmatch, smatch)
  
  # Getting the smallest unit the match was based on
  smallest <- tail(mtab$matched_using, 1)
  
  # If there were multiple matches based on several pieces of taxonomic
  # information, cleaning the match table and flagging any potential
  # re-classification of taxa
  if(nrow(mtab) > 1){
    # In-filling match columns above this-level where matches were found
    finmatches <- setNames(mtab$match, mtab$matched_using)
    finmatches <- finmatches[!str_detect(names(finmatches), smallest)]
    for(i in 1:length(finmatches)){
      mtab[i,][paste0(names(finmatches[i]), '_match')] <- finmatches[i]
    }
  }
  
  # Checking if there has been a taxonomic change using a flag variable
  allSame <- function(x){length(na.omit(unique(x))) <= 1}
  isChange <- !all(map_lgl(as.list(select(mtab, matches('_match$'))), allSame))
  
  # Creating an output table
  out_tab <- bind_cols(
    list('uid' = uid, 'source_family' = family, 
         'source_genus' = genus, 'source_species' = species)
    )
  # Joinging the final classified result to the output table
  joincol <- paste0('source_', smallest)
  jtab <- mtab
  names(jtab)[1] <- joincol
  names(jtab)[3] <- paste0(smallest, '_', names(jtab)[3])
  out_tab <- out_tab |> 
    left_join(jtab, by=joincol) |> 
    mutate(isChanged = isChange)
  
  # If there has been a change, storing the raw output from this classification to
  # return as an error table
  errtab <- if(isChange) mtab |> mutate(uid=uid, .before='src') else tibble()
  # Returning error table and match table
  return(list('errtab' = errtab, 'match' = out_tab))
}

# Calling the function on both databases via pmap
matched <- imap(dats, \(x, y){
  print(y)
  pmap(x|> mutate('rdbs' = list(rdbs)), match_catalogue_to_dbases)
})

# Transposing results to get separate tables
results <- map(matched, \(x){
  map(purrr::transpose(x), bind_rows)
})

# Manually reviewing errors (the matches have already taken the "best", i.e most
# granularly-based decision. This is just to review if they make sense)
results$tela$errtab
results$trujillo$errtab

# Cleaning the output columns with the correctly classified names and returning
# a filtered and cleaned output
cleaned <- imap(results, \(x, y){
  mtab <- x$match
  mtab |> 
    left_join(dats[[y]] |> select(uid, common_name), by='uid') |> 
    mutate(uid = paste0(id_prefix[y], uid)) |> 
    dplyr::select(uid, 'ref_dbase' = database, 
           'class' = class_match, 'order' = order_match, 
           'family' = family_match, 'genus' = genus_match,
           'species' = species_match, common_name)
})

# ==== Writing to disk ====
iwalk(cleaned, \(x, y){
  # Creating a vector of ID columns
  ids <- paste0(id_prefix[y], str_pad(1:nrow(x), 3, 'left', '0'))
  print(y)
  x |> 
    write_csv(file.path(cat_dir, paste0(y, '_species_catalogue_cleaned.csv')))
})
