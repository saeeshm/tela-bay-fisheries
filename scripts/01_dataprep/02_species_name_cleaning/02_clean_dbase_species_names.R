# Author: Saeesh Mangwani
# Date: 2024-01-31

# Description: Preparing a single authoritative codebook of common name to
# species names based on the 2 catalogues

# ==== libraries ====
library(openxlsx)
library(readr)
library(tidyr)
source('scripts/01_dataprep/02_species_name_cleaning/_codebook_help_funcs.R')

# ==== Paths and global variables ====

# Output directory
out_dir <- 'data/species_name_cleaning/scientific-names-matched'

# Helper function to quickly read a list of paths
qread <- function(obj_names, paths, ...){
  walk2(obj_names, paths, \(x, value){
    print(paste('Created vatiable:', x))
    assign(x, read_csv(value, ...), pos=globalenv())
  })
}

# Path and object list for quick-read
paths <- list(
  'tla' = 'data/temp_tidy/tela_tidied.csv',
  'trj' = 'data/temp_tidy/trujillo_tidied.csv',
  'bic' = 'data/temp_tidy/bica_tidied.csv',
  'tela_cat' = 'data/species_name_cleaning/coral-species-catalogues/tela_species_catalogue_cleaned.csv',
  'truj_cat' = 'data/species_name_cleaning/coral-species-catalogues/trujillo_species_catalogue_cleaned.csv'
)

# Reading data objects
qread(names(paths), paths, show_col_types=F)

# ==== Getting and standardizing reference catalogues ====

# Fishbase and Sealifebase
rdbs <- get_taxa_dbases()
db_disconnect()

# Combining local catalogues with the databases
rdbs$tela <- tela_cat |> 
  select(class, order, family, genus, species)
rdbs$trujillo <- truj_cat |> 
  select(class, order, family, genus, species)
names(rdbs)

# ==== Preparing the source matching data ====

# For each table, separating out rows where existing taxonomic fields are
# available. Those rows where only the common names are present are treated
# separately for common name classification (next stage)
prep_name_tables <- function(sourcedb){
  # Preparing a name table for matching
  nametab <- sourcedb %>%
    select(uid, 'scn'=nombre_cientifico, 'cmn'=nombre_comun) |> 
    # CLeaning text
    mutate(scn = clean_strings(scn),
           cmn = clean_strings(cmn)) |> 
    # Splitting to Genus and species
    separate(scn, into=c('genus', 'species')) |> 
    mutate(genus = str_to_title(genus)) |> 
    # Removing "sp(p)" from species
    mutate(species = if_else(str_detect(species, '^sp+'), NA_character_, species)) %>%
    # Where Genus ends with the suffix "dae" it is a family. So assigning those to
    # a new columny called family
    mutate(family = if_else(str_detect(genus, '(dae|nae|deae?)$'), genus, NA_character_), .before='genus') |> 
    # And setting Genus for these to NA, if any only if there is no species
    # information (if there is species info, we leave the genus column filled in
    # the chance that the genus name is quite close to family name, such as for
    # Holocentrus and Holocentridae)
    mutate(genus = if_else(!is.na(family) & is.na(species), NA_character_, genus)) |> 
    # Replacing empty strings with NA
    mutate(across(-uid,\(x){na_if(x, '')}))
  
  # Separating rows where no taxonomic info is present
  cmn <- nametab |> filter(is.na(family) & is.na(genus) & is.na(species))
  
  # The rest - used for catalogue matching (remove the common name)
  scn <- nametab |> 
    filter(!(uid %in% !!cmn$uid)) |> 
    select(-cmn)
  return(list('cmn' = cmn, 'scn' = scn))
}

# Preparing tables for name matching
match_input_tabs <- list(tla, trj, bic) |> 
  setNames(c('tela', 'trujillo', 'bica')) |> 
  imap(\(x, y){
    # For tables that have a separate "familia" column - moving that to the
    # scientific name field where it is otherwise missing
    if(y %in% c('tela', 'trujillo')){
      x <- x |> 
        mutate(nombre_cientifico = if_else(is.na(nombre_cientifico), familia, nombre_cientifico))
    }
    print(y)
    prep_name_tables(x)
  })

# Separating rows that contain scientific name information, and so can be
# cleaned via matching to external databases or catalogues
scn_input_tabs <- map(match_input_tabs, \(x){x$scn})
cmn_input_tabs <- map(match_input_tabs, \(x){x$cmn})

# ==== Matching rows to databases scientific name matching ====

# Creating restricted matching with only unique combinations of
# family-genus-species. Each group is assigned a groupid before reducing, so it
# can be joined back to the original table
tabs_by_unique_taxa <- map(scn_input_tabs, \(dat){
  x <- dat %>%
    group_by(family, genus, species)
  x$grpid <- group_indices(x)
  return(x)
})

# Large helper function that finds appropriate taxa matches for each row of a
# provided source dataframe (structured this way to be called with pmap). The
# logic it follows is to match using the highest available taxa first. It then
# works downwards, restricting the search to any matched upper taxa found. The
# resulting matches at the most granular level are then returned as the true
# match, and if any re-classification is detected (found if matches are
# inconsistent across all available taxonomic fields), it flags these in an
# error table and returns that separately for manual review (still choosing the
# most granular as the best one)

# Edit-distance parameters to apply (which method, and what is the maximum
# threshold)
dist_method <- 'lv'
dist_thresh <- 3

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

# Calling the function to match for each input dataset - getting match results
match_results <- imap(tabs_by_unique_taxa, \(dat, y){
  curr_rdbs <- rdbs[names(rdbs) %in% c('fishbase', 'sealifebase', y)]
  names(curr_rdbs) <- c('fishbase', 'sealifebase', 'catalogue')[1:length(curr_rdbs)]
  # Getting only 1 row from each group - only matching unique taxa for more
  # efficiency
  input_tab <- dat |> 
    arrange(grpid) |> 
    filter(row_number()==1) |> 
    mutate(uid = grpid) |> 
    ungroup() |> 
    mutate(rdbs = list(curr_rdbs))
  # Passing only this table to the matching function to efficiently get back
  # matches
  matches <- pmap(input_tab, match_catalogue_to_dbases) 
  out <- map(purrr::transpose(matches), bind_rows)
  return(out)
})

# Separating the error tables for manual review
errtabs <- map(match_results, \(x){x$errtab})
errtabs

# Cleaning the output columns with the correctly classified names and returning
# a filtered and cleaned output
cleaned <- imap(match_results, \(x, y){
  mtab <- x$match
  mtab |> 
    # left_join(dats[[y]] |> select(uid, common_name), by='uid') |> 
    # mutate(uid = paste0(id_prefix[y], uid)) |> 
    dplyr::select('grpid' = uid, 'ref_dbase' = database, 
                  'class' = class_match, 'order' = order_match,
                  'family' = family_match, 'genus' = genus_match,
                  'species' = species_match, isChanged)
})

# Joining each match back to the source rows
scn_matched <- map2(cleaned, tabs_by_unique_taxa, \(clean, source){
  source |> 
    ungroup() |> 
    select(uid, 'source_family' = family, 
           'source_genus' = genus, 'source_species' = species,
           grpid) |> 
    left_join(clean, by='grpid') |> 
    select(-grpid)
})

# Writing match results - these tables can be used to directly match cleaned
# scientific names to the source data
dir.create(out_dir)
iwalk(scn_matched, \(x, y){
  print(y)
  write_csv(x, file.path(out_dir, paste0(y, '_scientific_names_matched.csv')))
})

# ==== Creating an intermediate common/scientific name catalogue ====

# Joining all the matched taxonomic data back to the original tables, and then
# extracting the common names associated with each unique scientific name. This
# will be the common-scientific species crosswalk table for each dataset
# independently
codebooks <- list(tla, trj, bic) |> 
  setNames(c('tela', 'trujillo', 'bica')) |> 
  map2(scn_matched, \(x, y){
    x |> 
      # Cleaning the common names using the standard function
      select(uid, nombre_comun) |> 
      mutate(nombre_comun = clean_strings(nombre_comun)) |> 
      # For every row where it is available, joining matched taxonomy from the
      # results above
      left_join(
        y |> 
          mutate(nombre_cientifico = paste(family, genus, species)) |> 
          mutate(nombre_cientifico = str_squish(str_replace_all(nombre_cientifico, 'NA', ''))) |>
          select(uid, order, class, family, genus, species, nombre_cientifico),
        by='uid'
      ) |> 
      # Creating a table which gives all common names for each scientific name
      group_by(nombre_cientifico) |> 
      group_modify(\(x, ...){
        tibble(
          'class' = unique(x$class),
          'order' = unique(x$order),
          'family' = unique(x$family),
          'genus' = unique(x$genus),
          'species' =  unique(x$species),
          'nombre_comun' = unique(x$nombre_comun)
        )
      }) |> 
      ungroup() |> 
    # Removing rows where the common-name is NA (this is unhelpful) |> 
    filter(!is.na(nombre_comun) & !(nombre_comun==''))
  })

# Writing the codebook for each database to a separate excel sheet - missing
# scientific names for some common names (which remained unmatched) will be
# manually filled to create a final crosswalk table
openxlsx::write.xlsx(codebooks, file=file.path(out_dir, '../species_common_codebook_raw.xlsx'))




