# Author: Saeesh Mangwani
# Date: 2024-02-01

# Description: Helper functions for codebook creation

# ==== libraries ====
library(dplyr)
library(purrr)
library(stringdist)
library(stringr)
library(stringi)
library(rfishbase)

# ==== Functions ====

# A generic text cleaning function
clean_strings <- function(strings){
  strings |> 
    tolower() |> 
    stri_trans_general(id = "Latin-ASCII") |> 
    str_remove_all('[:digit:]') |> 
    str_remove_all('(#|$|@|%|&|^|\\.|\\,)+') |> 
    str_squish()
}

# Loads taxonomy tables from fishbase and sealifebase
get_taxa_dbases <- function(local_catalogue = NULL, loc_name=NULL){
  # Fishbase classification catalogue
  fb <- fb_tbl('classification_tree', server='fishbase') |> 
    select('class' = Class, 'order' = Order,'family' = Family, 
           'genus' = Genus, 'species' = Species)
  
  # Sealifebase classification catalogue
  slb <- fb_tbl('species_famcode', server='sealifebase') |> 
    left_join(fb_tbl('families', server='sealifebase'), 
              by='FamCode') |> 
    select('class' = Class, 'order' = Order,'family' = Family, 
           'genus' = Genus, 'species'=Species.x)
  
  return(list('fishbase' = fb, 'sealifebase' = slb))
}

# Getting a restricted subset of a taxonomic catalogue - allows for taxa
# matching to only occur within a specific taxoomic group
restrict_refdb <- function(rdb, wi_taxa, wi_taxa_level){
  # Argument checking
  if(!is.null(wi_taxa) & is.null(wi_taxa_level)) {
    stop("Gave a name to restrict the search to, but did not specifc the taxonomic level")
  }
  if(!is.null(wi_taxa_level) & is.null(wi_taxa)) {
    stop("A taxonomic level for restricted search is specified, but no name is given")
  }
  if(!wi_taxa_level %in% names(rdb)) {
    stop(paste(wi_taxa_level, "does not exist as a column inside the reference database!"))
  }
  # Returning a filtered database containing only rows within this taxa
  out_tab <- rdb[rdb[, wi_taxa_level][[1]] == wi_taxa, ]
  return(out_tab)
}

# Function that returns the upper-classification (up to order) for a given name
# and taxonomic level (from a specified reference database)
get_upper_taxonomy <- function(name, rdb, taxa_level){
  # A helper vector storing the order of taxa
  taxa_order <- c(1:5) |> setNames(c('species', 'genus', 'family', 'order', 'class'))
  rdb |> 
    # Filtering for only that specific taxa
    filter(!!sym(taxa_level) == name) |> 
    # Getting all higher order columns
    select(names(taxa_order[taxa_order > taxa_order[taxa_level]])) |> 
    # Returning as a 1-row dataframe as these are just repeats
    distinct()
}

# Find the matching name from a given database, for a certain taxonomic level
match_taxa_to_refdb <- function(x, rdb, taxa_level, wi_taxa=NULL, wi_taxa_level=NULL, method='lv', ...){
  # A helper vector storing the order of taxa
  taxa_order <- c(1:5) |> setNames(c('species', 'genus', 'family', 'order', 'class'))
  if(!taxa_level %in% names(rdb)) {
    stop(paste(taxa_level, "does not exist as a column inside the reference database!"))
  }
  # If requested, restricting the reference database to the specified search taxa
  if(!is.null(wi_taxa)){
    # print(paste0(" --- Restricting search to ", wi_taxa_level, ': ', wi_taxa))
    ref_dbase <- restrict_refdb(rdb, wi_taxa, wi_taxa_level)
  }else{
    ref_dbase <- rdb
  }
  # Vector of relevant names from the ref table
  refvec <- unique(ref_dbase[,taxa_level][[1]])
  # Finding best match with fishbase
  matchindex <- amatch(x, refvec, method=method, ...)
  # Calculating the pairwise edit distance between the source string and it's
  # best match (if no match, this returns NA)
  dist <- stringdist(x, refvec[matchindex], method = method)
  # Getting the upper-taxonomy classes for this match (upto class)
  upper_taxa <- get_upper_taxonomy(refvec[matchindex], ref_dbase, taxa_level)
  names(upper_taxa) <- paste0(names(upper_taxa), '_match')
  # Return as a list
  return(bind_cols(list('src' = x, 'match'=refvec[matchindex], 'dist'=dist), upper_taxa))
}

# A function that finds the best matching taxa across several databases and
# returns the best one, with the corresponding distance threshold
get_best_match <- function(x, rdbs, taxa_level, wi_taxa=NULL, wi_taxa_level=NULL, method='lv', maxDist=3, ...){
  # A helper vector storing the order of taxa
  taxa_order <- c(1:5) |> setNames(c('species', 'genus', 'family', 'order', 'class'))

  # Argument checks
  # if(any(is.na(x))) {
  #   stop("One or more of the input taxa are NA! Please remove these.")
  # }
  if(!(taxa_level %in% names(taxa_order))) {
    stop(paste0("Invalid value passed to `taxa_level`! Must be one of:", 
                paste0(names(torder), collapse=', ')))
  }
  if(!is.null(wi_taxa_level) && (taxa_order[taxa_level] > taxa_order[wi_taxa_level])){
    stop(paste0("Cannot search for a ", taxa_level, " under a ", 
                wi_taxa_level, '!'))
  }
  
  # Finding best match from each database
  # print("Searching databases for the best match...")
  # if(!is.null(wi_taxa)) print(paste0(" --- Restricting search to ", wi_taxa_level, ': ', wi_taxa))
  reslist <- imap(rdbs, \(rdb, y){
    # print(paste("Searching", y, "..."))
    match_taxa_to_refdb(x, rdb, taxa_level, wi_taxa, wi_taxa_level, 
                        maxDist=maxDist, method=method, ...)
  })
  # Converting results to a tabular format
  out_tab <- imap_dfr(reslist, \(x, y){
    as_tibble(x) |> 
      mutate('database' = y, .before='match')
  })
  
  # If all matches are NA, returning an empty table
  if(all(is.na(out_tab$match))){
    # print(paste("No matches found!"))
    best <- out_tab |> 
      head(1) |> 
      mutate(database = NA_character_) |> 
      mutate(matched_using=taxa_level)
    # names(best) <- paste0(names(best), '_', taxa_level)
    return(best)
  }
  
  # Otherwise returning only the best one with its distance - if multiple have
  # the same minimum distance, returning based on the database names (which
  # should be ordered)
  best <- out_tab |> 
    arrange(src) |> 
    group_by(src) |> 
    group_modify(\(x, ...){
      x |> 
        # Removing rows with NA matches
        filter(!is.na(match)) |> 
        # Getting the row with the minimum distance match for each input
        filter(dist == min(dist)) |> 
        # For multiple matches, arranging by database name (should be ordered by
        # priority) and returning the first one
        arrange(database) |> 
        head(1)
    }) |> 
    ungroup() |> 
    mutate(matched_using=taxa_level)
  # names(best) <- paste0(names(best), '_', taxa_level)
  return(best)
}

# Get a table of the matched taxanomic information above the current level,
# given the tabular output of a get_best_matches call
get_matched_upper_taxa <- function(matchobj, curr_level){
  upper_taxa_table <- matchobj |> select(matches('_?match$'))
  names(upper_taxa_table) <- names(upper_taxa_table) |> 
    str_remove_all('_match') |> 
    str_replace_all('match', curr_level)
  return(upper_taxa_table)
}

# A function that recurisively searches for matches at a given level, by
# restricting search within higher taxanomic levels. For a specified
# level-table, it recursively until a match is found. It calls the same
# get_best_match function under the hood, but automates the taxa-restriction
# process for several potential taxonomic orders
recursive_match_search <- function(x, rdbs, taxa_level, upper_taxa_table, i=1, ...){
  # For each level of the upper taxa table
  wi_taxa_level <- names(upper_taxa_table)[i]
  wi_taxa <- upper_taxa_table[, i][[1]]
  # Getting matches for this taxa level, restricted to the current upper taxa
  mtab <- get_best_match(x, rdbs, taxa_level, wi_taxa, wi_taxa_level, ...)
  # Renaming the final match column
  # names(mtab)[3] <- paste0(mtab$matched_using, '_match')
  # If there is a non-NA match, or we've reached the end of the upper taxa list,
  # returning the current match table
  if(nrow(mtab) > 0){
    # print(paste("Found a match under", wi_taxa_level, '-', wi_taxa))
    return(mtab)
  }else if(i == ncol(upper_taxa_table)){
    print('No matches found :( Returning the last empty match result...')
    return(mtab)
    # Otherwise calling the function again, with the iter set higher
  }else{
    # print("Moving up...")
    recursive_match_search(x, rdbs, taxa_level, upper_taxa_table, i+1, ...)
  }
}

# Helper function to determine whether to call the best-match database search
# function or the recursive search up a taxonomy grouping function, depending on
# if an upper taxa table is passed
call_match_function <- function(x, rdbs, taxa_level, upper_match_table=tibble(), upper_taxa_level=NULL, ...){
  if(nrow(upper_match_table) == 0){
    match <- get_best_match(x, rdbs, taxa_level, ...)
    # Otherwise searching recursively under higher taxa levels
  }else{
    upper_taxa_table <- get_matched_upper_taxa(upper_match_table, upper_taxa_level)
    match <- recursive_match_search(x, rdbs, taxa_level, upper_taxa_table, ...)
  }
  return(match)
}

