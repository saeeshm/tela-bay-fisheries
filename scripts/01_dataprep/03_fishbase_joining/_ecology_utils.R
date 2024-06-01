# Author: Saeesh Mangwani
# Date: 2024-02-07

# Description: Helper functions for the getting trophic levels for each species
# by querying the fishbase ecology table (02_get_species_tropic_level.R)

# ==== libraries ====
library(dplyr)

# ==== Functions ====
# Query the relevant classification database for matching from fishbase
get_relv_fbase_tabs <- function(){
  # Classification table
  txn <- fb_tbl('classification_tree', server='fishbase') |> 
    select('class' = Class, 'order' = Order,'family' = Family, 
           'genus' = Genus, 'species' = Species)
  # Table containing the species codes
  spid <- fb_tbl('species') |> select("SpecCode", "Genus", "Species")
  # Ecology table
  eco <- ecology()
  return(list('txn' = txn, 'spid' = spid, 'eco' = eco))
}

# A function that gets the trophic level associated with the input taxa. Where
# possible, the smallest classification (i.e species) is used. If however no
# data are available for the species, the trophic level is calculated as an
# average of the level for all species in the containing taxa, proceeding first
# to genus then species. This is to account for the issue where many times the
# catch is only reported at the family or genus level, and this seems to be the
# most objective and time-sensitive way to deal with this challenge rather than
# trying to pick a representative catch species for each genus and family
get_troph_level <- function(family, genus, species, n_min=1, ...){
  # Creating shorter variable names, to prevent confounding with same-named
  # columns
  fam <- family; gen <- genus; spec <- species; speco <- tibble();
  
  # If a species is available, using that 
  if(!is.na(spec)){
    eco_from <- 'species'
    # Getting the species code for this species
    spec_code <- filter(fb$spid, Genus %in% gen, Species %in% spec)$SpecCode
    # Getting any eco results for this species
    speco <- fb$eco |> 
      filter(SpecCode %in% spec_code)
  }
  
  # If either species is not present or there are no LL data for this particular
  # species (nor rows returned above), moving up to genus
  if((nrow(speco) < n_min) & !is.na(gen)){
    eco_from <- 'genus'
    # Getting the species codes for all species in this Genus
    spec_code <- filter(fb$spid, Genus %in% gen)$SpecCode
    speco <- fb$eco |> 
      filter(SpecCode %in% spec_code)
  }
  
  # If there are still no LL data so far, using data for the whole family
  if((nrow(speco) < n_min)){
    eco_from <- 'family'
    # Getting all unique genera for this family
    ugen <- fb$txn |> filter(family %in% fam) |> pull(genus) |> unique()
    # Getting the species codes for all species in these genera
    spec_code <- filter(fb$spid, Genus %in% ugen)$SpecCode
    # Getting any LL results for all these species
    speco <- fb$eco |> 
      filter(SpecCode %in% spec_code)
  }
  
  # Calculating the average trophic levels for this taxa using all available
  # info
  mean_troph <- speco |> 
    dplyr::select(FoodTroph, DietTroph) |> 
    summarize(
      'troph_food' = mean(FoodTroph, na.rm=T),
      'troph_diet' = mean(DietTroph, na.rm=T),
    )
  # Joining with the taxonomic classification and returning
  bind_cols(
    list('family' = fam, 'genus' = gen, 'species' = spec, 
         'troph_match_level' = eco_from),
    mean_troph
  )
}
