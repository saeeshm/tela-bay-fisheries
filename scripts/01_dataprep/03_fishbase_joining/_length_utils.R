# Author: Saeesh Mangwani
# Date: 2024-02-07

# Description: Helper functions for the length-standardization script
# (01_standardize_lengths.R)

# ==== libraries ====
library(dplyr)

# ==== Functions ====

# Query the relevant classification database for matching from fishbase
get_fbase_ll_tabs <- function(){
  txn <- fb_tbl('classification_tree', server='fishbase') |> 
    select('class' = Class, 'order' = Order,'family' = Family, 
           'genus' = Genus, 'species' = Species)
  spid <- fb_tbl('species') |> select("SpecCode", "Genus", "Species")
  ll <- length_length()
  return(list('txn' = txn, 'spid' = spid, 'll' = ll))
}

# Function that takes an TL/FL table and returns an average relationship. Where
# relationships are FL regressed over TL, the function is inverted, with an
# option to drop the intercept coefficient upon inversion as this is usually
# negative when inverted and can return TL values for a given FL that don't make
# sense. Admittedly inversion is not an ideal way to deal with the TL/FL
# problem, but this rule better respects the general fact that TL is always
# larger than FL
get_mean_length_scaling <- function(lltab, rm_fl = F, drop_fl_intcp=T){
  # Separating results where the dependent variables are different (and removing
  # rows where coefficients are missing)
  tl <- lltab |> filter(Length1 == 'TL', !is.na(a), !is.na(b))
  fl <- lltab |> filter(Length1 == 'FL', !is.na(a), !is.na(b))
  # If Fl regressed on TL relationships are being used, inverting the columns
  # where FL is the dependent variable (changing the equation to make TL the
  # dependent variable again)
  if(rm_fl){
    fl_inv <- tibble()
  }else{
    fl_inv <- fl |> 
      mutate(old_a = a, old_b = b) |> 
      mutate(a = (-1*old_a)*(1/old_b)) |> 
      mutate(b = 1/old_b)
    # If the inversion intercept should be droppped, setting all "a" values to NA for this table
    if(drop_fl_intcp) fl_inv <- fl_inv |> mutate(a = NA_real_)
  }
  
  # Getting mean coefficient values across model types and returning this as a
  # table
  tl |>
    bind_rows(fl_inv) |> 
    select('intcp' = a, 'coef' = b) |> 
    summarize(
      a = mean(intcp, na.rm=T),
      b = mean(coef, na.rm=T),
      sd_a = sd(intcp, na.rm=T),
      sd_b = sd(coef, na.rm=T),
      n_a = sum(!is.na(intcp)),
      n_b = sum(!is.na(coef)),
    )
}

# A function that returns averaged LL/TL scaling coefficients for a given
# species, genus, or family using the most specific information available (meant
# to be called via pmap). Where LL data for only a higher taxa is available (i.e
# family or genus), LL/TL data for ALL genera or species within that family or
# genus are queried and meaned. This may introduce superfluous variability, but
# it is still the most standard and objective way to deal with the no-data
# problem as compared to trying to choose a representative species for every
# genus or family
calc_best_ll_factors <- function(family, genus, species, rm_fl, n_min=1, ...){
  # Creating shorter variable names, to prevent confounding with same-named
  # columns
  fam <- family; gen <- genus; spec <- species; spll <- tibble();
  
  # If a species is available, using that 
  if(!is.na(spec)){
    ll_from <- 'species'
    # Getting the species code for this species
    spec_code <- filter(fb$spid, Genus %in% gen, Species %in% spec)$SpecCode
    # Getting any LL results for this species
    spll <- fb$ll |> 
      filter(SpecCode %in% spec_code) |> 
      filter((Length1 %in% c('FL', 'TL')) & (Length2 %in% c('FL', 'TL')))
    # Removing fork-length relationships if asked
    if(rm_fl) spll <- spll |> filter(!Length1 == 'FL')
  }
  
  # If either species is not present or there are no LL data for this particular
  # species (nor rows returned above), moving up to genus
  if((nrow(spll) < n_min) & !is.na(gen)){
    ll_from <- 'genus'
    # Getting the species codes for all species in this Genus
    spec_code <- filter(fb$spid, Genus %in% gen)$SpecCode
    spll <- fb$ll |> 
      filter(SpecCode %in% spec_code) |> 
      filter((Length1 %in% c('FL', 'TL')) & (Length2 %in% c('FL', 'TL')))
    # Removing fork-length relationships if asked
    if(rm_fl) spll <- spll |> filter(!Length1 == 'FL')
  }
  
  # If there are still no LL data so far, using data for the whole family
  if((nrow(spll) < n_min)){
    ll_from <- 'family'
    # Getting all unique genera for this family
    ugen <- fb$txn |> filter(family %in% fam) |> pull(genus) |> unique()
    # Getting the species codes for all species in these genera
    spec_code <- filter(fb$spid, Genus %in% ugen)$SpecCode
    # Getting any LL results for all these species
    spll <- fb$ll |> 
      filter(SpecCode %in% spec_code) |> 
      # Filtering for only TL/FL relationships
      filter((Length1 %in% c('FL', 'TL')) & (Length2 %in% c('FL', 'TL')))
    # Removing fork-length relationships if asked
    if(rm_fl) spll <- spll |> filter(!Length1 == 'FL')
  }
  
  # Calculating the average scaling coefficients for this species using all
  # available info
  mean_spll <- get_mean_length_scaling(spll, ...) |> 
    # Setting the intercept to 0 if there is a b but it is NaN
    mutate(a = if_else(!is.na(b) & is.na(a), 0, a))
  # Joining with the taxonomic classification and returning
  bind_cols(
    list('family' = fam, 'genus' = gen, 'species' = spec, 'll_match_level' = ll_from), 
    mean_spll
  )
}
