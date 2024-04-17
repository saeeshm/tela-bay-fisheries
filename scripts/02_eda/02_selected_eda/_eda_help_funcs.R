# Author: Saeesh Mangwani
# Date: 2024-02-12

# Description: Helper functions for exploratory data analyis

# ==== libraries ====
library(ggplot2)
library(stringr)
library(stringi)

# ==== Functions ====

# Plotting theme
qtheme <- function(size=12, font='serif'){
  theme_minimal(size, font) +
    theme(plot.background = element_rect(fill='white', colour=NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
}

# Function to find R-base colours matching a search string
findcols <- function(str){
  colors()[str_detect(colors(), str)]
}

# A generic text cleaning function
clean_strings <- function(strings){
  strings |> 
    tolower() |> 
    stri_trans_general(id = "Latin-ASCII") |> 
    str_remove_all('[:digit:]') |> 
    str_remove_all('(#|$|@|%|&|^|\\.|\\,)+') |> 
    str_squish()
}
