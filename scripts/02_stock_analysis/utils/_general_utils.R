# Author: Saeesh Mangwani
# Date: 2024-05-07

# Description: General helper functions for all stock analysis tasks

# ==== libraries ====
library(ggplot2)

# ==== Paths and global variables ====

# Custom plotting theme
qtheme <- function(size=12, font='serif'){
  theme_minimal(size, font) +
    theme(plot.background = element_rect(fill='white', colour=NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
}

# GGsave function with default options set
myggsave <- function(p, path, w=7, h=5, scale=1.2, units='in', ...){
  ggsave(plot = p, filename = path, width = w, height = h, 
         scale = scale, units = units, ...)
}
