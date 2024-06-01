# Author: Saeesh Mangwani
# Date: 2024-05-11

# Description: Summarizing results from the Calale YPR analysis (plots and tables)

# ==== libraries ====
library(stringr)
library(readr)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(flextable)
source('scripts/03_stock_analysis/utils/_general_utils.R')
source('scripts/03_stock_analysis/utils/07_ypr_assessment_utils.R')

# ==== User variables ====

# Scenario being tested:
# 'all' = all data together, with only selectivity correction applied to the
# mesh data
# 'mesh' = just mesh data with selectivity correction applied
scn <- 'mesh'
# scn <- 'all'

# Use overall total mortality or estimate separate total mortalities for each
# period
# useOverallZM <- T
useOverallZM <- F

# YPR Type - Selectivity curve, or VPA-based
ypr_type <- 'sel'
# ypr_type <- 'vpa'

# ==== Paths and global variables ====

# Directory containing YPR results
out_dir <- 'data/stock_analysis/ypr_calale/ypr'

# RDS file containing results from the VBGF fitting (contains info for the
# shared overall catch-curve)
vbgf_res_path <- paste0('data/stock_analysis/ypr_calale/vbgf_', scn, '.rds')

# Output directory for plots
plot_dir <- 'output/stock_analysis/ss_calale/ypr'

# ==== Reading relevant data ====

# Growth model results
growth <- readRDS(vbgf_res_path)

# YPR object filepaths
fpaths <- list.files(out_dir, full.names=T) |> 
  setNames(str_remove_all(list.files(out_dir), '\\.rds$'))

# Filtering only for this scenario
fpaths <- fpaths[str_detect(names(fpaths), scn)]
fpaths <- if(useOverallZM){
  fpaths[str_detect(names(fpaths), 'sharedz')]
}else{
  fpaths[str_detect(names(fpaths), 'sep_z')]
}

# Resetting names to just before/after
names(fpaths) <- str_extract(names(fpaths), 'before|after')

# ==== Required additional data ====

# Calculating froese indicators using the specified Linf from the vbgf
Linf <- growth$elefan_ga$par$Linf
froese_indics <- calc_froese_indics(Linf)

# ==== LFQ-raw and corrected mosaic ====

# Getting LFQ and selection curve plots for this scenario
lfqCorPlots <- imap(fpaths, \(x, y){
  res <- readRDS(x)
  list(
    'catch' = res$corCatchPlot + labs(title=str_to_title(y)),
    'sel' = res$selCurvePlot + guides (colour='none')
  )
})

# Converting plots to a list
lfqCorMosaic <- list(lfqCorPlots$before$catch, lfqCorPlots$before$sel, 
                     lfqCorPlots$after$catch, lfqCorPlots$after$sel) |>
  # Overalying froese indicators on each plot
  map(\(x){
    ylim_max <- Inf
    fontSize <- 2.5
    o_hjust <- -1
    o_vjust <- 2
    x +
      # Minimum catch length
      geom_vline(xintercept = 23, 
                 linetype='dashed', colour='firebrick') +
      ggplot2::annotate('text', 
                        # label='L-Min',
                        label='1.',
                        fontface = 'bold',
                        family='sans',
                        x=23, y=ylim_max,
                        vjust = o_vjust,
                        hjust= o_hjust,
                        size=fontSize) +
      # Length of maturity
      geom_vline(xintercept = froese_indics$Lmat, linetype='dashed') +
      annotate('text', label='2.',
               family='sans',
               fontface='bold',
               x=froese_indics$Lmat, y=ylim_max, 
               vjust = o_vjust,
               hjust = o_hjust,
               # hjust=1.2,
               size=fontSize) +
      # Optimal length range for maximum yield
      annotate('rect', 
               xmin = froese_indics$opt_range[1], xmax = froese_indics$opt_range[2], 
               ymin = 0, ymax = ylim_max,
               fill='darkgreen',
               alpha=0.2) +
      # Length of mega-spawners
      geom_vline(xintercept = froese_indics$LMspawn, 
                 linetype='dashed', colour='darkorange') +
      annotate('text', label='3.',
               x=froese_indics$LMspawn, y=ylim_max, 
               family='sans',
               fontface='bold',
               vjust = o_vjust,
               hjust = o_hjust,
               # hjust=1.2,
               size=fontSize) +
      scale_x_continuous(limits=c(7, 49))
  }) |> 
  # Patchwork plotting
  wrap_plots(guides='collect', axes='collect', design = 'AACC
             BBDD')
lfqCorMosaic

# ==== Catch-curves ====

# For the scenario with a shared FM, plotting the overall catch-curve
# (uses data from the VBGF result object)
if(useOverallZM){
  cctab <- get_cc_tab(vbgf_res_path)
  ccMosaic <- plot_cc(cctab)
}else{
  # Otherwise, creating the catch-curve mosaic for each period separately
  cctabs <- map(fpaths, get_cc_tab)
  # ccplots <- map(cctabs, plot_cc)
  ccplots <- imap(cctabs, \(x,y){
    plot_cc(x) +
      labs(subtitle=str_to_title(y))
  })
  ccMosaic <- ccplots$before/ccplots$after +
    plot_layout(guides='collect', axes='collect')
}
ccMosaic

# ==== F-array against reference points ====

# Preparing data and plotting
fmdats <- map(fpaths, get_f_array)
fmplots <- imap(fmdats, \(x, y){
  plotFMCurves(x, froese_indics, ylim_max = 3) +
    labs(subtitle=str_to_title(y))
})
# Producing mosaic
fmMosaic <- fmplots$before/fmplots$after +
  plot_layout(guides='collect', axes='collect')
fmMosaic

# ==== YPR plots ====
ypr <- map(fpaths, \(x){
  res <- readRDS(x)
  # Choosing only the VPA-based one for reporting
  if(ypr_type == 'sel'){
    print("Using Selectivity-based YPR")
    res$ypr_sel
  }else{
    res$ypr_vpa
  }
})

plot_ypr_mosaic <- function(ypr_before, ypr_after){
  ypr_mosaic <- wrap_elements(full=~plot(ypr_before, mark=T), clip = T)  +
    wrap_elements(full=~plot(ypr_after, mark=T, ylim=c(0, 400)), clip = ) +
    plot_layout(ncol=2, guides='collect', axes='collect')
  # plot_annotation(tag_levels = '', tag_prefix = c('Before', 'After'))
  ypr_mosaic
}
yprMosaic <- plot_ypr_mosaic(ypr$before, ypr$after)
yprMosaic

# ==== YPR tables ====

pridx <- setNames(c('2015 - 2018', '2019 - 2023'), c('before', 'after'))
ypr_tab <- imap_dfr(ypr, \(x, y){
  tibble(
    'Time Period' = pridx[y],
    'E-current' = x$currents$curr.E,
    'F-current' = x$currents$curr.F,
    'YPR-current' = x$currents$curr.Y,
    'F-Max' = x$df_Es$Fmax,
    'F-0.1' = x$df_Es$F01,
    'YPR F-Max' = x$df_Es$YPR_Fmax,
    'YPR F-0.1' = x$df_Es$YPR_F01,
  ) |> 
    mutate(across(!contains('Period'), \(x){round(x, 2)}))
})
flextable(ypr_tab)

# ==== Exporting ====

# Scenario label suffix
zm_id <- if(useOverallZM) 'sharez' else 'sepz'
slab <- paste(scn, zm_id, sep='_')

# LFQ-correction mosaic
ggsave(
  plot=lfqCorMosaic, 
  filename=file.path(plot_dir, 'lfq_sel_cor.png'),
  width=7, height=5, units = 'in', scale=1.2
)

# Catch-curves
ggsave(
  plot=ccMosaic, 
  filename=file.path(plot_dir, paste0(slab, '_catch_curve', '.png')),
  width=7, height=5, units = 'in', scale=1
)

# F-array plots
ggsave(
  plot=fmMosaic, 
  filename=file.path(plot_dir, paste0(slab, '_f_array', '.png')),
  width=7, height=5, units = 'in', scale=1.2
)

# YPR Plots
ggsave(
  plot=yprMosaic, 
  filename=file.path(plot_dir, paste0(slab, '_ypr_', ypr_type, '.png')),
  width=7, height=5, units = 'in', scale=1.2
)

# YPR Table - saved as a doc
# dir.create(file.path(plot_dir, 'tables'))
set_flextable_defaults(font.family = "serif", font.size = 11)
save_as_docx(
  flextable(ypr_tab), 
  path = file.path(plot_dir, paste0('tables/', slab, '.docx'))
)
