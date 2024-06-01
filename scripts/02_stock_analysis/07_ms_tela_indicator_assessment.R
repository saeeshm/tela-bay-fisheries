# Author: Saeesh Mangwani
# Date: 2024-05-02

# Description: Multispecies assessments of the catch-composition in Tela-Bay

# ==== libraries ====
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(patchwork)
library(ggpmisc)
library(purrr)
source('scripts/02_stock_analysis/utils/_general_utils.R')
source('scripts/02_stock_analysis/utils/07_ms_utils.R')

# ==== Paths and global variables ====

# Full database path
tela_path <- 'data/stock_analysis/tela_sa_raw.csv'
# LFQ-adjusted data path
lfq_adj_path <-  'data/stock_analysis/tela_lfq_adjust.csv'
# Trophic-level crosswalk table
troph_cwalk_path <- 'data/fishbase_tables/taxa_trophic_level_cwalk.csv'

# Output plot directory
out_dir <- 'output/stock_analysis/ms_tela'

# ==== Reading data ====
tela_og <- read_csv(tela_path) |> 
  mutate(year = year(fecha)) |> 
  mutate(month = factor(month.abb[month(fecha)], levels=month.abb))

# LFQ-adjusted data
lfqAdj_og <- read_csv( 'data/stock_analysis/tela_lfq_adjust.csv') |> 
  mutate(year = year(fecha)) |> 
  mutate(month = factor(month.abb[month(fecha)], levels=month.abb))

# Trophic-level crosswalk
troph_cwalk <- read_csv(troph_cwalk_path) |> 
  mutate(species = ifelse(is.na(species), 'sp', species)) |> 
  mutate(nombre_cientifico = paste(genus, species))

# ==== Preparing data for analysis ====

# Getting all length observations as a vector
lenvec <- lfqAdj_og |> 
  dplyr::select(long_bin, adjCount) |> 
  pmap(function(long_bin, adjCount){
    rep(long_bin, adjCount)
  }) |> 
  unlist()

# Identifying lengths that are outliers
isanom <- anomalize::iqr(lenvec, alpha=0.05)
outLens <- lenvec[isanom=='Yes']
outLens

# Preparing data
lfqAdj <- lfqAdj_og |> 
  # Creating year and month columns
  mutate(year = year(fecha)) |> 
  mutate(month = month.abb[month(fecha)]) |> 
  mutate(month = factor(month, levels=month.abb)) |> 
  # Adding a period variable to designate before and after periods
  mutate(period = case_when(
    # year %in% 2015:2017 ~ 'before',
    # year %in% 2021:2023 ~ 'after',
    year %in% 2015:2018 ~ 'Before',
    year %in% 2019:2023 ~ 'After',
    T ~ NA_character_
  )) |> 
  mutate(period=factor(period, levels=c('Before', 'After'))) |> 
  # Removing outlier lengths
  filter(!long_bin %in% outLens)

tela <- tela_og |> 
  # Creating year and month columns
  mutate(year = year(fecha)) |> 
  mutate(month = month.abb[month(fecha)]) |> 
  mutate(month = factor(month, levels=month.abb)) |> 
  # Adding a period variable to designate before and after periods
  mutate(period = case_when(
    # year %in% 2015:2017 ~ 'before',
    # year %in% 2021:2023 ~ 'after',
    year %in% 2015:2018 ~ 'Before',
    year %in% 2019:2023 ~ 'After',
    T ~ NA_character_
  )) |> 
  mutate(period=factor(period, levels=c('Before', 'After'))) |> 
  # Removing outlier lengths
  filter(!long_bin %in% outLens)

# ==== Sampling variability ====

# Table of sampled observations by year and gear
tela |> select(year, tipo_arte) |> table()

# Sampled trips per year and gear
tela |> 
  select(year, 'var'=tipo_arte, codigo) |> 
  # select(year, 'var'=tipo_arte, codigo) |> 
  group_by(year, var) |> 
  summarize('n'=length(unique(codigo, na.rm=T))) |> 
  tidyr::pivot_wider(names_from='var', values_from='n', names_sort = T)

# ==== Catch LFQ distributions ====

# Dataframe that joins trophic level data
lfqTroph <- lfqAdj |> 
  # Joining trophic levels
  left_join(troph_cwalk |> 
              # Using whichever trophic estimate is available, prioritizing diet
              mutate(troph_cont = ifelse(is.na(troph_diet), troph_food, troph_diet)) |> 
              dplyr::select(nombre_cientifico, troph_cont),
            by='nombre_cientifico') |> 
  # Categorizing for viz
  mutate(troph = cut(
    troph_cont, 
    breaks=seq(2, 4.8, 0.3)
    # labels=seq(2.2, 4.6, 0.4)
  ))

# Mean size for each gear (overall)
meanLens <- lfqAdj |> 
  filter(tipo_arte != 'Gillnet - Unknown') |> 
  mutate(tipo_arte = factor(tipo_arte, 
                            levels = c('Chinchorro', 'Handline', 
                                       'Gillnet - 2in', 'Gillnet - 3in'))) |> 
  group_by(period, tipo_arte) |> 
  summarize('mlen' = weighted.mean(long_bin, adjWt, na.rm = T)) |> 
  mutate(mlen=round(mlen, 2))

# LFQ Distribution by gear, coloured by Trophic level ----------
lfqGearPeriodTroph <- lfqTroph |> 
  # Removing unknown gillnet data from the plot
  filter(tipo_arte != 'Gillnet - Unknown') |>
  mutate(tipo_arte = factor(tipo_arte, 
                            levels = c('Chinchorro', 'Handline', 
                                       'Gillnet - 2in', 'Gillnet - 3in'))) |> 
  ggplot() +
  geom_bar(
    aes(
      x = long_bin,
      fill=troph_cont,
      group=troph_cont,
      weight=adjWt
    ), 
    colour='white', 
    linewidth=0.01
  ) +
  # scale_fill_brewer(palette = 'RdYlGn', direction=-1) +
  scale_fill_distiller(palette = 'RdYlGn', direction=-1) +
  # scale_fill_manual(values = cols(length(levels(lfqTroph$troph)))) +
  geom_vline(data = meanLens,
             aes(xintercept = mlen),
             colour='red', linetype='dashed', linewidth=0.8) +
  geom_text(data = meanLens |> 
              mutate(lab=paste(mlen, 'cm')),
            aes(x = mlen, label=lab),
            y=90, nudge_x = 8, size=3,
            family = 'serif') +
  facet_grid(tipo_arte ~ period, scales = 'fixed', drop=F) +
  # scale_y_continuous(limits=c(0, 350)) +
  qtheme() +
  labs(
    y = 'Caught Weight (kg)',
    x = 'Length (cm)',
    fill='Trophic Level'
  )
lfqGearPeriodTroph |> 
  myggsave(file.path(out_dir, 'lfq_period_gear_troph.png'), h=7, w=7, scale=1)

# Weighted average trophic levels by gear and period
trophRangeGear <- lfqTroph |> 
  filter(tipo_arte != 'Gillnet - Unknown') |> 
  mutate(tipo_arte = factor(tipo_arte, 
                            levels = c('Chinchorro', 'Handline', 
                                       'Gillnet - 2in', 'Gillnet - 3in'))) |> 
  group_by(period, tipo_arte) |> 
  summarize(
    mean_troph = weighted.mean(troph_cont, adjWt, na.rm=T),
    mint = min(troph_cont, na.rm=T),
    medt = median(troph_cont, na.rm=T),
    maxt = max(troph_cont, na.rm = T)
  ) |> 
  ggplot(aes(x = tipo_arte)) +
  geom_errorbar(aes(ymin = mint, ymax=maxt), colour='darkgrey', width=0.1) +
  geom_point(aes(y = mean_troph), colour='firebrick') +
  geom_line(aes(y = mean_troph, group=period), colour='firebrick') +
  facet_wrap('period', nrow=2) +
  qtheme() +
  labs(
    y = 'Trophic Level',
    x = NULL
  )
trophRangeGear |> 
  myggsave(file.path(out_dir, 'troph_range_by_gear.png'), h=5, w=7, scale=1.1)

# Extra descriptive catch-LFQ plots (not used, but insightful) ----------

# Trophic level to max-length relationship
tela |> 
  mutate(troph_cont = ifelse(is.na(troph_diet), troph_food, troph_diet)) |> 
  group_by(nombre_cientifico) |> 
  summarize(
    troph_cont = mean(troph_cont),
    maxlen = max(longitud, na.rm=T),
    q75len = quantile(longitud, probs=0.75)
  ) |> 
  # filter(troph_cont > 2.5) |> 
  ggplot(aes(x = (maxlen), y=troph_cont)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_poly_eq(use_label(c("P", "R2")), family='sans', size=3, 
               label.x = 'left')

# Species diversity indices per length bin
lfqTroph |> 
  group_by(period, long_bin) |> 
  summarize(
    div = calc_spec_index(nombre_cientifico, wt=adjWt, index='div'),
    rich = calc_spec_index(nombre_cientifico, wt=adjWt, index='rich'),
    eq = calc_spec_index(nombre_cientifico, wt=adjWt, index='eq')
  ) |> 
  ggplot(aes(x = long_bin, y=rich, colour=period)) +
  scale_colour_manual(values = c('orange', '#008080')) +
  geom_point() +
  geom_line() +
  qtheme() +
  labs(
    x = 'Length (cm)',
    y = 'Number of species',
    colour = 'Period'
  )

# Investigating mean lengths of capture per-species (within certain trophic
# level ranges)
tela |> 
  mutate(troph_cont = ifelse(is.na(troph_diet), troph_food, troph_diet)) |> 
  filter(troph_cont < 3.6) |>
  group_by(nombre_cientifico) |> 
  summarize(
    'wt' = sum(peso), 
    'wmlen' = weighted.mean(longitud, peso, na.rm=T),
    'mlen' = mean(longitud), 
    'sdlen' = sd(longitud)
  )

# ==== Biomass spectrum ====

# Database subset for size-based analysis
sdat <- tela |> 
  dplyr::select(year, tipo_arte, nombre_cientifico, longitud, peso)

# Min and max sizes (in grams)
lmin <- floor(min(tela$peso*1000))
lmax <- ceiling(max(tela$peso*1000))

# Grouping sizes into Log-width bins, returning a list containing the breaks,
# widths, and labels
logbase <- 1.5
logBins <- calc_log_bins(lmin-1, lmax, logbase)

# Calculating log-log frequency table for each logged bin in 3 scenarios
scenarios <- c('before', 'after', 'overall')
names(scenarios) <- scenarios
lltabs <- map(scenarios, \(scn){
  print(scn)
  # Current dataset - filtered based on scenario
  cdat <- if(scn=='before'){
    sdat |> 
      filter(year %in% 2015:2018)
      # filter(nombre_cientifico != 'Lutjanus synagris')
      # filter(year %in% 2015:2017)
  }else if(scn=='after'){
    sdat |> 
      filter(year %in% 2019:2023)
      # filter(year %in% 2021:2023)
  }else{
    sdat
  }
  
  # Returning log-log frequency table for this scenario
  cdat |> 
    # kg to grams
    mutate(adjWt = round(peso*1000, 0)) |>
    # Creating size bins
    mutate(lClass = cut(adjWt, breaks = logBins$breaks, labels=F)) |>
    # Calculating biomass density per bin
    group_by(lClass, .drop=F) |> 
    summarise(Biomass = sum(adjWt, na.rm=T)) %>% 
    # Getting bin metric
    mutate(bin_start = logBins$breaks[.$lClass],
           bin_end = logBins$breaks[-1][.$lClass],
           bin_width = bin_end - bin_start,
           bin_midpoint = exp((log(bin_start) + log(bin_end)) / 2),
           # Calculating Normalized biomass density
           Biomass_density = Biomass / bin_width
    ) 
})

# Plotting log-log frequencies
llplots <- map(lltabs, \(lldat){
  lldat |> 
    ggplot(aes(x = bin_midpoint, y = Biomass_density)) +
    geom_line(colour='grey50') +
    geom_point() +
    scale_x_continuous(
      limits=c(2, 12000),
      transform = scales::log_trans(base=logbase), 
      name = "Weight (grams)",
      # Axis labels to have a precision of only 2 decimal places
      labels = function(x) sprintf("%.2f", x)
    ) + 
    scale_y_continuous(
      transform = scales::log_trans(base=logbase), name = "Normalized Biomass Density",
      limits = c(1, 6000),
      labels = function(x) sprintf("%.2f", x)
    ) +
    qtheme()
})
llplots$before
llplots$after
llplots$overall

# Creating modelling datasets by only selecting biomass densities for the
# descending part of the curve
mdats <- map(lltabs, \(lldat, start_thresh){
  # Identifying where the biomass density peaks
  peak_idx <- which.max(log(lldat$Biomass_density))
  print(peak_idx)
  peak <- floor(lldat$bin_midpoint[peak_idx])
  # Only filtering data for the peak
  lldat |>
    filter(bin_midpoint >= peak) |> 
    mutate(lclass_int = 1:length(unique(lClass)))
})

# Fitting linear models
lms <- imap(mdats, \(mdat, y){
  print(y)
  # print(cdat$lclass_int)
  # Weight as the model input
  model <- mdat |> 
    mutate(bin_midpoint = log(bin_midpoint, base=logbase), 
           Biomass_density = log(Biomass_density, base=logbase)) %>%
    lm(Biomass_density ~ bin_midpoint, data=.)
  list(
    'dat' = mdat,
    # 'minLen' = minClass,
    'm' = model,
    'cf' = model$coefficients,
    'se' = sqrt(diag(vcov(model))),
    'summ' = summary(model)
  )
})

# ANCOVA - Are the slopes significantly different?
d1 <- lms$before$m$model |> mutate(period = 'Before')
d2 <- lms$after$m$model |> mutate(period = 'After')
stdat <- bind_rows(d1, d2) |> 
  mutate(period = factor(period, levels=c('Before', 'After')))
# No, they are not
m <- lm(Biomass_density ~ bin_midpoint*period, stdat)
summary(m)

# Creating plots with overlaid regression information
llplots_wlm <- map2(llplots, lms, \(p, mod){
  # Getting max-yaxis from the plot
  y_lab_pos <-  mod$dat$Biomass_density[1] + mod$dat$Biomass_density[1]*0.2
  x_lab_pos <- mod$dat$bin_midpoint[length(mod$dat$bin_midpoint) - 3.5]
  # Creating regression annotation
  annotation <- paste0(
    'B: ', round(mod$cf[2], 2), '\n',
    'SE(B): ', round(mod$se[2], 2), '\n',
    # 'P(B): ', '<0.000', '\n',
    'R^2: ', round(summary(mod$m)$adj.r.squared, 2)
    # 'P: ', '<0.000'
  )
  # Creating plot with overlaid regression
  p +
    # Adding regression line
    geom_smooth(data=mod$dat,
                aes(x = bin_midpoint, y=Biomass_density),
                method = 'lm',
                linewidth=0.6,
                # colour='black',
                colour='firebrick',
                alpha=0.4) +
    # Adding regression details
    annotate(
      geom='label',
      label=annotation,
      x=x_lab_pos,
      y=y_lab_pos,
      family='serif',
      hjust=0,
      vjust=1.2,
      size=3,
      label.padding=unit(0.6, "lines")
    )
})
# Viewing
llplots_wlm$before
llplots_wlm$after
llplots_wlm$overall

# Creating the before/after comparison mosaic
bioDensityMosaic <- (
  (llplots_wlm$before + labs(title='Before')) / (llplots_wlm$after + labs(title='After'))
) + 
  plot_layout(ncol=2, guides='collect', axes='collect')

# Exporting 
bioDensityMosaic |> 
  myggsave(file.path(out_dir, 'biomass_density.png'), scale=1.2, w=7, h=5)
  
# ==== Distribution of caught weight by species ====

# Caught weights in the overall fishery ----------

# Calculating total catches by weight and number across the whole period
ctab <- lfqAdj |> 
  mutate(grpvar = nombre_cientifico) |> 
  filter(!is.na(grpvar)) |> 
  group_by(grpvar) |> 
  # Defining total catch as number
  summarize(
    count = sum(count), 
    adjCount = sum(adjCount), 
    wt = sum(wt, na.rm=T),
    adjWt = sum(adjWt, na.rm=T),
  ) |> 
  arrange(adjWt)

# Setting order of names as a factor (to ensure both plots have the same axis)
nmidx <- unique(ctab$grpvar)
ctab <- ctab |> 
  mutate(grpvar = factor(grpvar, levels=nmidx))

# Frequency of catch by species - weight (full fishery)
catchBySpec <- ctab |> 
  ggplot(aes(y = grpvar, x=adjWt)) +
  geom_col(colour='white', fill='firebrick', alpha=0.8, show.legend = F,
           linewidth=0.2) +
  qtheme() +
  labs(
    x = 'Weight (kg)',
    y= NULL,
  )
catchBySpec |> 
  myggsave(file.path(out_dir, 'catch_wt_by_spec_overall.png'), w=7, h=7, scale=1.1)

# A vector of the top 10 species overall
topSpecs <- ctab |> arrange(adjWt) |> tail(5) |> pull(grpvar) |> as.character()

# Catches of the top-10 per period ----------

# Calculating total caught weights per period
ctab_period <- lfqAdj |> 
  mutate(grpvar = nombre_cientifico) |> 
  filter(!is.na(grpvar)) |> 
  group_by(grpvar, period) |> 
  # Defining total catch as number
  summarize(
    count = sum(count), 
    adjCount = sum(adjCount), 
    wt = sum(wt, na.rm=T),
    adjWt = sum(adjWt, na.rm=T),
  ) |> 
  arrange(adjWt) |> 
  ungroup()


# Top 10 species per period
periods <- c('Before', 'After')
top10_periods <- ctab_period |> 
  group_by(period) |> 
  group_map(\(x, ...){
    x |> arrange(adjWt) |> tail(10) |> pull(grpvar) |> as.character()
  })
names(top10_periods) <- periods

# Catches for the top-10 per period
wtPlots <- imap(top10_periods, \(x, y){
  print(y)
  lfqAdj |> 
    filter(period == y) |> 
    mutate(grpvar = nombre_cientifico) |> 
    filter(grpvar %in% x) |>
    mutate(grpvar = factor(grpvar, levels=x)) |>
    group_by(grpvar) |> 
    summarize(
      cnum = sum(adjCount, na.rm = T), 
      cwt = sum(adjWt, na.rm=T)
    ) |> 
    ungroup() |> 
    ggplot(aes(y = grpvar, fill=grpvar, x=cwt)) +
    geom_col(colour='white', 
             # fill='#008080', 
             alpha=0.8, show.legend = F) +
    scale_fill_brewer(palette = 'RdYlBu', direction=-1) +
    scale_x_continuous(limits=c(0, 700)) +
    qtheme() +
    labs(
      subtitle=y,
      x = 'Weight (kg)',
      y= NULL,
    )
})

# Before/after mosaic of top 10 catches
wtPeriodMosaic <- (wtPlots$Before/wtPlots$After) + plot_layout(axes='collect')
wtPeriodMosaic |> 
  myggsave(file.path(out_dir, 'top10_wt_per_period.png'), w=7, h=7, scale=1.0)

# Numerical descriptions ----------

# Total change in weight
bdat <- wtPlots$Before$data
adat <- wtPlots$After$data

# Total weight before and after
twt_bef <- sum(bdat$cwt)
twt_bef
twt_aft <- sum(adat$cwt)
twt_aft

# Species top in both periods
topshared <- inner_join(bdat, adat, by='grpvar', suffix = c('_bef', '_aft'))
# Species not present in the after period
anti_join(bdat, adat, by='grpvar')

# Proportional changes
pchanges <- topshared |> 
  mutate(pcbef = cwt_bef/twt_bef) |> 
  mutate(pcaft = cwt_aft/twt_aft) |> 
  mutate(pc = round((pcaft - pcbef)*100, 2))

# Average and sd of increasing changes
pchanges |> 
  filter(pc > 0) |>
  summarize(
    mean_pc = mean(pc),
    sd_pc = sd(pc)
  )

# Getting the length range of exploitation and the total caught weight for each
# species
expLenRanges <- tela |> 
  mutate(grpvar = nombre_cientifico) |> 
  group_by(grpvar, period) |> 
  summarize(
    wt = sum(peso),
    mlen = mean(longitud, na.rm=T),
    wmlen = weighted.mean(longitud, peso, na.rm=T),
    minlen = min(longitud, na.rm=T),
    maxlen = max(longitud, na.rm=T),
  ) |> 
  mutate(rlen = maxlen-minlen)

# Plotting exploited length range vs log caught weight regression
expLenVsWt <- expLenRanges |>   
  filter(rlen != 0) |> 
  # ggplot(aes(x = rlen, y=(wt))) +
  ggplot(aes(y = rlen, x=log(wt))) +
  geom_point() +
  geom_smooth(method='lm', colour='firebrick', linewidth=0.6) +
  stat_poly_eq(use_label(c("eq", "P", "R2")), family='sans', size=3.5, 
               label.x  = 'left') +
  facet_wrap('period') +
  qtheme(12, 'serif') +
  labs(
    y = 'Exploited length range (cm)',
    x = 'Log weight (kg)'
  )
expLenVsWt |> 
  myggsave(file.path(out_dir, 'expl_lens_vs_caught_wt.png'))

# ==== Timeseries trends ====

# Data to plot TS trends per gear
tsDat <- lfqAdj |> 
  # Removing unknown gillnet data from the plot
  filter(tipo_arte != 'Gillnet - Unknown') |>
  # Refactorizing gear type variable
  mutate(tipo_arte = factor(tipo_arte, 
                            levels = c('Chinchorro', 'Handline', 
                                       'Gillnet - 2in', 'Gillnet - 3in'))) |> 
  mutate(cpue = adjWt/tot_boats) |> 
  # Editing dates to allow summarization by month
  mutate(fecha = ymd(paste(year, month, '01', sep='-'))) |>
  group_by(fecha, tipo_arte) |> 
  summarize(
    peso = sum(adjWt, na.rm=T),
    tot_boats = sum(tot_boats, na.rm=T),
    cpue = mean(cpue, na.rm=T),
    # Diversity indices - richness, diversity, equity
    rich = calc_spec_index(specvec = nombre_cientifico, wt=adjWt, index='rich'),
    divr = calc_spec_index(nombre_cientifico, wt=adjWt, index='div'),
    eq = calc_spec_index(nombre_cientifico, wt=adjWt, index='eq'),
  ) |> 
  ungroup() |> 
  # Remaking year and period variables
  mutate(year = year(fecha)) |>
  mutate(period = case_when(
    year %in% 2015:2018 ~ 'before',
    year %in% 2019:2023 ~ 'after',
    T ~ NA
  ))

# CPUE trends by gear ----------
cpueByGear <- tsDat |> 
  ggplot(aes(x = fecha, y=cpue)) +
  geom_point(alpha=0.4, show.legend = F) +
  geom_line(alpha=0.8, show.legend = F) +
  facet_wrap('tipo_arte', nrow=2) +
  scale_y_continuous(limits=c(0, 5)) +
  geom_smooth(method='lm', colour='firebrick', linewidth=0.5) +
  stat_poly_eq(use_label(c("P", "R2")), family='sans', size=3, 
               label.x = 'right') +
  qtheme() +
  labs(
    x = NULL,
    y = 'CPUE (kg/day)'
  )
cpueByGear |> 
  myggsave(file.path(out_dir, 'ms_cpue_by_gear.png'), w=7, h=5, scale=1.2)

# Calculating overall CPUE across the entire fishery
ovCpueTab <- lfqAdj |> 
  filter(nombre_cientifico %in% tail(topSpecs, 5)) |> 
  mutate(cpue = adjWt/tot_boats) |> 
  # Summarizing total catches by trip, per-gear
  mutate(fecha = ymd(paste(year, month, '01', sep='-'))) |>
  group_by(fecha) |> 
  summarize(
    cpue = mean(cpue, na.rm=T),
  ) |> 
  ungroup() |> 
  mutate(nombre_cientifico = 'Entire Fishery')

# Overall CPUE for top-5 species
top5CpueTab <- lfqAdj |> 
  filter(nombre_cientifico %in% tail(topSpecs, 5)) |> 
  mutate(cpue = adjWt/tot_boats) |> 
  # Summarizing total catches by trip, per-gear
  mutate(fecha = ymd(paste(year, month, '01', sep='-'))) |>
  group_by(fecha, nombre_cientifico) |> 
  summarize(
    cpue = mean(cpue, na.rm=T),
  ) |> 
  ungroup()
  
# Grid of overall vs top-5 CPUE ----------
cpueGrid <- top5CpueTab |> 
  bind_rows(ovCpueTab) |> 
  mutate(nombre_cientifico = factor(nombre_cientifico, 
                                    levels=c('Entire Fishery', unique(top5CpueTab$nombre_cientifico)))) |> 
  mutate(fecha = ymd(fecha)) |>
  mutate(year = year(fecha)) |> 
  ggplot(aes(x = fecha, y=cpue)) +
  geom_point(alpha=0.4, show.legend = F) +
  geom_line(alpha=0.8, show.legend = F) +
  scale_color_brewer(palette = 'Set1') +
  facet_wrap('nombre_cientifico', nrow=3, scales='free_y') +
  geom_smooth(method='lm', colour='firebrick', linewidth=0.5) +
  stat_poly_eq(use_label(c("P", "R2")), family='sans', size=3, 
               label.x = 'left') +
  qtheme() +
  labs(
    x = NULL,
    y = 'CPUE (kg/day)'
  )
cpueGrid |> 
  myggsave(file.path(out_dir, 'cpue_trends_top.png'), w=7, h=7, scale=1)

# Values of CPUE for the top 5 (they are quite different from each other)
lfqAdj |> 
  filter(nombre_cientifico %in% tail(topSpecs, 5)) |> 
  mutate(cpue = adjWt/tot_boats) |> 
  # Summarizing total catches by trip, per-gear
  mutate(fecha = ymd(paste(year, month, '01', sep='-'))) |>
  group_by(nombre_cientifico) |> 
  summarize(
    cpue = mean(cpue, na.rm=T),
  ) |> 
  ungroup()

# Misc diversity explorations ----------

# Species evenness trend over time 
tsDat |> 
  # mutate(eq = ifelse(is.nan(eq), 0, eq)) |> 
  ggplot(aes(x = fecha, y=eq)) +
  geom_point(alpha=0.4, show.legend = F) +
  geom_line(aes(), alpha=0.8, show.legend = F) +
  geom_smooth(method='lm', colour='black', linewidth=0.3) +
  stat_poly_eq(use_label(c("P", "R2")), family='sans', size=3, 
               label.x = 'right', label.y = 'bottom') +
  scale_y_continuous(limits=c(0, 1)) +
  # scale_color_brewer(palette = 'Set1') +
  facet_wrap('tipo_arte', nrow=2) +
  qtheme() +
  labs(
    x = NULL,
    y = "Pielou's Evenness Index (H/H-max)"
  )

# Relationship between species richness and catch-size
tsDat |> 
  ggplot(aes(x = rich, y=peso)) +
  geom_smooth(aes(colour=tipo_arte), 
              method='lm',
              se=T,
              alpha=0.3, show.legend = F) +
  scale_color_brewer(palette = 'Set1') +
  geom_point(aes(colour=tipo_arte), alpha=0.8, show.legend = F) +
  facet_wrap('tipo_arte', nrow=2) +
  qtheme() +
  labs(
    x = 'Catch richness (number of unique species)',
    y = 'Catch size (kg)'
  )

# ==== Extra/misc explorations ====

# Which months had no average diversity?
noDiv_dates <- tsDat |> 
  filter(divr == 0) |> 
  pull(fecha)

# What was the total caught weight and average length of catch for the 10 most
# caught species, before and after
lfqAdj |> 
  filter(nombre_cientifico %in% topSpecs) |> 
  # filter(fecha %in% noDiv_dates) |> 
  # filter(year %in% year(noDiv_dates)) |> 
  # filter(month %in% month.abb[month(noDiv_dates)]) |> 
  # filter(year < 2019) |> 
  group_by(period, nombre_cientifico) |> 
  summarize('wt' = sum(adjWt),
            'len' = mean(long_bin)) |> 
  arrange(period, desc(wt)) |> 
  tidyr::pivot_wider(names_from = period, values_from=c(wt, len))

