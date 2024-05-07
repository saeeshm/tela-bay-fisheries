# Author: Saeesh Mangwani
# Date: 2024-05-02

# Description: Multispecies assessments of the catch-composition in Tela-Bay

# ==== libraries ====
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

# ==== Paths and global variables ====

# Database (path) - Tela Bay
tela_path <- 'data/dbase_tela_clean.csv'

# ==== Reading data ====
tela_og <- read_csv(tela_path) |> 
  mutate(year = year(fecha)) |> 
  mutate(month = factor(month.abb[month(fecha)], levels=month.abb))

# Frequencies by species - heavily skewed (clearly some more important than
# others)
tela_og |> 
  mutate(grpvar = nombre_cientifico) |> 
  filter(!is.na(grpvar)) |> 
  group_by(grpvar) |> 
  summarize(num_obs = n()) |> 
  arrange((num_obs)) %>%
  mutate(grpvar = factor(grpvar, levels=pull(., grpvar))) |> 
  ggplot(aes(y = grpvar, fill=grpvar, x=num_obs)) +
  geom_col(colour='black', alpha=0.8, show.legend = F) +
  theme_minimal()

# Top-5, by year
tela_og |> 
  filter(!is.na(year)) |> 
  mutate(grpvar = nombre_cientifico) |> 
  filter(!is.na(grpvar)) |> 
  group_by(year) |> 
  group_modify(\(x, ...){
    x |> 
      group_by(grpvar) |> 
      summarize(num_obs = n()) |> 
      arrange(num_obs) |> 
      tail(5)
  }) |> 
  ungroup() |> 
  ggplot(aes(y = grpvar, fill=grpvar, x=num_obs)) +
  geom_col(colour='black', alpha=0.8, show.legend = F) +
  facet_wrap('year') +
  theme_minimal()

# ==== LFQs overall per gear ====
tela_og |> select(year, tipo_arte) |> table()

# Reclassifying gear types and keeping onyl relevant ones
tela <- tela_og |> 
  filter(!is.na(longitud)) |> 
  filter(!is.na(tipo_arte)) |> 
  filter(!tipo_arte %in% 'Arpon') |> 
  mutate(tipo_arte = ifelse(tipo_arte == 'Nasa', 'Trasmallo', tipo_arte)) |> 
  mutate(tipo_arte = case_when(
    tipo_arte == 'Trasmallo' & is.na(luz_malla) ~ 'Trasmallo-Unknown',
    tipo_arte == 'Trasmallo' & luz_malla >= 3 ~ 'Trasmallo-3',
    tipo_arte == 'Trasmallo' & luz_malla < 3 ~ 'Trasmallo-2',
    T ~ tipo_arte
  ))

# Mean size by gear (overall)
meanLens <- tela |> 
  filter(tipo_arte != 'Trasmallo-Unknown') |> 
  group_by(tipo_arte) |> 
  summarize('mlen' = mean(longitud, na.rm = T)) |> 
  mutate(mlen=round(mlen, 2))

# Size distribution by gear - total
tela |> 
  filter(tipo_arte != 'Trasmallo-Unknown') |> 
  ggplot(aes(x = longitud)) +
  geom_histogram(fill='white', colour='black', bins=40) +
  geom_vline(data = meanLens, aes(xintercept = mlen), 
             colour='red', linetype='dashed', linewidth=0.8) +
  geom_text(data = meanLens |> 
              mutate(lab=paste(mlen, 'cm')), 
            aes(x = mlen, label=lab),
            y=750, nudge_x = 20, size=3) +
  facet_wrap('tipo_arte', nrow=2, scales = 'fixed') +
  theme_minimal()

# Size distribution by gear - by family, v different
tela |> 
  filter(tipo_arte != 'Trasmallo-Unknown') |> 
  ggplot(aes(x = longitud)) +
  # geom_histogram(fill=NA, colour='black', bins=40) +
  geom_histogram(aes(fill=family), 
                 colour='black', bins=40) +
  # geom_density(show.legend = F) +
  # geom_density(aes(fill = genus), show.legend = F) +
  facet_wrap('tipo_arte', nrow=2)

# Log-log bin frequency classification ----------

# Database subset for size-based analysis
sdat <- tela |> 
  filter(!is.na(longitud)) |> 
  select(tipo_arte, nombre_cientifico, longitud, peso)
# Min and max sizes
lmin <- floor(min(sdat$longitud))
lmax <- ceiling(max(sdat$longitud))
# Grouping sizes into Log-width bins (using a log-base of 2)
logBase <- 1.2
binBreaks <- logBase^seq(1:100)
validBins <- sum(lmax > binBreaks) + 1
binBreaks <- binBreaks[1:validBins]
binBreaks
binLabs <- binBreaks-lag(binBreaks)
sdat |> 
  mutate(lClass = cut(longitud, breaks = binBreaks, labels = binLabs)) |> 
  mutate(lClass = as.numeric(as.character(lClass))) |> 
  # mutate(normSize = lClass/binBreaks) |>
  ggplot() +
  # geom_histogram(aes(x = lClass)) +
  geom_bar(aes(x = lClass))

# ==== CPUE ====

# Sampling variability
tela |> 
  filter(!is.na(fecha)) |> 
  select(year, 'var'=tipo_arte, codigo) |> 
  # select(year, 'var'=tipo_arte, codigo) |> 
  group_by(year, var) |> 
  summarize('n'=length(unique(codigo, na.rm=T))) |> 
  tidyr::pivot_wider(names_from='var', values_from='n', names_sort = T)

# CPUE trend per gear
tela |> 
  # filter(codigo=='CB016') |> 
  # View()
  filter(tipo_arte != 'Trasmallo-Unknown') |> 
  filter(!is.na(fecha)) |>
  mutate(peso = peso/1000) |> 
  # Summarizing total catches by trip, per-gear
  group_by(codigo, tipo_arte) |> 
  summarize(
    # Some codigos have multiple date-times as a typo, so using the most-common
    # dates so as not to confuse the summarization (any given trip can only
    # refer to one day)
    fecha = names(which.max(table(fecha))),
    peso = sum(peso, na.rm=T),
    # longitud = mean(longitud, na.rm=T),
    # Sometimes multiple hours are reported, choosing the most common one
    horas_pesca = suppressWarnings(max(unique(horas_pesca), na.rm=T))
  ) |> 
  mutate(horas_pesca = ifelse(is.infinite(horas_pesca), NA_real_, horas_pesca)) |> 
  # Trend in Catch-per-day (CPUE) per gear
  group_by(fecha, tipo_arte) |> 
  summarize(
    peso = mean(peso),
    horas_pesca = mean(horas_pesca)
  ) |> 
  ungroup() |> 
  mutate(year = year(fecha)) |> 
  mutate(month = factor(month.abb[month(fecha)], levels=month.abb)) |> 
  mutate(ymth = paste(year, month, sep='-')) |> 
  mutate(fecha = ymd(fecha)) |> 
  ggplot(aes(x = fecha, y=peso)) +
  geom_line() +
  # geom_smooth(method = 'lm', colour='firebrick', alpha=0.2,
  #             linewidth=0.5) +
  # geom_boxplot(aes(x = ymth, y = peso, group=ymth)) +
  facet_wrap('tipo_arte', nrow=2) +
  theme_minimal()

# Calale dists

# Scientific names
fam <- 'Lutjanidae'
gen <- 'Lutjanus'
spc <- 'synagris'
# Common name
cmn <- 'calale|lane'

calale <- tela |> 
  filter((str_detect(genus, gen) & str_detect(species, spc)) |
           str_detect(nombre_comun_cln, cmn)) 

# Comparing empirical Linf to estimated based on the VBGF
Lmax <- max(calale$longitud[which(anomalize::iqr(calale$longitud) == 'No')])
Linf_emp <- exp(0.044 + 0.9841*log(Lmax))
# Linf_emp <- 34
Linf_emp
curr_linf <- Linf_emp
# Calculating Length-at-first-maturity, length-at-optimum-yield and therefore
# the length range of optimal catch, and length of megaspawners
Lmat <- exp((0.8979 * log(curr_linf)) - 0.0782)
Lopt <- exp((1.0421 * log(curr_linf)) - 0.2742)
opt_range <- c(Lopt - Lopt*0.1, Lopt + Lopt*0.1)
mspawn_length <- Lopt+Lopt*0.1
# ymax <- 
calale |> 
  filter(str_detect(tipo_arte, 'Trasmallo-2')) |> 
  # filter(year %in% c(2015:2017)) |>
  # filter(year %in% c(2020:2023)) |>
  ggplot() +
  geom_histogram(aes(x=longitud), fill='white', colour='black', bins=40) +
  # Length-of maturity
  geom_vline(xintercept = Lmat, linetype='dashed') +
  coord_cartesian(ylim=c(0, 600), 
                  xlim=c(14, 56)) +
  annotate('text', label='L-Mat',
           x=Lmat, 
           y=300,
           # hjust=-0.2,
           hjust=1.2,
           size=2.5, family='serif') +
  # Optimal length range for maximum yield
  annotate('rect', 
           xmin = opt_range[1], xmax = opt_range[2], 
           ymin = -1, 
           ymax = 300,
           fill='darkgreen',
           alpha=0.2) +
  annotate('text', label='L-Mspawn',
           x=mspawn_length, 
           y=300,
           # hjust=-0.2,
           hjust=1.2,
           size=2.5, family='serif') +
  # Length of mega-spawners
  geom_vline(xintercept = mspawn_length, 
             linetype='dashed', colour='firebrick') +
  theme_minimal()
