# Author: Saeesh Mangwani
# Date: 2024-01-19

# Description: Exploratory data analysis replicating the template provided by
# Antonella

# ==== libraries ====
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(stringr)
library(anomalize)
library(vegan)

# ==== Paths ====

# Path to coral TS dataset
ts_path <- 'data/bica/bica_master_dbase_clean.csv'

# Path to output directory where plots will be stored
out_dir <- 'output/eda/bica'
dir.create(out_dir)

# ==== Global/helper variables ====

# Index of essential columns (useful for viewing subsets of tables)
idx <- c('id', 'codigo', 'fecha', 'comunidad', 'zona_pesca', 'nombre_comun',
         'nombre_cientifico', 'peso', 'longitud', 'horas_pesca','tipo_arte')

# Plotting theme
qtheme <- function(size=12, font='serif'){
  theme_minimal(size, font) +
    theme(plot.background = element_rect(fill='white', colour=NA),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
}

# Function to find colours matching a search string
findcols <- function(str){
  colors()[str_detect(colors(), str)]
}

# ==== Reading data ====
dat <- read_csv(ts_path)

# ==== Data preparation ====

# Preparing columns
dat <- dat |> 
  # Cleaning the nombre-comun column - REVIEW
  mutate(nombre_comun = str_to_title(str_squish(tolower(nombre_comun)))) |> 
  # Renaming the total captures column
  rename('capturas_totales' = capturas_totales_comunidad) |> 
  # Factorizing relevant columns
  mutate(comunidad = as.factor(comunidad)) |> 
  mutate(zona_pesca = as.factor(zona_pesca)) |> 
  # Getting year, month, and year-month columns
  mutate(year = year(fecha), month=month(fecha), .after='fecha') |> 
  mutate(ym = paste(year, str_pad(month, 2, 'left', '0'), sep='-'), .after=month) |> 
  mutate(month = month.abb[month]) |> 
  # Factorizing year and month column
  mutate(year = as.factor(year)) |> 
  mutate(month = factor(month, levels=month.abb)) |> 
  # Removing problematic rows where the hours fished were negative (see 2021
  # data cleaning notes)
  filter(horas_pesca > 0)

# Removing outliers - based on length
dat <- dat |> 
  group_by(nombre_cientifico) |> 
  anomalize(longitud, method = "iqr") |> 
  ungroup() |> 
  as_tibble() |> 
  # About 112 anomalies detected
  filter(anomaly == 'No') |> 
  select(-c(longitud_l1, longitud_l2, anomaly))

# ==== Distribution of fishing gear ====

mypal <- colorRampPalette(RColorBrewer::brewer.pal(6, 'RdPu'))
# mypal <- colorRampPalette(c( "#FAFAD2", "#a3aabd", "#a0d0de","#c7d4b6","#97b5cf"))
# Distribution of fishing gears used across all fish in the sample
dat |> 
  ggplot(aes(x = tipo_arte, fill=tipo_arte)) +
  # scale_fill_manual(values = mypal(8)) +
  scale_fill_brewer(palette = 'RdPu') +
  geom_bar(colour = 'black', alpha=0.9, linewidth=0.4, show.legend = F) +
  qtheme() +
  labs(
    x = NULL,
    y = 'Number of samples'
  )

# ==== Top-10 species caught ====

# Summarized data frame for plotting only the 10 most frequently caught species
top10 <- dat |> 
  filter(!is.na(nombre_comun)) |> 
  group_by(nombre_comun) |> 
  summarize(
    'num_indiv' = n(),
    'mean_length' = mean(longitud, na.rm=T),
    'sd_length' = sd(longitud, na.rm=T),
    'n_length' = sum(!is.na(longitud))
  ) |> 
  mutate(se = sd_length/n_length) |> 
  arrange(desc(num_indiv)) |> 
  head(10)

# Getting the names of the top 10 species (by catch number)
top10_index <- top10$nombre_comun

# Plot of 10-most frequently caught species in the sample
top10 |> 
  ggplot(aes(x = nombre_comun, y=num_indiv, fill=nombre_comun)) +
  geom_col(colour='black', linewidth=0.4, show.legend = F, alpha=0.9) +
  scale_fill_brewer(palette = 'RdPu') +
  qtheme() +
  labs(
    x = NULL,
    y = 'Numero de individuos'
  )
  
# Mean length by species
top10 |> 
  ggplot(aes(x = nombre_comun, y=mean_length, fill=nombre_comun)) +
  geom_col(colour='black', linewidth=0.4, show.legend = F, alpha=0.9) +
  scale_fill_brewer(palette = 'RdPu') +
  qtheme() +
  labs(
    x = NULL,
    y = 'Longitud premedio [cm]'
  )
  
# ==== Diversity of species ====

# Diversity of caught species by community ----------

# Generating count matrix
dmx <- as.matrix(table(dat$comunidad, dat$nombre_comun))
# Calculating the diversity index
div_comun <- as.table(diversity(dmx, index = "shannon", MARGIN = 1, base = exp(1)))
print(div_comun) # 

# Diversity of caught species by gear type ----------
dmx <- as.matrix(table(dat$tipo_arte, dat$nombre_comun))
div_arte <- as.table(diversity(dmx, index = "shannon", MARGIN = 1, base = exp(1)))
print(div_arte) # 

# Diversity of gear types by community ----------
dmx <- as.matrix(table(dat$comunidad, dat$tipo_arte))
div_arte_por_comun <- as.table(diversity(dmx, index = "shannon", MARGIN = 1, base = exp(1)))
print(div_arte_por_comun) 

# ==== Seasonality of caught species ====

# Counts of individuals caught per month - seasonality overall
ssn <- dat |> 
  filter(nombre_comun %in% top10_index) |> 
  ggplot(aes(x = month)) +
  geom_bar(stat='count', 
           fill='coral3',
           alpha=0.8,
           colour='black', 
           show.legend=F) +
  scale_x_discrete(drop = FALSE) +
  qtheme() +
  labs(
    x = NULL,
    y = 'Numero de individuos'
  )
ssn

# Counts of individuals caught per month, for the top 10 species - seasonality
# by species
ssn + facet_wrap('nombre_comun', nrow=5, ncol=2, scales='free')
 
# ==== Total captures/sampled biomass by year ====

# Calculating a by-year summary table - this is for an understanding of the data
# distribution
byear <- dat |> 
  mutate(kg = peso/1000) |> 
  group_by(year) |> 
  summarize(
    'peso_muest' = sum(kg, na.rm=T),
    # Assuming this is in pounds, because that's what Antonella did. So making
    # it kg
    'capt_total' = (sum(capturas_totales, na.rm = T)/2.2),
    'num_samples' = n()
  ) |> 
  mutate(capt_total = ifelse(capt_total == 0, NA_real_, capt_total)) |> 
  mutate(prop_sampled = peso_muest/capt_total)

# Summarizing by month and gear-type for plotting
dat |> 
  mutate(kg = peso/1000) |> 
  group_by(month, tipo_arte) |> 
  summarize('peso_muest' = sum(kg, na.rm=T)) |> 
  ggplot(aes(x = month, y = peso_muest, fill=tipo_arte)) +
  geom_col(position='stack', alpha=0.8, colour='black') +
  scale_fill_brewer(palette='Set2') +
  qtheme() +
  labs(
    x=NULL, 
    y='Biomasa monitoreadas [Kg]',
    fill='Arte de pesca'
  )

# ==== Catch per-unit effort ====

# Declining over time, with an uncharacteristic peak in 2021?
dat |> 
  # mutate(peso = peso/1000) |> 
  # Calculating CPUE per fishing trip per year
  group_by(month, codigo) |> 
  summarize(cpue = mean(peso/horas_pesca, na.rm=T)) |> 
  # Removing outliers
  filter(iqr(cpue) == 'No') |> 
  # Further summarizing these by year - using a median
  # group_by(year) |> 
  # summarize(cpue = median(cpue, na.rm=T)) |> 
  ggplot(aes(x = month, y=cpue)) +
  geom_boxplot(colour='black', alpha=0.8, fill='skyblue2') +
  # geom_col(colour='black', alpha=0.8, fill='skyblue2') +
  qtheme() +
  labs(
    x=NULL,
    y='Capturas por unidad de esfuerzo [g/hora]'
  )

# CPUE trend per community, by years
dat |> 
  # mutate(peso = peso/1000) |> 
  # Calculating CPUE per fishing trip per year
  group_by(comunidad, month, codigo) |> 
  summarize(cpue = mean(peso/horas_pesca, na.rm=T)) |> 
  # Removing outliers 
  filter(iqr(cpue) == 'No') |>  
  # Further summarizing these by year - using a median
  # group_by(comunidad, year) |> 
  # summarize(cpue = median(cpue, na.rm=T)) |> 
  ggplot(aes(x = month, y=cpue)) +
  geom_boxplot(colour='black', alpha=0.8, fill='skyblue2') +
  # geom_col(colour='black', alpha=0.8, fill='skyblue2') +
  qtheme() +
  facet_wrap('comunidad', nrow=4, ncol=3) +
  labs(
    x=NULL,
    y='Capturas por unidad de esfuerzo [g/hora]'
  )

# CPUE trend by top 10 species
dat |> 
  filter(nombre_comun %in% top10_index) |> 
  # mutate(peso = peso/1000) |> 
  # Calculating CPUE per fishing trip per year
  # group_by(nombre_comun, year, codigo) |> 
  # summarize(cpue = mean(peso/horas_pesca, na.rm=T)) |> 
  # Further summarizing these by month - using a median
  group_by(nombre_comun, month) |> 
  summarize(
    cpue = mean(peso/horas_pesca, na.rm=T),
    sd = sd(peso/horas_pesca, na.rm=T),
    n = n()
  ) |> 
  mutate(se = sd/n) |> 
  mutate(ci = se*1.96) |> 
  ggplot(aes(x = month, y=cpue, group=nombre_comun)) +
  # geom_col(colour='black', alpha=0.8, fill='skyblue2') +
  geom_errorbar(aes(ymin=cpue-(1.96*se), ymax=cpue+(1.96*se)),
                width=0.15) +
  geom_point() +
  geom_line() +
  qtheme() +
  facet_wrap('nombre_comun', nrow=4, ncol=3, scales='free_y') +
  labs(
    x=NULL,
    y='Capturas por unidad de esfuerzo [g/hora]'
  )

# ==== Average weight per year ====

# Overall average weight per year - similar general trend of decline with a
# spike in 2021. What happened in 2021?
dat |> 
  # mutate(year = as.numeric(as.character(year))) |> 
  group_by(month) |> 
  summarize(
    mean_wt = mean(peso, na.rm=T),
    sd_wt = sd(peso, na.rm=T),
    n = n()
  ) |> 
  ungroup() |> 
  mutate(se = sd_wt/n) |> 
  ggplot(aes(x = month, y=mean_wt)) +
  geom_errorbar(aes(ymin=mean_wt-(1.96*se), ymax=mean_wt+(1.96*se)),
                width=0.15) +
  geom_point() +
  geom_line(aes(group='month')) +
  # geom_col(colour='black', alpha=0.8, fill='skyblue2') +
  qtheme() +
  labs(
    x=NULL,
    y='Peso promedio'
  )

# Mean weight trends by species  
dat |> 
  filter(nombre_comun %in% top10_index) |> 
  group_by(month, nombre_comun) |> 
  summarize(
    mean_wt = mean(peso, na.rm=T),
    sd_wt = sd(peso, na.rm=T),
    n = n()
  ) |> 
  mutate(se = sd_wt/n) |> 
  ggplot(aes(x = month, y=mean_wt, colour=nombre_comun, group=nombre_comun)) +
  geom_errorbar(aes(ymin=mean_wt-(1.96*se), ymax=mean_wt+(1.96*se)),
                width=0.15) +
  geom_point() +
  geom_line() +
  # geom_col(colour='black', alpha=0.8, fill='skyblue2') +
  qtheme() +
  guides(colour='none') +
  facet_wrap('nombre_comun', nrow=5, ncol=2, scales='free_y') +
  labs(
    x=NULL,
    y='Peso promedio [g]'
  )

# ==== Length optimality analysis ====

# Calculating asymptotic length, length at first-maturity, and optimum length
# using the equations Froese and Binohlan (2000). Comparing these to current
# averages
optimal_lengths <- dat |> 
  filter(nombre_comun %in% top10_index) |> 
  group_by(nombre_comun) %>% 
  summarize(
    freq = n(), 
    mean = mean(longitud,na.rm = T), 
    std = sd(longitud,na.rm = T), 
    se = (std/freq),
    # Equations from Froese and Binohlan (2000)
    Linf = 10^( 0.044 + 0.9841*log10(max(longitud,na.rm = T))), 
    Lmat = 10^(0.8979*log10(Linf)-0.0782),
    Lopt = 10^(1.0421*log10(Linf)-0.2742)
  )

# Splitting the results by month for plotting
mnth_lengths <- dat |> 
  filter(nombre_comun %in% top10_index) |> 
  group_by(nombre_comun, month) %>% 
  summarize(
    freq = n(), 
    mean = mean(longitud, na.rm = T), 
    std = sd(longitud,na.rm = T), 
    se = (std/freq),
    # Equations from Froese and Binohlan (2000)
    Linf = 10^( 0.044 + 0.9841*log10(max(longitud,na.rm = T))), 
    Lmat = 10^(0.8979*log10(Linf)-0.0782),
    Lopt = 10^(1.0421*log10(Linf)-0.2742)
  )

# Plotting the percentage of sampled catches that were mature - by year
dat |> 
  filter(nombre_comun %in% top10_index) |>
  # Adding calculated length metrics for each species to the table
  left_join(optimal_lengths, by='nombre_comun') |> 
  # Calculating the percent that were mature, per year
  mutate(mature = longitud > Lmat) |> 
  group_by(month, nombre_comun) |> 
  summarize(perc_mature = (sum(mature, na.rm = T)/n())*100) |> 
  # Calculating a binary column if the percentage of maturity was higher than
  # 90% (the optimal condition)
  mutate(value = if_else(perc_mature>90, "Pos", "Neg")) |> 
  # mutate(month = as.numeric(as.character(year))) |> 
  # plotting
  ggplot(aes(x=month, y=perc_mature, fill=value, group=nombre_comun)) +  
  # Box indicating the safe/optimal threshold
  # geom_rect(aes(ymin = 90, ymax = 110, xmin = 2015-0.3, xmax = 2023+0.3), 
  #           fill= "grey80", alpha = 0.5) +
  geom_hline(yintercept = 90, linetype='dashed', colour='black') +
  # geom_line() +
  geom_point(shape = 23, size = 2, show.legend = F) +
  scale_fill_manual(values = c("firebrick", "forestgreen"))+ 
  facet_wrap('nombre_comun', ncol = 5) +
  qtheme() +
  labs(
    x = NULL, 
    y = "Porcentaje de capturas de individuos maduros")

# Percentage of sampled catches that were mature - by gear (for the top 10
# species only)
dat |> 
  filter(nombre_comun %in% top10_index) |>
  # Adding calculated length metrics for each species to the table
  left_join(optimal_lengths, by='nombre_comun') |> 
  # Calculating the percent that were mature, per year
  mutate(mature = longitud > Lmat) |> 
  group_by(tipo_arte, nombre_comun) |> 
  summarize(perc_mature = (sum(mature, na.rm = T)/n())*100) |> 
  # filter(!is.na(tipo_arte)) |> 
  # Calculating a binary column if the percentage of maturity was higher than
  # 90% (the optimal condition)
  mutate(value = if_else(perc_mature>90, "Pos", "Neg")) |> 
  # plotting
  ggplot(aes(x=tipo_arte, y=perc_mature, fill=value, group=nombre_comun)) +  
  # Box indicating the safe/optimal threshold
  # geom_rect(aes(ymin = 90, ymax = 110, xmin = 2015-0.3, xmax = 2023+0.3), 
  #           fill= "grey80", alpha = 0.5) +
  geom_hline(yintercept = 90, linetype='dashed', colour='black') +
  # geom_line() +
  geom_point(shape = 23, size = 2, show.legend = F) +
  scale_fill_manual(values = c("firebrick", "forestgreen"))+ 
  facet_wrap('nombre_comun', ncol = 5) +
  qtheme() +
  theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2)) +
  labs(
    x = NULL, 
    y = "Porcentaje de capturas de individuos maduros")

# ==== Trend in total length over time ====
dat |> 
  filter(nombre_comun %in% top10_index) |>
  ggplot(aes(x = fecha, y = longitud)) +
  geom_point(alpha=0.3) +
  geom_smooth(method='loess', colour='firebrick', alpha=0.8) +
  facet_wrap('nombre_comun', scales='free_y', nrow=5) +
  qtheme() +
  labs(x = NULL, y = 'Longitud total [cm]')

# ==== Length distributions by reported mesh size ====
dat |> 
  filter(nombre_comun %in% top10_index) |> 
  # Adding calculated length metrics for each species to the table
  left_join(optimal_lengths, by='nombre_comun') |> 
  # Removing rows where mesh size is missing
  filter(!is.na(luz_malla)) |> 
  # Converting mesh size to a factor
  mutate(luz_malla = round(luz_malla, 1)) |>
  mutate(luz_malla = as.factor(luz_malla)) |> 
  # Plotting
  ggplot(aes(x = luz_malla, y = longitud, fill=luz_malla)) +
  geom_boxplot(show.legend=F) +
  scale_fill_brewer(palette='RdYlGn') +
  geom_hline(data=optimal_lengths, aes(yintercept=Lmat), 
             colour='firebrick', linetype='dashed') +
  qtheme() +
  facet_wrap('nombre_comun', nrow=2, scales='free_y') +
  labs(
    x = 'Luz de malla [pulgadas]',
    y = 'Longitud total [cm]'
  )

# ==== Sex-distribution and gonad size ====

# Most of the data that contains gonad size information is for Calale, and all
# of it was in 2017 only - so only working with this species
gonad <- dat |> 
  # filter(nombre_comun == 'Calale') |> 
  filter(!is.na(madurez))

# Distribution by month of gonad size/maturity status
gonad |> 
  mutate(madurez = as.factor(madurez)) |> 
  ggplot(aes(x = month, fill=madurez)) +
  geom_bar(stat='count', position='stack', colour='black') +
  qtheme() +
  labs(x=NULL, y='Numero de individuos', fill='Estadio gonadal')
  # facet_wrap('sexo', nrow=1, scales='fixed')

# How many females relative to males? - to 41% to 59%  F to M
# round(table(gonad$sexo)/nrow(gonad), 2)

# Distribution over time of sexes
# gonad |> 
#   mutate(sexo = as.factor(sexo)) |> 
#   ggplot(aes(x = month, fill=sexo)) +
#   geom_bar(stat='count', position='stack', colour='black') +
#   qtheme() +
#   labs(x=NULL, y='Numero de individuos', fill='Sexo')

# ==== Time-series trends - catch, effort and CPUE ====

# Plotting dataframes containing all the aggregated metrics ----------
ts_df <- dat |> 
  # Calculating average CPUE per date/trip 
  group_by(codigo, fecha) |> 
  summarize(
    wt_avg = mean(peso, na.rm=T),
    esf_avg = mean(horas_pesca, na.rm=T),
    cpue_avg = mean(peso/horas_pesca, na.rm=T),
  ) |> 
  ungroup() |> 
  # Removing outliers
  filter(iqr(cpue_avg) == 'No', 
         iqr(esf_avg) == 'No',
         iqr(wt_avg) == 'No') |> 
  # Averaging those values by month to get a more complete TS
  # mutate(yrmth = paste0(year(fecha), '-', month(fecha))) |>
  # mutate(yrmth = ym(yrmth)) |> 
  mutate(month = month.abb[month(fecha)]) |> 
  mutate(month = factor(month, levels=month.abb)) |> 
  group_by(month) |> 
  summarize(
    # Weight
    wt = mean(wt_avg, na.rm=T),
    sd_wt = sd(wt_avg, na.rm=T),
    se_wt = sd_wt/sum(!is.na(wt_avg)),
    # Effort
    esf = mean(esf_avg, na.rm=T),
    sd_esf = sd(esf_avg, na.rm=T),
    se_esf = sd_wt/sum(!is.na(esf_avg)),
    # CPUE
    cpue = mean(cpue_avg, na.rm=T),
    sd_cpue = sd(cpue_avg, na.rm=T),
    se_cpue = sd_cpue/sum(!is.na(cpue_avg))
  ) |> 
  ungroup()

# Same but split by top10 species
ts_df_top10 <- dat |> 
  filter(nombre_comun %in% top10_index) |> 
  # Calculating average CPUE per date/trip 
  group_by(nombre_comun, fecha) |> 
  summarize(
    wt_avg = mean(peso, na.rm=T),
    esf_avg = mean(horas_pesca, na.rm=T),
    cpue_avg = mean(peso/horas_pesca, na.rm=T),
  ) |> 
  ungroup() |> 
  # Removing outliers
  # Removing outliers
  filter(iqr(cpue_avg) == 'No', 
         iqr(esf_avg) == 'No',
         iqr(wt_avg) == 'No') |>  
  # Averaging those values by month to get a more complete TS
  # mutate(yrmth = paste0(year(fecha), '-', month(fecha))) |> 
  # mutate(yrmth = ym(yrmth)) |> 
  mutate(month = month.abb[month(fecha)]) |> 
  mutate(month = factor(month, levels=month.abb)) |> 
  group_by(nombre_comun, month) |> 
  summarize(
    # Weight
    wt = mean(wt_avg, na.rm=T),
    sd_wt = sd(wt_avg, na.rm=T),
    se_wt = sd_wt/sum(!is.na(wt_avg)),
    # Effort
    esf = mean(esf_avg, na.rm=T),
    sd_esf = sd(esf_avg, na.rm=T),
    se_esf = sd_wt/sum(!is.na(esf_avg)),
    # CPUE
    cpue = mean(cpue_avg, na.rm=T),
    sd_cpue = sd(cpue_avg, na.rm=T),
    se_cpue = sd_cpue/sum(!is.na(cpue_avg))
  ) |> 
  ungroup()

# Plotting caught weight ----------

# Total
ts_df |> 
  ggplot(aes(x = month, y = wt, group='month')) +
  geom_smooth(method='lm', colour='firebrick', alpha=0.6) +
  # geom_errorbar(aes(ymin=wt-(se_wt*1.96),
  #                   ymax=wt+(se_wt*1.96)),
  #               alpha=0.8, width=0.2) +
  geom_point(alpha = 0.9) +
  geom_line() +
  qtheme() +
  labs(
    x = NULL, y='Capturas [g]'
  )

# Split by top 10 species
ts_df_top10 |> 
  ggplot(aes(x = month, y = wt, group='month')) +
  geom_smooth(method='lm', colour='firebrick', alpha=0.6) +
  # geom_errorbar(aes(ymin=wt-(se_wt*1.96),
  #                   ymax=wt+(se_wt*1.96)),
  #               alpha=0.8, width=0.2) +
  geom_point(alpha = 0.9) +
  geom_line() +
  qtheme() +
  labs(x = NULL, y='Capturas [g]') +
  facet_wrap('nombre_comun', nrow=5, scales = 'free_y')

# Plotting effort (hours fished) ----------

# Total
ts_df |> 
  ggplot(aes(x = month, y = esf, group='month')) +
  geom_smooth(method='lm', colour='firebrick', alpha=0.6) +
  # geom_errorbar(aes(ymin=esf-(se_esf*1.96),
  #                   ymax=esf+(se_esf*1.96)),
  #               alpha=0.8, width=0.2) +
  geom_point(alpha = 0.9) +
  geom_line() +
  qtheme() +
  labs(x = NULL, y='Esfuerzo [horas de pesca]')

# Split by top10 species
ts_df_top10 |> 
  ggplot(aes(x = month, y = esf, group='month')) +
  geom_smooth(method='lm', colour='firebrick', alpha=0.6) +
  # geom_errorbar(aes(ymin=esf-(se_esf*1.96),
  #                   ymax=esf+(se_esf*1.96)),
  #               alpha=0.8) +
  geom_point(alpha = 0.9) +
  geom_line() +
  qtheme() +
  labs(x = NULL, y='Esfuerzo [horas de pesca]') +
  facet_wrap('nombre_comun', nrow=5, scales = 'free_y')

# Plotting CPUE ----------

# Total
ts_df |> 
  ggplot(aes(x = month, y = cpue, group='month')) +
  geom_smooth(method='lm', colour='firebrick', alpha=0.6) +
  geom_errorbar(aes(ymin=cpue-(se_cpue*1.96), 
                    ymax=cpue+(se_cpue*1.96)),
                alpha=0.8, width=0.2) +
  geom_point(alpha = 0.9) +
  geom_line() +
  qtheme() +
  labs(x = NULL, y='Capturas por unidad de esfuerzo [g/hora]')

# Split by species (top10)
ts_df_top10 |> 
  ggplot(aes(x = month, y = cpue, group='month')) +
  geom_smooth(method='lm', colour='firebrick', alpha=0.6) +
  geom_errorbar(aes(ymin=cpue-(se_cpue*1.96), ymax=cpue+(se_cpue*1.96)),
                alpha=0.8, width=0.2) +
  geom_point(alpha = 0.9) +
  # geom_line(aes(group='month')) +
  qtheme() +
  labs(x = NULL, y='Capturas por unidad de esfuerzo [g/hora]') +
  facet_wrap('nombre_comun', nrow=5, scales = 'free_y')
