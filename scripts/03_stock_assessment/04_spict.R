# Author: Saeesh Mangwani
# Date: 2024-03-25

# Description: Performing a stock-assessment with a surplus production model,
# using the SPiCT model

# ==== libraries ====
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(patchwork)
library(spict)
library(anomalize)
source('scripts/03_stock_assessment/05_spict_utils.R')

# ==== Paths and global variables ====

# Stock database path
fdb_path <- 'data/stock_assessment/lane_snapper_total.csv'

# Monthly time numeric index
mindex <- setNames(round((1/12)*c(0:11), 1), month.abb)

# Directory to store outputs
out_dir <- 'output/stock_assessment/spict'

# ==== Reading data ====
og <- read_csv(fdb_path)

# Creating base input database
fdb_full <- og |> 
  # Only Tela data
  filter(region == 'Tela') |> 
  # Removing rows where fishing hours were interpolated/guessed
  filter(!horas_pesca_intp) |>
  # filter(!is.na(tipo_arte)) |>
  # mutate(tipo_arte = ifelse(tipo_arte %in% c('Cuerda', 'Linea De Mano'), 'Linea', 'Nasa')) |>
  # filter(tipo_arte == 'Nasa') |>
  # Only relevant columns
  dplyr::select(codigo, fecha, year, month, peso, horas_pesca, numero_pescadores, tipo_arte) |> 
  # Removing rows with missing weight or effort data
  filter(!is.na(peso)) |> 
  filter(!is.na(horas_pesca)) |> 
  filter(!is.na(numero_pescadores)) |>
  # Removing anomalous caught weights 
  mutate(isanom = anomalize::iqr(peso)) |>
  filter(isanom == 'No') |>
  # Anomolous fishing hours
  mutate(isanom = anomalize::iqr(horas_pesca)) |>
  filter(isanom == 'No') |>
  # Converting weight to kg
  mutate(peso = peso/1000)

# Sampling variability by month
fdb_full |> 
  mutate(month=factor(month, levels=month.abb)) |>
  dplyr::select(year, month) |>
  table()

# Sampling variability by gear
fdb_full |> 
  dplyr::select(year, tipo_arte) |>
  table()

# Aggregating data by model time-step ----------

# Aggregating data to reflect entire quarters
fday <- fdb_full |> 
  group_by(year, month, fecha) |>
  summarize(
    # tipo_arte = unique(tipo_arte),
    peso = mean(peso, na.rm=T),
    # peso = sum(peso, na.rm=T),
    horas_pesca = mean(horas_pesca),
    # numero_pescadores = max(numero_pescadores),
    n_samples = n()
  ) |> 
  # Monthly time index for aggregation
  mutate(month_index = mindex[month]) |> 
  # Quarterly time index (used for aggregation)
  mutate(quarter_index = case_when(
    month %in% month.abb[1:3] ~ 0,
    month %in% month.abb[4:6] ~ 0.25,
    month %in% month.abb[7:9] ~ 0.5,
    month %in% month.abb[10:12] ~ 0.75,
  )) |> 
  # Creating a summarizing date variable
  # mutate(date = year+month_index) |>
  mutate(date = year+quarter_index) |>
  # Summarizing total catch, total effort, and index CPUE as a snapshot (i.e
  # average) for each quarter
  group_by(date) |> 
  summarize(
    catch=sum(peso),
    effort=sum(horas_pesca)
  ) |> 
  # Calculating CPUE as a snapshot
  mutate(cpue = catch/effort)

# Creating a second dataset meaned by isoweek before quarter aggregation - this
# aims to test for the impact sampling variability. I.e, to examine whether
# variation in catches by quarter is a result of actual variability in the catch
# or just due to variability in the sampling frequency
fiwk <- fdb_full |> 
  # Getting the isoweek for each year
  mutate(isoweek = isoweek(fecha), .after='fecha') |> 
  # Aggregating data by isoweek to remove sampling variability
  group_by(year, month, isoweek) |>
  summarize(
    peso = mean(peso, na.rm=T),
    horas_pesca = mean(horas_pesca, na.rm=T),
    n_samples = n()
  ) |>
  # Monthly time index for aggregation
  mutate(month_index = mindex[month]) |> 
  # Creating a quarterly time index (used for aggregation)
  mutate(quarter_index = case_when(
    month %in% month.abb[1:3] ~ 0,
    month %in% month.abb[4:6] ~ 0.25,
    month %in% month.abb[7:9] ~ 0.5,
    month %in% month.abb[10:12] ~ 0.75,
  )) |> 
  # Creating a summarizing date variable
  # mutate(date = year+month_index) |>
  mutate(date = year+quarter_index) |>
  # Summarizing total catch, total effort, and index CPUE as a snapshot (i.e
  # average) for each quarter
  group_by(date) |> 
  summarize(
    catch=sum(peso),
    effort=sum(horas_pesca)
  ) |> 
  # Calculating CPUE as a snapshot
  mutate(cpue = catch/effort)

# ==== Model setup ====

# Creating input objects as lists
inps_raw <- list('day' = fday, 'wk' = fiwk) |> 
  map(\(db){
    list(
      'obsC' = db$catch,
      'timeC' = db$date,
      'obsE' = db$effort,
      'timeE' = db$date
    )
  })

# Setting model options and checking inputs
inps <- map(inps_raw, \(inp){
  # inp$dtc <- round(1/12, 1) 
  # inp$dte <- round(1/12, 1) 
  # inp$dteuler <- 1/24
  # inp$eulertype <- 'soft' 
  # inp$priors$logbkfrac <- c(log(0.25),2,1)
  
  # Priors from fishbase
  inp$priors$logr <- c(log(0.87), 2, 1)
  # Scaling uncertainty of some years of data - setting 2020-21 to 5 times,
  # because the data are so unreliable. Setting 2017 to 0.75, as the sampling
  # effort this year was relatively well executed
  inp$stdevfacC <- rep(1, length(inp$obsC))
  # inp$stdevfacC[1:2] <- 2
  inp$stdevfacC[3:6] <- 0.8
  inp$stdevfacC[14:15] <- 5
  # inp$stdevfacE <- rep(1, length(inp$obsE))
  # inp$stdevfacE[3:6] <- 0.75
  # inp$stdevfacE[14:15] <- 5
  inp$optimiser.control <-  list(iter.max = 1e8, eval.max = 1e8)
  inp <- check.inp(inp)
})

# Plotting basic descriptives (time trend of catch and cpue)
plotspict.ci(inps$day)
plotspict.ci(inps$wk)

# ==== Model fitting ====

# Fixed key-parameters (the baseline model, i.e imposed assumptions on n, alpha,
# and beta following Pederson and Berg 2016 and Ono 2012).
resnull <- imap(inps, \(inp, name){
  print(name)
  inp$priors$logn <- c(log(2), 0.01, 1)
  inp$phases$logn <- -1
  inp$priors$logalpha <- c(log(1), 0.01, 1)
  inp$priors$logbeta <- c(log(1), 0.01, 1)
  fit.spict(inp)
})

# Null model is a poor description of the fishery
plot(resnull$day)
plot(resnull$wk)

# All free parameters, with default uninformative priors
res <- imap(inps, \(inp, name){
  print(name)
  fit.spict(inp)
})

# Plots
plot(res$day)
plot(res$wk)

# All free parameters, default uninformative priors, robust estimation against
# outliers
res_rob <- imap(inps, \(inp, name){
  print(name)
  inp$robflagc <- 1
  inp$robflage <- 1
  fit.spict(inp)
})
plot(res_rob$day)
plot(res_rob$wk)

# Estimating seasonality using coupled SDE approach
res_sde <- imap(inps, \(inp, name){
  print(name)
  inp$seasontype <- 2
  # inp$robflagc <- 1
  # inp$robflage <- 1
  try(fit.spict(inp))
})
plot(res_sde$day)
plot(res_sde$wk)

# Estimating seasonality using coupled SDE approach (with robust
# estimation)
res_sde_rob <- imap(inps, \(inp, name){
  print(name)
  inp$seasontype <- 2
  inp$robflagc <- 1
  inp$robflage <- 1
  try(fit.spict(inp))
})
plot(res_sde_rob$day)
plot(res_sde_rob$wk)

# ==== Diagnostics ====

# plotspict.catch(res_rob$day, ylim=c(0, 15))
my_plotspict.catch(res$day)
my_plotspict.catch(res_rob$day)
my_plotspict.catch(res_sde$day)

# We proceed here only with our pre-determined optimal choices models, the
# models utilizing catch and effort data. Residuals continue to be tested for
# both daily and weekly aggregated data to investigate consistency.
selres <- res_sde

# OSA observation residuals
obsResids <- map(selres, \(mod){
  calc.osa.resid(mod)
})
plotspict.diagnostic(obsResids$day)
plotspict.diagnostic(obsResids$wk)

# Process residuals
procResids <- map(selres, \(mod){
  calc.process.resid(mod)
})
plotspict.diagnostic.process(procResids$day)
plotspict.diagnostic.process(procResids$wk)

# Estimate correlations
significance_threshold <- 0.9
pCors <- map(selres, \(mod){
  covmx <- cov2cor(mod$cov.fixed)
  # Creating a separate matrix that identifies correlations larger than the
  # defined threshold
  signifmx <- abs(covmx) > significance_threshold
  list('cov'=covmx, 'sgnf'=signifmx)
})
# Checking only correlations shown as significant
(pCors$day$cov * pCors$day$sgnf)
(pCors$wk$cov * pCors$wk$sgnf)

# Orders of magnitude of assessment credible intervals - estimates are also
# similar and CIs overlap each other across models - improved ranges in the
# weekly aggregated model
calc.om(selres$day)
calc.om(selres$wk)

# Robustness to initial parameters
iniTest <- map(selres, \(mod){
  check.ini(mod, ntrials=30, verbose = F)
})
iniTest$day$check.ini
iniTest$wk$check.ini

# Retrospective analysis
rspct <- map(selres, \(mod){
  retro(mod, nretroyear = 5)
})
plotspict.retro(rspct$day)
plotspict.retro.fixed(rspct$day)
plotspict.retro(rspct$wk)
plotspict.retro.fixed(rspct$wk)

# Comparing weekly/daily outputs directly
plotspict.compare(
  list('day' = selres$day, 
       'week' = selres$wk
  ))

# ==== Preparing summary/presentation plots ====

# Selecting the final model to use for producing the outputs
ores <- selres$day

t1 <- get.par("logCpred", ores)[,'est'][1]
t2 <- get.par("logIp", ores)[,'est'][1]
t3 <- get.par("logB", ores)[,'est'][2]
t4 <- get.par("logqf", ores)[,'est']
exp((t4*0.0625)+t3)
t1

# Point-estimated parameters as a dataframe
mod_pars <- sumspict.parest(ores, ndigits = 3)

# Estimated timeseries of F/Fmsy and B/BMSY
fmsy <- get.par("logFFmsy", ores, exp = TRUE) |> 
  as.data.frame() %>%
  mutate(time = rownames(.), .before='ll') |> 
  as_tibble()
bmsy <- get.par("logBBmsy", ores, exp = TRUE) |> 
  as.data.frame() %>%
  mutate(time = rownames(.), .before='ll') |> 
  as_tibble()

# Plotting SPiCT summary mosaic
png(
  filename=file.path(out_dir, 'spict_summ.png'), 
  width=8, height=6, unit='in', 
  res=300,
  pointsize=12
)
par(mfrow=c(2,2))
plotspict.ffmsy(ores)
plotspict.bbmsy(ores)
plotspict.production(ores)
my_plotspict.catch(ores)
dev.off()

# Observation residual diagnostics
png(
  filename=file.path(out_dir, 'spict_diag_obs.png'), 
  width=8, height=6, unit='in', 
  res=300,
  pointsize=12
)
plotspict.diagnostic(calc.osa.resid(ores))
dev.off()

# Process residual diagnostics
png(
  filename=file.path(out_dir, 'spict_diag_proc.png'), 
  width=8, height=6, unit='in', 
  res=300,
  pointsize=12
)
plotspict.diagnostic.process(calc.process.resid(ores))
dev.off()

# Saving spict result object
saveobjs <- list(
  'mod' = ores,
  'ftab' = fmsy,
  'btab' = bmsy
)
write_rds(saveobjs, file = file.path(out_dir, 'best_spict.rds'))
