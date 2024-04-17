# Author: Saeesh Mangwani
# Date: 2024-03-25

# Description: Surplus production model in continous time

# ==== libraries ====
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(spict)
library(anomalize)

# ==== Paths and global variables ====

# Stock database path
fdb_path <- 'data/stock_assessment/lane_snapper_total.csv'

# Monthly time numeric index
mindex <- setNames(round((1/12)*c(0:11), 2), month.abb)

# ==== Reading data ====
fdb_og <- read_csv(fdb_path)

# Creating base input database
fdb <- fdb_og |> 
  # Only Tela data
  filter(region == 'Tela') |> 
  # Only relevant columns
  dplyr::select(fecha, year, month, peso, horas_pesca) |> 
  # Removing rows with missing weight or effort data
  filter(!is.na(peso)) |> 
  filter(!is.na(horas_pesca)) |> 
  # Removing anomalous caught weights 
  mutate(isanom = anomalize::iqr(peso)) |>
  filter(isanom == 'No') |> 
  # Removing rows where both effort values were estimated
  # filter(!horas_intp_dos) |> 
  # Converting weight to kg
  mutate(peso = peso/1000) |> 
  # Getting the isoweek for each year
  mutate(isoweek = isoweek(fecha), .after='fecha')
  # Getting a table of isoweeks used per month
  # mutate(month=factor(month, levels=month.abb)) |>
  # pull(isoweek) |>
  # hist(breaks=52)
  # table()
  # Estimating sampling effort variability by month now - it's much less, but
  # still present (this is a problem as it distorts the estimate of total catch
  # due simple to the difference in sampling effort, which does not necessarily
  # reflect a difference in overall actual catch)
  # mutate(month=factor(month, levels=month.abb)) |>
  # dplyr::select(year, month) |>
  # table()


# Aggregating data by timestep ----------

# Aims to account for sampling variability, since catches are summed across
# each time period so we don't want that catches in years where there was
# simply more sampling look inflated relative to the catches in other years.
# The choice of date or week is the one that is up for debate, so I am testing
# both. Both datasets are ultimately still summed by quarter before fitting.

test |> 
  mutate(year = year(fecha)) |> 
  mutate(month = month(fecha)) |> 
  # mutate(month = factor(month, levels=month.abb)) |> 
  select(year, month) |> 
  table()

# Creating a dataset meaned by day
fday <- fdb |> 
  group_by(year, month, fecha) |>
  summarize(
    peso = mean(peso, na.rm=T),
    horas_pesca = mean(horas_pesca, na.rm=T),
    n_samples = n()
  ) |>
  # Creating a quarterly time index (used for aggregation)
  mutate(quarter_index = case_when(
    month %in% month.abb[1:3] ~ 0,
    month %in% month.abb[4:6] ~ 0.25,
    month %in% month.abb[7:9] ~ 0.5,
    month %in% month.abb[10:12] ~ 0.75,
  )) |> 
  # Creating a monthly time index (used for aggregation)
  # mutate(month_index = mindex[month]) |> 
  # Creating a summarizing date variable
  mutate(date = year+quarter_index) |> 
  # Summarizing total catch, total effort, and index CPUE as a snapshot (i.e
  # average) for each quarter
  group_by(date) |> 
  summarize(
    catch=sum(peso),
    effort=sum(horas_pesca)
  ) |> 
  # Calculating CPUE as a snapshot per quarter
  mutate(cpue = catch/effort)

# Creating a second dataset meaned by isoweek 
fiwk <- fdb |> 
  group_by(year, month, isoweek) |>
  summarize(
    peso = mean(peso, na.rm=T),
    horas_pesca = mean(horas_pesca, na.rm=T),
    n_samples = n()
  ) |>
  # Creating a quarterly time index (used for aggregation)
  mutate(quarter_index = case_when(
    month %in% month.abb[1:3] ~ 0,
    month %in% month.abb[4:6] ~ 0.25,
    month %in% month.abb[7:9] ~ 0.5,
    month %in% month.abb[10:12] ~ 0.75,
  )) |> 
  # Creating a monthly time index (used for aggregation)
  # mutate(month_index = mindex[month]) |> 
  # Creating a summarizing date variable
  mutate(date = year+quarter_index) |> 
  # Summarizing total catch, total effort, and index CPUE as a snapshot (i.e
  # average) for each quarter
  group_by(date) |> 
  summarize(
    catch=sum(peso),
    effort=sum(horas_pesca)
  ) |> 
  # Calculating CPUE as a snapshot per quarter
  mutate(cpue = catch/effort)

# ==== Model setup ====

# Creating input objects as lists
inps <- list(fday, fiwk, fday, fiwk) |> 
  setNames(c('day_cpue', 'iwk_cpue', 'day_eff', 'iwk_eff')) |> 
  imap(\(db, name){
    if(str_detect(name, 'eff')){
      list(
        'obsC' = db$catch,
        'timeC' = db$date,
        'obsE' = db$effort,
        'timeE' = db$date
      )
    }else{
      list(
        'obsC' = db$catch,
        'timeC' = db$date,
        'obsI' = db$cpue,
        'timeI' = db$date
      )
    }
  })

# Checking inputs
inps <- map(inps, \(inp){
  check.inp(inp)
  # Increasing number of iterations (defaults are 1e5)
  # inp$optimiser.control <-  list(iter.max = 1e8, eval.max = 1e8)
  inp
})

# Plotting basic descriptives (time trend of catch and cpue)
plotspict.ci(inps$day_cpue)
plotspict.ci(inps$iwk_cpue)
plotspict.ci(inps$day_eff)
plotspict.ci(inps$iwk_eff)

# ==== Model fitting ====

# Fixed key-parameters (the baseline model, i.e imposed assumptions on n, alpha,
# and beta following Pederson and Berg 2016 and Ono 2012).
resnull <- imap(inps, \(inp, name){
  print(name)
  inp$priors$logn <- log(2)
  inp$phases$logn <- -1
  # inp$priors$logalpha <- log(1)
  # inp$priors$logbeta <- log(1)
  # inp$priors$logalpha <- c(log(1), 0.01, 1)
  # inp$priors$logbeta <- c(log(1), 0.01, 1)
  fit.spict(inp)
})
plot(resnull$day_cpue)
plot(resnull$iwk_cpue)
plot(resnull$day_eff)
plot(resnull$iwk_eff)

# All free parameters - this appears more consistent and optimal than setting
# baseline uninformative priors as per the previous set of models!
res <- imap(inps, \(inp, name){
  print(name)
  fit.spict(inp)
})

# Plots
plot(res$day_cpue)
plot(res$iwk_cpue)
plot(res$day_eff)
plot(res$iwk_eff)

# Estimating seasonality using coupled SDE approach (likely not necessary, but
# good for robustness checking)
res_sde <- imap(inps, \(inp, name){
  print(name)
  inp$robflagc <- 1
  inp$seasontype <- 2
  inp$robflage <- 1
  try(fit.spict(inp))
})
plot(res_sde$day_cpue)
plot(res_sde$iwk_cpue)
plot(res_sde$day_eff)
plot(res_sde$iwk_eff)

# All free parameters with robust estimation (tested separately on each
# observation data model)
res_rob <- imap(inps, \(inp, name){
  print(name)
  inp$robflagc <- 1
  # inp$robflagi <- 1
  inp$robflage <- 1
  fit.spict(inp)
})
plot(res_rob$day_cpue)
plot(res_rob$iwk_cpue)
plot(res_rob$day_eff)
plot(res_rob$iwk_eff)

# ==== Diagnostics ====

# We proceed here only with our pre-determined optimal choices models, the
# models utilizing catch and effort data. Residuals continue to be tested for
# both daily and weekly aggregated data to investigate consistency.
selres <- res[c('day_eff', 'iwk_eff')]
selres2 <- res_rob[c('day_eff', 'iwk_eff')]

# OSA observation residuals
obsResids <- map(selres, \(mod){
  calc.osa.resid(mod)
})
plotspict.diagnostic(obsResids$day_eff)
plotspict.diagnostic(obsResids$iwk_eff)

# Process residuals - minor autocorrelation issue.
procResids <- map(selres, \(mod){
  calc.process.resid(mod)
})
plotspict.diagnostic.process(procResids$day_eff)
plotspict.diagnostic.process(procResids$iwk_eff)

# Does using robust estimation with seasonality fitting 2 help account for this?
procResids2 <- map(selres2, \(mod){
  calc.process.resid(mod)
})
plotspict.diagnostic.process(procResids2$day_eff)
plotspict.diagnostic.process(procResids2$iwk_eff)

# Estimate correlations
significance_threshold <- 0.9
pCors <- map(selres, \(mod){
  covmx <- cov2cor(selres$day_eff$cov.fixed)
  # Creating a separate matrix that identifies correlations larger than the
  # defined threshold
  signifmx <- abs(covmx) > significance_threshold
  list('cov'=covmx, 'sgnf'=signifmx)
})
# Plotting only correlations shown as significant - none!
(pCors$day_eff$cov * pCors$day_eff$sgnf) |> 
  heatmap()
(pCors$day_eff$cov * pCors$iwk_eff$sgnf) |> 
  heatmap()

# Orders of magnitude of assessment credible intervals - estimates are also
# similar and CIs overlap each other across models - improved ranges in the
# weekly aggregated model
calc.om(selres$day_eff)
calc.om(selres$iwk_eff)

# Robustness to initial parameters
iniTest <- map(selres, \(mod){
  check.ini(mod, ntrials=30, verbose = F)
})
# Pretty solid robustness! Only 1 off estimation for the weekly dataset, but it
# returned a non-sensical m-value of 0
iniTest$day_eff$check.ini
iniTest$iwk_eff$check.ini

# Retrospective analysis
rspct <- map(selres, \(mod){
  retro(mod, nretroyear = 5)
})
plotspict.retro(rspct$day_eff)
plotspict.retro.fixed(rspct$day_eff)
plotspict.retro(rspct$iwk_eff)
plotspict.retro.fixed(rspct$iwk_eff)


