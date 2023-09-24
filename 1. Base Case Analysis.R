
# ---- Install SAMSE R Package ----
# Install the latest version of the SAMSE R package and its dependencies

# Install `remotes` package (if required)
if(!require('remotes'))  install.packages('remotes')

# Install latest version of MSEtool Package
remotes::install_github('blue-matter/MSEtool')

# Install latest version of openMSE Package
remotes::install_github('blue-matter/openMSE')

# Install SAMSE R Package (and dependencies)
remotes::install_github('blue-matter/SAFMC-MSE')



# ---- Simulate Historical Fishery Dynamics ----
# needs to be done anytime the Operating Models have changed/been updated

library(SAMSE)

run_hist <- TRUE

if (run_hist) {
  # Simulate Base Case OM (MOM_001)
  multiHist <- SimulateMOM(MOM_001)

  # Save to disk
  if (!dir.exists('Hist_Objects'))
    dir.create('Hist_Objects')
  saveRDS(multiHist, 'Hist_Objects/001_BaseCase.hist')

} else {
  # Load from disk
  multiHist <- readRDS('Hist_Objects/001_BaseCase.hist')
}


# ----  Make DataList ----
# This creates the DataList object from the end of the historical period
# DataList is updated in the Projection period with simulated data.
# The DataList object can be used for stepping through the MP code
# to see how the management recommendations are set for the
# first projection year

DataList <- list()

for (p in 1:2) {
  DataList[[p]] <- list()
  for (f in 1:7) {
    DataList[[p]][[f]] <-multiHist[[p]][[f]]@Data
  }
}

x <- 1 # for stepping through MP code

# ----- Specify Management Procedures -----

# F for all fleets is fixed to the mean F from 2017 -- 2019
StatusQuo <- function(x, DataList, ...) {

  stocks <- unique(Fleet_Management$Stock)
  fleets <- unique(Fleet_Management$Fleet)
  nstocks <- length(stocks)
  nfleets <- length(fleets)

  # copy the internal `Fleet_Management` object
  this_Fleet_Management <- Fleet_Management

  # loop over stocks and fleets
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      # calculate mean F from 3 last historical years
      meanF <- mean(DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,68:70])
      # populate the `F` value in `this_Fleet_Management` object
      this_Fleet_Management <- this_Fleet_Management %>%
        dplyr::mutate(F=replace(F, Stock==stocks[s] &Fleet==fleets[f], meanF))
    }
  }
  # call internal `Fleet_MMP` function with `this_Fleet_Management` object
  Fleet_MMP(x, DataList, Fleet_Management=this_Fleet_Management)
}
# define as class `MMP`
class(StatusQuo) <- 'MMP'



# Overall fishing mortality is set to the respective MFMT for each stock
# Relative F of each fleet (and season) remains unchanged
Ftarget <- function(x, DataList, ...) {

  MFMT <- data.frame(Stock=c('Red Snapper', 'Gag Grouper'),
                     MFMT=c(0.21, 0.42))

  stocks <- unique(Fleet_Management$Stock)
  fleets <- unique(Fleet_Management$Fleet)
  nstocks <- length(stocks)
  nfleets <- length(fleets)

  # copy the internal `Fleet_Management` object
  this_Fleet_Management <- Fleet_Management

  # loop over stocks and fleets
  for (s in 1:nstocks) {
    for (f in 1:nfleets) {
      # calculate mean F from 3 last historical years
      meanF <- mean(DataList[[s]][[f]]@Misc$FleetPars$Fishing_Mortality[x,68:70])

      # populate the `F` value in `this_Fleet_Management` object
      this_Fleet_Management <- this_Fleet_Management %>%
        dplyr::mutate(F=replace(F, Stock==stocks[s] &Fleet==fleets[f], meanF))
    }
  }

  # Calculate relative F for each fleet (by Stock)
  this_Fleet_Management <- this_Fleet_Management %>% group_by(Stock) %>% mutate(Frat=F/sum(F))


  # Set overall F to MFMT for each stock
  this_Fleet_Management <- left_join(this_Fleet_Management, MFMT, by='Stock')
  this_Fleet_Management <- this_Fleet_Management %>% mutate(F=MFMT*Frat)


  # call internal `Fleet_MMP` function with `this_Fleet_Management` object
  Fleet_MMP(x, DataList, this_Fleet_Management)
}
# define as class `MMP`
class(Ftarget) <- 'MMP'




# ----- Run Projections -----

run_projections <- TRUE

if (run_projections) {
  # Run Projections with MPs
  MMSE <- ProjectMOM(multiHist, MPs=c('StatusQuo',
                                      'Ftarget'),
                     dropHist = FALSE)

  # Save to disk
  if (!dir.exists('MSE_Objects'))
    dir.create('MSE_Objects')
  saveRDS(MMSE, 'MSE_Objects/001_BaseCase.mmse')

} else {
  MMSE <- readRDS('MSE_Objects/001_BaseCase.mmse')
}

# ---- Time-Series Plots -----


plot_Fmort(MMSE)
ggsave('img/MSE/F.png', width=8, height=6)

plot_Catch(MMSE)

ggsave('img/MSE/Catch.png', width=8, height=6)

plot_SB(MMSE)

ggsave('img/MSE/SB.png', width=8, height=6)




# ----- Calculate Performance Metrics -----

P_MFMT(MMSE)  # Probability F < MFMT

P_MSST(MMSE) # Probability SB>MSST


Landings_10(MMSE) # mean landings in first 10 years
Landings_20(MMSE) # mean landings in last 10 years

P_rebuild(MMSE) # probability of rebuilding by end of projection period

Landings_Removals(MMSE) # mean ratio of landings to overall removals (landings + discards)

# ----- Trade-Off Plots of Performance Metrics ----

TradeOff(MMSE, c('P_MSST', 'P_MFMT'))

ggsave('img/MSE/TO_1.png', width=8, height=6)

TradeOff(MMSE, c('Landings_10', 'Landings_20'))

ggsave('img/MSE/TO_2.png', width=8, height=6)















