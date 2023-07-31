
library(SAMSE)

# Location of raw data files
data.dir <- 'G:/Shared drives/BM shared/1. Projects/Snapper - Grouper SAFMC/Data/MSE data'

# MSE Parameters
nsim <- 50 # number of simulations
pyears <- 20 # number of projection years

# Observation Model
# CAL bins (mm)
bin_width <- 50
CAL_bins <- seq(from=0, to=1500, by=bin_width)
CAL_mids <- seq(0.5*bin_width, by=bin_width, length.out=length((CAL_bins)-1))

Obs_Model <- Perfect_Info



