library(SAMSE)

data.dir <- 'G:/Shared drives/BM shared/1. Projects/Snapper - Grouper SAFMC/Data/MSE data'

nsim <- 50 # number of simulations
pyears <- 20 # number of projection years

bin_width <- 50
CAL_bins <- seq(from=0, to=1500, by=bin_width)
CAL_mids <- seq(0.5*bin_width, by=bin_width, length.out=length((CAL_bins)-1))


