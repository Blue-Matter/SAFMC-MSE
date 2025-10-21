
# Load Libraries

if (!packageVersion('MSEtool') >= '4.0.0') {
  stop("MSEtool v4+ must be installed. \nInstall from Github with `pak::pkg_install('blue-matter/MSEtool')`")
}

if (!packageVersion('bamExtras') >= '0.0.2') {
  stop("bamExtras v0.0.2+ must be installed. \nInstall from Github with `pak::pkg_install('Nikolai-Klibansky/bamExtras')`")
}

library(bamExtras)
library(MSEtool)
library(ggplot2)


# Specifications

nSim <- 20 # number of simulations
pYear <- 20 # number of projection years



