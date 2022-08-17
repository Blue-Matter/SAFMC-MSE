# devtools::install_github("nikolaifish/bamExtras")

nsim <- 3

# ---- Generate the Base Case MOM from the latest assessments ----

library(bamExtras)
library(MSEtool)
library(ggplot2)

# ---- Load functions ----
fls <- list.files('functions')
for (f in fls) source(file.path('functions', f))

# ---- Generate individual MOMs ----
# Red snapper  MOM - SEDAR 73 March 2021
RSMOM <- BAM2MOM(rdat=rdat_RedSnapper, stock_name='Red Snapper', nsim = nsim)

# Gag MOM - SEDAR 71 April 2021
GGMOM <- BAM2MOM(rdat=rdat_GagGrouper, stock_name='Gag', nsim = nsim)


# ---- Create Fleet Data-Frames ----
nfleet <- RSMOM@Fleets[[1]] %>% length()
RS_fleets <- data.frame(i=1:nfleet,
                        code=lapply(RSMOM@Fleets[[1]], slot, 'Name') %>% unlist() %>% as.vector(),
                        name=c('Commercial handline',
                               'Recreational headboats',
                               'General recreational',
                               'Commerical handline - discards',
                               'Recreational headbots - discards',
                               'General recreational - discards'))


nfleet <- GGMOM@Fleets[[1]] %>% length()
GG_fleets <- data.frame(i=1:nfleet,
                        code=lapply(GGMOM@Fleets[[1]], slot, 'Name') %>% unlist() %>% as.vector(),
                        name=c('Commercial handline',
                               'Commercial diving',
                               'Recreational headboats',
                               'General recreational',
                               'Commerical handline - discards',
                               'Recreational headbots - discards',
                               'General recreational - discards'))

# ---- Aggregate Discard Fleets into Main Fleets ----
# This is necessary so cMPs can set single TAC per fleet and adjust
# discard mortality and/or selectivity/retention curves

# Table 6 in SEDAR 73
RS_discMort <- list(cHL=matrix(c(0.48, 2006,
                                 0.38, 2016,
                                 0.36, 2020), ncol=2, byrow=T),
                    rHB=matrix(c(0.37, 2010,
                                 0.26, 2017,
                                 0.25, 2020), ncol=2, byrow=T),
                    rGN=matrix(c(0.37, 2010,
                                 0.28, 2017,
                                 0.26, 2020), ncol=2, byrow=T)
)

# Page 53 and 55 in SEDAR 71
GG_discMort <- list(cHL=matrix(c(0.4, 2020), ncol=2, byrow=T),
                    rHB=matrix(c(0.25, 2020), ncol=2, byrow=T),
                    rGN=matrix(c(0.25, 2020), ncol=2, byrow=T)
)
# Combine discard fleets into respective main fleets
RSMOM_2 <- Combine_discard_fleets(RSMOM, RS_fleets, RS_discMort)
GGMOM_2 <- Combine_discard_fleets(GGMOM, GG_fleets, GG_discMort)

# ---- Combine RS and GG into multi-stock MOM ----

# Things to consider:
# - management interval
# - units of the TAC - weight-at-age is in whole weight lbs
# -
RS_GG_MOM <- Combine_MOM(MOMlist=list(RSMOM_2, GGMOM_2), 'Red Snapper & Gag Grouper MOM')

saveRDS(RS_GG_MOM, 'OMs/BaseCaseMOM.rda')












