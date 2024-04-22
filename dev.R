
# install.packages('pak')

# pak::pkg_install('nikolaifish/bamExtras')
# pak::pkg_install('blue-matter/MSEtool')


library(bamExtras)
library(MSEtool)

source('Build_Package/functions.R')

# 1. Set OM Specifications
#
# 2. Import SEDAR assessments from bamExras package and convert into openMSE
#    operating models (OMs)
#

# ---- OM Specifications ----

# MSE Parameters
nsim <- 50 # number of simulations
proyears <- 20 # number of projection years

# CAL bins (mm) - need to be the same for all stocks
bin_width <- 50
CAL_bins <- seq(from=0, to=1500, by=bin_width)
CAL_mids <- seq(0.5*bin_width, by=bin_width, length.out=length((CAL_bins)-1))

# Observation Model
# currently perfect obs as data aren't being used in the MPs
# can be changed later

Obs_Model <- Perfect_Info


# ----- Import SEDAR Assessments and Create OMs ----

## ---- Red Snapper ----

RS_OM <- bamExtras::rdat_RedSnapper |>
  bamExtras::standardize_rdat() |>
  BAM2MOM(stock_name='Red Snapper')

## ---- Gag Grouper ----

GG_OM <- bamExtras::rdat_GagGrouper |>
  bamExtras::standardize_rdat() |>
  BAM2MOM(stock_name='Gag Grouper')


## ---- Black Sea Bass ----

BSB_OM <- bamExtras::rdat_BlackSeaBass |>
  bamExtras::standardize_rdat() |>
  BAM2MOM(stock_name='Black Sea Bass')


# ---- Define Fleet Structure ----

# Assumption: fleets with same name are identical across stocks

fleet_names <- c(names(RS_OM@Fleets[[1]]),
                 names(GG_OM@Fleets[[1]]),
                 names(BSB_OM@Fleets[[1]]))

fleet_df <- data.frame(Code=unique(fleet_names))

fleet_df$Name <- c('Commercial Line',
                   'Recreational Headboat',
                   'General Recreational',
                   'Commercial Line - Discard',
                   'Recreational Headboat - Discard',
                   'General Recreational - Discard',
                   'Commercial Dive',
                   'Commercial Pot',
                   'Commercial General - Discard')

fleet_df$Mapping <- c(1,2,3,1,2,3,4,1,1)
fleet_df$Type <- 'Landing'
fleet_df$Type[grepl('\\.D', fleet_df$Code)] <- 'Discard'


# ---- Define Discard Mortality -----

# need to define discard mortality by fleet and year
# need to inflate effort to account for fish that were caught and discarded alive

# SEDAR 73 Table 6
names(RS_OM@Fleets[[1]])

rdat <- bamExtras::rdat_RedSnapper |>
  bamExtras::standardize_rdat()

ind <- which(grepl('D.mort', names(rdat$parms)))
rdat$parms[ind]

discard_mortality_RS <- dplyr::bind_rows(
  data.frame(Stock='Red Snapper',
             Code='cHL',
             Year=c(1900, 2007, 2017),
             DiscM=c(0.48, 0.38, 0.36)),
  data.frame(Stock='Red Snapper',
             Code='rHB',
             Year=c(1900, 2011, 2018),
             DiscM=c(0.37, 0.26, 0.25)),
  data.frame(Stock='Red Snapper',
             Code='rGN',
             Year=c(1900, 2011, 2018),
             DiscM=c(0.37, 0.28, 0.26))
)

# SEDAR 71
# Commercial = 0.4
# Rec & Headboats = 0.25
names(GG_OM@Fleets[[1]])

rdat <- bamExtras::rdat_GagGrouper |>
  bamExtras::standardize_rdat()

ind <- which(grepl('D.mort', names(rdat$parms)))
rdat$parms[ind]

discard_mortality_GG <- dplyr::bind_rows(
  data.frame(Stock='Gag Grouper',
             Code='cHL',
             Year=1900,
             DiscM=0.4),
  data.frame(Stock='Gag Grouper',
             Code='rHB',
             Year=1900,
             DiscM=0.25),
  data.frame(Stock='Gag Grouper',
             Code='rGN',
             Year=1900,
             DiscM=0.25)
)

# SEDAR 76 - Section 2.2.1
names(BSB_OM@Fleets[[1]])
rdat <- bamExtras::rdat_BlackSeaBass |>
  bamExtras::standardize_rdat()

ind <- which(grepl('Dmort', names(rdat$parms)))
rdat$parms[ind]

discard_mortality_BSB <- dplyr::bind_rows(
  data.frame(Stock='Black Sea Bass',
             Code='cHL',
             Year=1900,
             DiscM=0.19),
  data.frame(Stock='Black Sea Bass',
             Code='cPT',
             Year=c(1900,2007),
             DiscM=c(0.14, 0.068)),
  data.frame(Stock='Black Sea Bass',
             Code='rHB',
             Year=1900,
             DiscM=0.152),
  data.frame(Stock='Black Sea Bass',
             Code='rGN',
             Year=1900,
             DiscM=0.137)
)

discard_mortality <- dplyr::bind_rows(discard_mortality_RS,
                                      discard_mortality_GG,
                                      discard_mortality_BSB)



# ---- Aggregate Fleets ----

rdat <- bamExtras::rdat_RedSnapper |>
  bamExtras::standardize_rdat()

Compare_Biomass(RS_OM, rdat)

RS_OM_agg <- Aggregate_Fleets(RS_OM, fleet_df, discard_mortality)


Compare_Biomass(RS_OM_agg, rdat)

################################################################################
# Discard M is not calcualted correctly.
################################################################################

## Compare Fs in final year

MOM_temp <- MOM
MOM_temp@nsim <- 2
Hist <- SimulateMOM(MOM_temp, parallel = FALSE, silent=TRUE)


MOM2 <- Aggregate_Fleets(MOM, fleet_df, discard_mortality)
MOM2@nsim <- 2
Hist2 <- SimulateMOM(MOM2, parallel = FALSE, silent=TRUE)

nyears <- MOM@Fleets$`Red Snapper`$cHL@nyears
nage <- MOM@Stocks[[1]]@maxage + 1
F_orig <- matrix(0, nage, nyears)
nf <- length(Hist[[1]])
for (f in 1:nf) {
  F_orig <- F_orig + Hist[[1]][[f]]@AtAge$F.Mortality[1,,,1]
}

F_agg <- matrix(0, nage, nyears)
nf <- length(Hist2[[1]])
for (f in 1:nf) {


  F_agg <- F_agg + Hist2[[1]][[f]]@AtAge$F.Mortality[1,,,1]
}

for (i in 1:70) {
  plot(F_orig[,i], type='l')
  lines(F_agg[,i], col='blue')
  readline(paste(i, 'enter'))
}

# need to fix the total Fs #
MOM2@cpars[[1]][[f]]$retA[1,,1]
MOM2@cpars[[1]][[f]]$V[1,,1]


F_orig[,i]/F_agg[,i]










MOM2 <- Aggregate_Fleets(MOM, fleet_df)
MOM2@nsim <- 2
Hist2 <- SimulateMOM(MOM2, parallel = FALSE, silent=TRUE)
Compare_Biomass(Hist2, rdat)


GG_OM_agg <- Aggregate_Fleets(GG_OM, fleet_df)
BSB_OM_agg <- Aggregate_Fleets(BSB_OM, fleet_df)



Hist <- RS_OM
rdat <- bamExtras::rdat_RedSnapper |>
  bamExtras::standardize_rdat()

Hist <- GG_OM
rdat <- bamExtras::rdat_GagGrouper |>
  bamExtras::standardize_rdat()

Hist <- BSB_OM
rdat <- bamExtras::rdat_BlackSeaBass |>
  bamExtras::standardize_rdat()

Compare_BAM <- function(Hist, rdat) {
  Hist <- run_simulations(Hist)

  Compare_N(Hist, rdat)
  Compare_Biomass(Hist, rdat)
  Compare_F(Hist, rdat)
  Compare_Landings(Hist, rdat)



}

###### TESTING #########

# Red Snapper

## Before Aggregation

# Biomass



## After Aggregation
# Biomass

# Removals

######### DEBUG ###########
rdat <- bamExtras::rdat_RedSnapper |>
  bamExtras::standardize_rdat()

MOM <- BAM2MOM(rdat, stock_name='Red Snapper')

MOM_temp <- MOM
MOM_temp@nsim <- 2
Hist <- SimulateMOM(MOM_temp, parallel = FALSE, silent=TRUE)

Compare_Biomass(Hist, rdat)


MOM2 <- Aggregate_Fleets(MOM, fleet_df, discard_mortality)
MOM2@nsim <- 2
Hist2 <- SimulateMOM(MOM2, parallel = FALSE, silent=TRUE)
Compare_Biomass(Hist2, rdat)



nyears <- MOM@Fleets$`Red Snapper`$cHL@nyears
nage <- MOM@Stocks[[1]]@maxage + 1
F_orig <- matrix(0, nage, nyears)
nf <- length(Hist[[1]])
for (f in 1:nf) {
  F_orig <- F_orig + Hist[[1]][[f]]@AtAge$F.Mortality[1,,,1]
}

F_agg <- matrix(0, nage, nyears)
nf <- length(Hist2[[1]])
for (f in 1:nf) {
  F_agg <- F_agg + Hist2[[1]][[f]]@AtAge$F.Mortality[1,,,1]
}

for (i in 1:70) {
  plot(F_orig[,i], type='l')
  lines(F_agg[,i], col='blue')
  readline(paste(i, 'enter'))
}





Hist[[1]][[1]]@TSdata$Find[1,]


MOM@cpars$`Red Snapper`[[1]]$V |> range()
MOM@cpars$`Red Snapper`[[2]]$V |> range()
MOM@cpars$`Red Snapper`[[3]]$V |> range()
MOM@cpars$`Red Snapper`[[4]]$V |> range()





print_vrange <- function(MOM) {
  nfleet <- length(MOM@cpars[[1]])
  for (fl in 1:nfleet) {
    print(fl)
    print(MOM@cpars[[1]][[fl]]$V |> range())
  }
}

