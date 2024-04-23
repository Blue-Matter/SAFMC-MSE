
# install.packages('pak')

# pak::pkg_install('nikolaifish/bamExtras')
# pak::pkg_install('blue-matter/MSEtool')


library(bamExtras)
library(MSEtool)
library(SAMSE)


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

RS_rdat <- bamExtras::rdat_RedSnapper |>
  bamExtras::standardize_rdat()

GG_rdat <- bamExtras::rdat_GagGrouper |>
  bamExtras::standardize_rdat()

BSB_rdat <- bamExtras::rdat_BlackSeaBass |>
  bamExtras::standardize_rdat()

RS_OM <- BAM2MOM(RS_rdat, stock_name='Red Snapper')
GG_OM <- BAM2MOM(GG_rdat, stock_name='Gag Grouper')
BSB_OM <- BAM2MOM(BSB_rdat, stock_name='Black Sea Bass')


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
discard_mortality_RS <- dplyr::bind_rows(
  data.frame(Stock='Red Snapper',
             Code='cHL',
             Year=c(1900, 2008, 2017),
             DiscM=c(0.48, 0.38, 0.36)),
  data.frame(Stock='Red Snapper',
             Code='rHB',
             Year=c(1900, 2012, 2018),
             DiscM=c(0.37, 0.26, 0.25)),
  data.frame(Stock='Red Snapper',
             Code='rGN',
             Year=c(1900, 2012, 2018),
             DiscM=c(0.37, 0.28, 0.26))
)

# SEDAR 71
# Commercial = 0.4
# Rec & Headboats = 0.25
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
# data.frame(Stock='Black Sea Bass',
#            Code='cPT',
#            Year=c(1900,2007),
#            DiscM=c(0.14, 0.068)),

discard_mortality_BSB <- dplyr::bind_rows(
  data.frame(Stock='Black Sea Bass',
             Code='cHL',
             Year=c(1900, 2007),
             DiscM=c(mean(0.14,0.19), mean(0.14, 0.068))
             ),
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

RS_OM <- Aggregate_Fleets(RS_OM, fleet_df, discard_mortality)
RS_Hist <- SAMSE:::run_simulations(RS_OM)

Compare_Biomass(RS_Hist, RS_rdat)
Compare_Catch(RS_Hist, RS_rdat)
Compare_F(RS_Hist, RS_rdat)

GG_OM <- Aggregate_Fleets(GG_OM, fleet_df, discard_mortality)
GG_Hist <- SAMSE:::run_simulations(GG_OM)

Compare_Biomass(GG_Hist, GG_rdat)
Compare_F(GG_Hist, GG_rdat)
Compare_Catch(GG_Hist, GG_rdat)



BSB_OM <- Aggregate_Fleets(BSB_OM, fleet_df, discard_mortality)
BSB_Hist <- SAMSE:::run_simulations(BSB_OM)

Compare_Biomass(BSB_Hist, BSB_rdat)
Compare_F(BSB_Hist, BSB_rdat)
Compare_Catch(BSB_Hist, BSB_rdat, TRUE)


