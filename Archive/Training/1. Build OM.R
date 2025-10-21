library(SAMSE)

# ---- Settings ----
nsim <- 10 # number of simulations
proyears <- 25 # number of projection years
Obs_Model <- MSEtool::Perfect_Info


# ---- Import Red Snapper SEDAR 73 Assessment ----
rdat_RS <- bamExtras::rdat_RedSnapper |>
  bamExtras::standardize_rdat()

# ---- Convert BAM Assessment to OM ----
OM <- BAM2MOM(rdat_RS,
                 stock_name='Red Snapper',
                 Obs=Obs_Model,
                 nsim=nsim,
                 proyears=proyears)



# ---- Map Landings and Discard Fleets ----
fleet_df <- data.frame(Code=names(OM@Fleets[[1]]))
fleet_df$Name <- c('Commercial Line',
                   'Recreational Headboat',
                   'General Recreational',
                   'Commercial Line - Discard',
                   'Recreational Headboat - Discard',
                   'General Recreational - Discard')

fleet_df$Mapping <- c(1,2,3,1,2,3)
fleet_df$Type <- 'Landing'
fleet_df$Type[grepl('\\.D', fleet_df$Code)] <- 'Discard'

# ---- Discard Mortality by Year Block ----
discard_mortality <- dplyr::bind_rows(
  data.frame(Stock='Red Snapper',
             Code='cHL',
             Year=c(1900, 2007, 2017),
             Prob_Dead=c(0.48, 0.38, 0.36)),
  data.frame(Stock='Red Snapper',
             Code='rHB',
             Year=c(1900, 2011, 2018),
             Prob_Dead=c(0.37, 0.26, 0.25)),
  data.frame(Stock='Red Snapper',
             Code='rGN',
             Year=c(1900, 2011, 2018),
             Prob_Dead=c(0.37, 0.28, 0.26))
)

# ---- Aggregate Landings and Discard Fleets -----
OM <- Aggregate_Fleets(OM, fleet_df, discard_mortality)


# ---- Spatial Structure -----
# Simple two-area model for now

# Area 1 is 10% of the total area
OM@Stocks$`Red Snapper`@Size_area_1 <- c(0.1, 0.1)

# 10% Unfished biomass in Area 1 - Equal Density
OM@Stocks$`Red Snapper`@Frac_area_1 <- c(0.1, 0.1)

# 90% of fish in Area 1 remain in Area 1 within a year
OM@Stocks$`Red Snapper`@Prob_staying <- c(0.9, 0.9)

# ---- Save OM to Disk ----
saveRDS(OM, 'Training/RS.om')

# ---- Explore the Operating Model Object ----

class(OM) # multi-fleet (stock) operating model

slotNames(OM)

OM@Name
names(OM@Fleets[[1]])

# Custom Parameters
OM@cpars[[1]][[1]] |> names() |> sort()









