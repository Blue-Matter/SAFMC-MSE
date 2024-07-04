
source('Run_Analysis/Global_Variables.R')

fleet_df <- readRDS('inst/fleet_df.rda')
discard_mortality <- readRDS('inst/discard_mortality.rda')

Rel_Abun_Area_RS <- readRDS('inst/Rel_Abun_Area_RS.rda')
Rel_Abun_Area_GG <- readRDS('inst/Rel_Abun_Area_GG.rda')
Rel_Abun_Area_BS <- readRDS('inst/Rel_Abun_Area_BS.rda')


# Lower Natural Mortality ----

## ---- low_M_RS ----

OM_Low_M_RS <- BAM2MOM(modify_adult_M('RedSnapper', 0.07, 'Low_M_RS'),
                       stock_name='Red Snapper',
                       Obs=Obs_Model,
                       nsim=nsim, proyears=proyears) |>
  Aggregate_Fleets(fleet_df, discard_mortality) |>
  Add_Spatial_to_OM(Rel_Abun_Area_RS)


## ---- low_M_GG ----

OM_Low_M_GG <- BAM2MOM(modify_adult_M('GagGrouper', 0.10, 'Low_M_GG'),
                       stock_name='Gag Grouper',
                       Obs=Obs_Model,
                       nsim=nsim, proyears=proyears) |>
  Aggregate_Fleets(fleet_df, discard_mortality) |>
  Add_Spatial_to_OM(Rel_Abun_Area_GG)


## ---- low_M_BS ----

OM_Low_M_BS <- BAM2MOM(modify_adult_M('BlackSeaBass', 0.22, 'Low_M_BS'),
                       stock_name='Black Sea Bass',
                       Obs=Obs_Model,
                       nsim=nsim, proyears=proyears) |>
  Aggregate_Fleets(fleet_df, discard_mortality) |>
  Add_Spatial_to_OM(Rel_Abun_Area_BS)

## ---- low_M_recDevs ----

OMList <- Generate_Correlated_Rec_Devs(list(OM_Low_M_RS,
                                            OM_Low_M_GG,
                                            OM_Low_M_BS),
                                       truncsd=2)
OM_Low_M_RS <- OMList[[1]]
OM_Low_M_GG <- OMList[[2]]
OM_Low_M_BS <- OMList[[3]]


## ---- Save_low_M_OMs ----

saveRDS(OM_Low_M_RS, 'OM_Objects/Low_M_RS.OM')
saveRDS(OM_Low_M_GG, 'OM_Objects/Low_M_GG.OM')
saveRDS(OM_Low_M_BS, 'OM_Objects/Low_M_BS.OM')


# Higher Natural Mortality ----

## ---- high_M_RS ----

OM_High_M_RS <- BAM2MOM(modify_adult_M('RedSnapper', 0.15, 'High_M_RS'),
                       stock_name='Red Snapper',
                       Obs=Obs_Model,
                       nsim=nsim, proyears=proyears) |>
  Aggregate_Fleets(fleet_df, discard_mortality) |>
  Add_Spatial_to_OM(Rel_Abun_Area_RS)


## ---- high_M_GG ----

OM_High_M_GG <- BAM2MOM(modify_adult_M('GagGrouper', 0.25, 'High_M_GG'),
                        stock_name='Gag Grouper',
                        Obs=Obs_Model,
                        nsim=nsim, proyears=proyears) |>
  Aggregate_Fleets(fleet_df, discard_mortality) |>
  Add_Spatial_to_OM(Rel_Abun_Area_GG)


## ---- high_M_BS ----

OM_High_M_BS <- BAM2MOM(modify_adult_M('BlackSeaBass', 0.60, 'High_M_BS'),
                       stock_name='Black Sea Bass',
                       Obs=Obs_Model,
                       nsim=nsim, proyears=proyears) |>
  Aggregate_Fleets(fleet_df, discard_mortality) |>
  Add_Spatial_to_OM(Rel_Abun_Area_BS)

## ---- high_M_recDevs ----

OMList <- Generate_Correlated_Rec_Devs(list(OM_High_M_RS,
                                            OM_High_M_GG,
                                            OM_High_M_BS),
                                       truncsd=2)
OM_High_M_RS <- OMList[[1]]
OM_High_M_GG <- OMList[[2]]
OM_High_M_BS <- OMList[[3]]


## ---- Save_high_M_OMs ----

saveRDS(OM_High_M_RS, 'OM_Objects/High_M_RS.OM')
saveRDS(OM_High_M_GG, 'OM_Objects/High_M_GG.OM')
saveRDS(OM_High_M_BS, 'OM_Objects/High_M_BS.OM')


# Reduced Recreational Effort ----

## ---- lower_rec_effort_RS -----

OM_lower_rec_effort_RS <- BAM2MOM(lower_rec_effort('RedSnapper', 0.4, 'Lower_Rec_Effort_RS'),
                        stock_name='Red Snapper',
                        Obs=Obs_Model,
                        nsim=nsim, proyears=proyears) |>
  Aggregate_Fleets(fleet_df, discard_mortality) |>
  Add_Spatial_to_OM(Rel_Abun_Area_RS)

## ---- lower_rec_effort_GG -----

OM_lower_rec_effort_GG <- BAM2MOM(lower_rec_effort('GagGrouper', 0.4, 'Lower_Rec_Effort_GG'),
                                  stock_name='Gag Grouper',
                                  Obs=Obs_Model,
                                  nsim=nsim, proyears=proyears) |>
  Aggregate_Fleets(fleet_df, discard_mortality) |>
  Add_Spatial_to_OM(Rel_Abun_Area_GG)



## ---- lower_rec_effort_BS -----

OM_lower_rec_effort_BS <- BAM2MOM(lower_rec_effort('BlackSeaBass', 0.4, 'Lower_Rec_Effort_BS'),
                                  stock_name='Black Sea Bass',
                                  Obs=Obs_Model,
                                  nsim=nsim, proyears=proyears) |>
  Aggregate_Fleets(fleet_df, discard_mortality) |>
  Add_Spatial_to_OM(Rel_Abun_Area_BS)

## ---- lower_rec_effort_recDevs ----

OMList <- Generate_Correlated_Rec_Devs(list(OM_lower_rec_effort_RS,
                                            OM_lower_rec_effort_GG,
                                            OM_lower_rec_effort_BS),
                                       truncsd=2)
OM_lower_rec_effort_RS <- OMList[[1]]
OM_lower_rec_effort_GG <- OMList[[2]]
OM_lower_rec_effort_BS <- OMList[[3]]


## ---- Save_high_M_OMs ----

saveRDS(OM_lower_rec_effort_RS, 'OM_Objects/Lower_Rec_Effort_RS.OM')
saveRDS(OM_lower_rec_effort_GG, 'OM_Objects/Lower_Rec_Effort_GG.OM')
saveRDS(OM_lower_rec_effort_BS, 'OM_Objects/Lower_Rec_Effort_BS.OM')

