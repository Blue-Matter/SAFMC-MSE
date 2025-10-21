
source('Run_Analysis/0b. Global_Variables.R')


## ---- importBAM ----

rdat_RS <- bamExtras::rdat_RedSnapper |> bamExtras::standardize_rdat()
rdat_GG <- bamExtras::rdat_GagGrouper |> bamExtras::standardize_rdat()
rdat_BS <- bamExtras::rdat_BlackSeaBass |> bamExtras::standardize_rdat()

## ---- SAVE_BAM_Objects ----
if (!dir.exists('BAM_Objects'))
    dir.create('BAM_Objects')
saveRDS(rdat_RS, 'BAM_Objects/BaseCase_RS.bam')
saveRDS(rdat_GG, 'BAM_Objects/BaseCase_GG.bam')
saveRDS(rdat_BS, 'BAM_Objects/BaseCase_BS.bam')


## ---- BAM2OM -----

OM_RS <- BAM2MOM(rdat_RS, stock_name='Red Snapper',
                 Obs=Obs_Model,
                 nsim=nsim, proyears=proyears)
OM_GG <- BAM2MOM(rdat_GG, stock_name='Gag Grouper',
                 Obs=Obs_Model,
                 nsim=nsim, proyears=proyears)
OM_BS <- BAM2MOM(rdat_BS, stock_name='Black Sea Bass',
                 Obs=Obs_Model,
                 nsim=nsim, proyears=proyears)


## ---- fleetnames ----

fleet_names_df <- dplyr::bind_rows(
  data.frame(Stock='Red Snapper', Fleets=names(OM_RS@Fleets[[1]])),
  data.frame(Stock='Gag Grouper', Fleets=names(OM_GG@Fleets[[1]])),
  data.frame(Stock='Black Sea Bass', Fleets=names(OM_BS@Fleets[[1]]))
)

## ---- SAVE_fleet_names_df ----
if (!dir.exists('Misc_Objects'))
  dir.create('Misc_Objects')
saveRDS(fleet_names_df, 'Misc_Objects/fleet_names_df.rda')

## ---- fleet_df ----

fleet_df <- data.frame(Code=unique(fleet_names_df$Fleets))

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
fleet_df

## ---- SAVE_fleet_df ----
saveRDS(fleet_df, 'Misc_Objects/fleet_df.rda')

## ---- RS_DiscardMort ----

discard_mortality_RS <- dplyr::bind_rows(
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

## ---- GG_DiscardMort ----
discard_mortality_GG <- dplyr::bind_rows(
  data.frame(Stock='Gag Grouper',
             Code='cHL',
             Year=1900,
             Prob_Dead=0.4),
  data.frame(Stock='Gag Grouper',
             Code='rHB',
             Year=1900,
             Prob_Dead=0.25),
  data.frame(Stock='Gag Grouper',
             Code='rGN',
             Year=1900,
             Prob_Dead=0.25)
)

## ---- BS_DiscardMort ----
discard_mortality_BS <- dplyr::bind_rows(
  data.frame(Stock='Black Sea Bass',
             Code='cHL',
             Year=c(1900, 2007),
             Prob_Dead=c(mean(c(0.14,0.19)), mean(c(0.14, 0.068)))
  ),
  data.frame(Stock='Black Sea Bass',
             Code='rHB',
             Year=1900,
             Prob_Dead=0.152),
  data.frame(Stock='Black Sea Bass',
             Code='rGN',
             Year=1900,
             Prob_Dead=0.137)
)

## ---- discard_mortality ----

discard_mortality <- dplyr::bind_rows(discard_mortality_RS,
                                      discard_mortality_GG,
                                      discard_mortality_BS)

## ---- SAVE_discard_mortality ----
saveRDS(discard_mortality, 'Misc_Objects/discard_mortality.rda')


## ---- Aggregate_Fleets ----

OM_RSTEMP <- Aggregate_Fleets(OM_RS, fleet_df, discard_mortality)

OM_RSTEMP@cpars$`Red Snapper`$`Commercial Line`$Find[1,]

OM_RSTEMP@cpars$`Red Snapper`$`Commercial Line`$V[1,,1]
OM_RSTEMP@cpars$`Red Snapper`$`Commercial Line`$V[1,,43]
OM_RSTEMP@cpars$`Red Snapper`$`Commercial Line`$retA[1,,43]

OM_RSTEMP@cpars$`Red Snapper`$`Commercial Line`$V[1,,70]
OM_RSTEMP@cpars$`Red Snapper`$`Commercial Line`$retA[1,,70]



OM_RS <- Aggregate_Fleets(OM_RS, fleet_df, discard_mortality)

OM_GG <- Aggregate_Fleets(OM_GG, fleet_df, discard_mortality)
OM_BS <- Aggregate_Fleets(OM_BS, fleet_df, discard_mortality)


## ---- spatial_map ----

require(marmap)

bathy <- marmap::getNOAA.bathy(-83, -71.37133, 23.8, 36.55028)

numbers <- data.frame(Number=1:6,
                      x=c(-79.5, -77.7, -81, -80.3, -82, -80.3),
                      y=c(33, 33, 30, 30, 25, 25))

map <- ggplot()+
  geom_raster(data=bathy, aes(x=x, y=y,fill=z)) +
  coord_sf() +
  marmap::scale_fill_etopo() +
  guides(fill='none') +
  ggnewscale::new_scale_fill() +
  geom_polygon(data =  Spatial_Area_Definition(),
               aes(x = X, y = Y, group=Region, fill=Region), alpha=0.5) +
  scale_fill_manual(values= c('#7fc97f', '#beaed4', '#fdc086')) +
  geom_contour(data=bathy, aes(x=x, y=y, z=z), breaks=c(-30),
               colour="black",linewidth=0.2) +
  geom_contour(data=bathy, aes(x=x, y=y, z=z), breaks=c(-300),
               colour="black",linewidth=0.2, linetype=2) +
  geom_text(data=numbers, aes(x=x, y=y, label=Number), size=5) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude') +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

map

## ---- save_map ----

ggsave('man/figures/spatial_map.png', map, width=7, height=5.5)


## ---- Rel_Abun_Area ----
Rel_Abun_Area_RS <- data.frame(
  Area=1:6,
  Region=c(rep('North and South Carolina',2),
           rep('Georgia - Cape Canaveral',2),
           rep('Cape Canaveral - Florida',2)),
  Depth=c('Nearshore', 'Offshore'),
  Stock='Red Snapper',
  Proportion=c(0.095,	0.048, 0.497, 0.309, 0.020, 0.031)
)

Rel_Abun_Area_GG <- data.frame(
  Area=1:6,
  Region=c(rep('North and South Carolina',2),
           rep('Georgia - Cape Canaveral',2),
           rep('Cape Canaveral - Florida',2)),
  Depth=c('Nearshore', 'Offshore'),
  Stock='Gag Grouper',
  Proportion=c(0.318,	0.298, 0.148,	0.182, 0.025, 0.029)
)

Rel_Abun_Area_BS <- data.frame(
  Area=1:6,
  Region=c(rep('North and South Carolina',2),
           rep('Georgia - Cape Canaveral',2),
           rep('Cape Canaveral - Florida',2)),
  Depth=c('Nearshore', 'Offshore'),
  Stock='Black Seabass',
  Proportion=c(0.449, 0.055, 0.415, 0.047, 0.027, 0.008)
)


## ---- SAVE_Rel_Abun_Area ----
saveRDS(Rel_Abun_Area_RS, 'Misc_Objects/Rel_Abun_Area_RS.rda')
saveRDS(Rel_Abun_Area_GG, 'Misc_Objects/Rel_Abun_Area_GG.rda')
saveRDS(Rel_Abun_Area_BS, 'Misc_Objects/Rel_Abun_Area_BS.rda')

## ---- Add_Spatial ----

OM_RS <- Add_Spatial_to_OM(OM_RS, Rel_Abun_Area_RS)

OM_GG <- Add_Spatial_to_OM(OM_GG, Rel_Abun_Area_GG)

OM_BS <- Add_Spatial_to_OM(OM_BS, Rel_Abun_Area_BS)

## ---- RecDevs ----

OMList <- Generate_Correlated_Rec_Devs(list(OM_RS, OM_GG, OM_BS), truncsd=2)

Plot_Correlated_Rec_Devs(OMList)

OM_RS <- OMList[[1]]
OM_GG <- OMList[[2]]
OM_BS <- OMList[[3]]


## ---- Save_OMs ----
if (!dir.exists('OM_Objects'))
  dir.create('OM_Objects')

saveRDS(OM_RS, 'OM_Objects/BaseCase_RS.OM')
saveRDS(OM_GG, 'OM_Objects/BaseCase_GG.OM')
saveRDS(OM_BS, 'OM_Objects/BaseCase_BS.OM')

## ---- plotRecDevs ----
p <- Plot_Correlated_Rec_Devs(OMList)

ggsave('man/figures/rec_devs.png', p, width=8, height=2.66)

## ---- compareRS ----

Hist_RS <- Simulate(OM_RS, silent = TRUE, nsim=2)
Compare_Biomass(Hist_RS, rdat_RS)

## ---- plotcompareRS ----
p <- Compare_Biomass(Hist_RS, rdat_RS)
ggsave('man/figures/RS_compare.png', p, height=4, width=5)

## ---- compareGG ----
Hist_GG <- Simulate(OM_GG, silent = TRUE, nsim=2)
Compare_Biomass(Hist_GG, rdat_GG)

## ---- plotcompareGG ----
p <- Compare_Biomass(Hist_GG, rdat_GG)
ggsave('man/figures/GG_compare.png', p, height=4, width=5)


## ---- compareBS ----
Hist_BS <- Simulate(OM_BS, silent = TRUE, nsim=2)
Compare_Biomass(Hist_BS, rdat_BS)

## ---- plotcompareBS ----
p <- Compare_Biomass(Hist_BS, rdat_BS)
ggsave('man/figures/BS_compare.png', p, height=4, width=)





