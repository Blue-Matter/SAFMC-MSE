
source('Build_Package/2. Set OM Parameters.R')

rdat <- rdat_RedSnapper
stock_name <- 'Red Snapper'
nsim
pyears

Fleet_Structure_GG <- data.frame(Name=c('Commercial Handline',
                                        'Recreational Headboat',
                                        'General Recreational',
                                        'Commercial Dive'),
                                 Landing=c(1,3,4, 2),
                                 Discard=c(5,6,7, NA))


Fleet_Structure_RS <- data.frame(Name=c('Commercial Handline',
                                        'Recreational Headboat',
                                        'General Recreational'),
                                 Landing=c(1,2,3),
                                 Discard=c(4,5,6))

Fleet_Structure <- Fleet_Structure_RS


seasonal_F_RS <- list(RS_Comm_relF, RS_HB_relF, RS_GR_relF)

seasonal_F_List <- seasonal_F_RS




# ---- Generate the Red Snapper Base Case Operating Model ----

##  Convert all weights to metric (kg) ----
rdat_RedSnapper$a.series$weight <- rdat_RedSnapper$parms$wgt.a*
  rdat_RedSnapper$a.series$length^rdat_RedSnapper$parms$wgt.b

## Import SEDAR 73 Assessment into MOM ----
RSMOM_SEDAR <- BAM2MOM(rdat=rdat_RedSnapper, stock_name='Red Snapper',
                       nsim = nsim, proyears = pyears)


## Add CALbins to cpars ----
nfleet <- length(RSMOM_SEDAR@cpars[[1]])
for (f in 1:nfleet) {
  RSMOM_SEDAR@cpars[[1]][[f]]$CAL_bins  <- CAL_bins
  RSMOM_SEDAR@cpars[[1]][[f]]$CAL_binsmid <- CAL_mids
}

## Combine Landing and Discard Fleets together into Removals ----
RSMOM_combined <- combineFleets(RSMOM_SEDAR)



## Create closure and size limit data.frame ----

### Commercial ----
## Table 2.2.1 in SEDAR 73 - Seasonal Closures
# Table 6 in SEDAR 73 - Discard mortality
current_yr <- RSMOM_combined@Fleets[[1]][[1]]@CurrentYr
nyears <- RSMOM_combined@Fleets[[1]][[1]]@nyears

Years <- rev(seq(current_yr, by=-1, length=nyears))
Years <- Years[Years>=2010]

df_list <- list()

# 2010
year <- 2010
df_list[[1]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(paste(year, '-01-01')),
                           Date_Closed=lubridate::as_date(paste(year, '-01-04')),
                           Size_Limit=MSEgraph::inch2mm(20),
                           Disc_M=0.38)
# 2011
year <- 2011
df_list[[2]] <- data.frame(Year=year,
                           Date_Open=NA,
                           Date_Closed=lubridate::as_date(paste(year, '-01-01')),
                           Size_Limit=NA,
                           Disc_M=0.38)

# 2012
year <- 2012
df_list[[3]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(c(paste(year, '-09-17'),
                                                          paste(year, '-11-13'),
                                                          paste(year, '-12-12'))),
                           Date_Closed=lubridate::as_date(c(paste(year, '-09-25'),
                                                            paste(year, '-11-22'),
                                                            paste(year, '-12-20'))),
                           Size_Limit=NA,
                           Disc_M=0.38)

# 2013
year <- 2013
df_list[[4]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(paste(year, '-08-26')),
                           Date_Closed=lubridate::as_date(paste(year, '-10-9')),
                           Size_Limit=NA,
                           Disc_M=0.38)

# 2014
year <- 2014
df_list[[5]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(paste(year, '-07-14')),
                           Date_Closed=lubridate::as_date(paste(year, '-09-10')),
                           Size_Limit=NA,
                           Disc_M=0.38)

# 2015
year <- 2015
df_list[[6]] <- data.frame(Year=year,
                           Date_Open=NA,
                           Date_Closed=lubridate::as_date(paste(year, '-01-01')),
                           Size_Limit=NA,
                           Disc_M=0.38)

# 2016
year <- 2016
df_list[[7]] <- data.frame(Year=year,
                           Date_Open=NA,
                           Date_Closed=lubridate::as_date(paste(year, '-01-01')),
                           Size_Limit=NA,
                           Disc_M=0.38)

# 2017
year <- 2017
df_list[[8]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(paste(year, '-11-02')),
                           Date_Closed=lubridate::as_date(paste(year+1, '-01-01')),
                           Size_Limit=NA,
                           Disc_M=0.36)

# 2018
year <- 2018
df_list[[9]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(c(paste(year, '-07-26'),
                                                          paste(year, '-12-05'))),
                           Date_Closed=lubridate::as_date(c(paste(year, '-11-08'),
                                                            paste(year, '-12-16'))),
                           Size_Limit=NA,
                           Disc_M=0.36)

# 2019
year <- 2019
df_list[[10]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(paste(year, '-07-08')),
                            Date_Closed=lubridate::as_date(paste(year, '-08-30')),
                            Size_Limit=NA,
                            Disc_M=0.36)

RS_Comm_Season <- do.call('rbind', df_list)

### Recreational ----
## Table 2.2.2 in SEDAR 73 - Seasonal Closures
# Table 6 in SEDAR 73 - Discard mortality
df_list <- list()

# 2010
year <- 2010
df_list[[1]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(paste(year, '-01-01')),
                           Date_Closed=lubridate::as_date(paste(year, '-12-03')),
                           Size_Limit=MSEgraph::inch2mm(20),
                           Disc_M=0.37)
# 2011
year <- 2011
df_list[[2]] <- data.frame(Year=year,
                           Date_Open=NA,
                           Date_Closed=lubridate::as_date(paste(year, '-01-01')),
                           Size_Limit=NA,
                           Disc_M=0.26)

# 2012
year <- 2012
df_list[[3]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(c(paste(year, '-09-14'),
                                                          paste(year, '-09-21'))),
                           Date_Closed=lubridate::as_date(c(paste(year, '-09-17'),
                                                            paste(year, '-09-24'))),
                           Size_Limit=NA,
                           Disc_M=0.26)

# 2013
year <- 2013
df_list[[4]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(paste(year, '-08-23')),
                           Date_Closed=lubridate::as_date(paste(year, '-08-26')),
                           Size_Limit=NA,
                           Disc_M=0.26)

# 2014
year <- 2014
df_list[[5]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(c(paste(year, '-07-11'),
                                                          paste(year, '-07-18'),
                                                          paste(year, '-07-25'))),
                           Date_Closed=lubridate::as_date(c(paste(year, '-07-14'),
                                                            paste(year, '-07-21'),
                                                            paste(year, '-07-27'))),
                           Size_Limit=NA,
                           Disc_M=0.26)

# 2015
year <- 2015
df_list[[6]] <- data.frame(Year=year,
                           Date_Open=NA,
                           Date_Closed=lubridate::as_date(paste(year, '-01-01')),
                           Size_Limit=NA,
                           Disc_M=0.26)

# 2016
year <- 2016
df_list[[7]] <- data.frame(Year=year,
                           Date_Open=NA,
                           Date_Closed=lubridate::as_date(paste(year, '-01-01')),
                           Size_Limit=NA,
                           Disc_M=0.26)

# 2017
year <- 2017
df_list[[8]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(c(paste(year, '-11-03'),
                                                          paste(year, '-11-10'),
                                                          paste(year, '-12-08'))),
                           Date_Closed=lubridate::as_date(c(paste(year, '-11-06'),
                                                            paste(year, '-11-13'),
                                                            paste(year, '-12-11'))),
                           Size_Limit=NA,
                           Disc_M=0.25)

# 2018
year <- 2018
df_list[[9]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(c(paste(year, '-08-10'),
                                                          paste(year, '-08-17'))),
                           Date_Closed=lubridate::as_date(c(paste(year, '-08-13'),
                                                            paste(year, '-08-20'))),
                           Size_Limit=NA,
                           Disc_M=0.25)

# 2019
year <- 2019
df_list[[10]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(c(paste(year, '-07-12'),
                                                           paste(year, '-07-19'))),
                            Date_Closed=lubridate::as_date(c(paste(year, '-07-15'),
                                                             paste(year, '-07-21'))),
                            Size_Limit=NA,
                            Disc_M=0.25)

RS_Rec_Season <- do.call('rbind', df_list)


## Calculate relative F by Season  ----

### Commercial Handline ----
RS_logbook_all <- read.csv(file.path(data.dir, 'Coastal Logbooks_RS.csv'))
RS_logbook_SA <- RS_logbook_all %>% filter(Trip.Region == 'SA')

years <- unique(RS_Comm_Season$Year)

rel_F_list <- lapply(years, calc_seasons_RS_Comm, logbook=RS_logbook_SA, season_df=RS_Comm_Season)

RS_Comm_relF <- do.call('rbind', rel_F_list)
RS_Comm_relF$Fleet <- 'Commercial Handline'


### Recreational Headboat ----
RS_logbook_all <- read.csv(file.path(data.dir, 'SE Headboat trip data 12 species.csv'))
RS_logbook_SA <- RS_logbook_all %>% filter(Species.Name == 'RED SNAPPER')

years <- unique(RS_Rec_Season$Year)

rel_F_list <- lapply(years, calc_seasons_RS_HB, logbook=RS_logbook_SA, season_df=RS_Rec_Season)

RS_HB_relF <- do.call('rbind', rel_F_list)
RS_HB_relF$Fleet <- 'Recreational Headboat'


### General Recreational ----

RS_GR_relF <- RS_HB_relF
RS_GR_relF$Fleet <- 'General Recreational'

## Make Dataframe
RS_relF_DF <- bind_rows(RS_Comm_relF, RS_HB_relF, RS_GR_relF)
RS_relF_DF$Fleet <- factor(RS_relF_DF$Fleet, levels=unique(RS_relF_DF$Fleet), ordered = TRUE)

# Make plot
df <- tidyr::pivot_longer(RS_relF_DF, cols=2:3)
df$name <- gsub('\\.', '-', df$name)
df$name <- factor(df$name, ordered = TRUE, levels=c('On-Season', 'Off-Season'))
p <- ggplot(df, aes(x=Year, y=value, color=name)) +
  facet_wrap(~Fleet) +
  geom_line() +
  geom_point(size=2) +
  theme_classic() +
  labs(y='Proportion F', color='') +
  scale_color_manual(values=c('blue', 'orange')) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(breaks= scales::pretty_breaks()) +
  coord_cartesian(clip = 'off')

ggsave('img/OM_Construction/BaseCase/RS_relativeF.png', width=8, height=3)

# Generate OM with Seasonal Fleets ----
RSMOM_season <- RSMOM_combined
RSMOM_season <- AddSeasonal_RS(RSMOM_season, RS_Comm_relF, On.Fleet=1, Off.Fleet=4, Fleet='Commercial Handline')
RSMOM_season <- AddSeasonal_RS(RSMOM_season, RS_HB_relF, On.Fleet=2, Off.Fleet=5, Fleet='Recreational Headboat')
RSMOM_season <- AddSeasonal_RS(RSMOM_season, RS_GR_relF, On.Fleet=3, Off.Fleet=6, Fleet='General Recreational')

saveRDS(RSMOM_season, 'Build_Package/Objects/RS_basecase.mom')

# Simulate Historical Fishery for RS Base Case ----
RS_multiHist <- SimulateMOM(RSMOM_season)
saveRDS(RS_multiHist, 'Build_Package/Objects/RS_basecase.hist')

p <- plot_Catch_Discards(RS_multiHist)
ggsave('img/OM_Construction/BaseCase/RS_histLandings_Discards.png', width=8, height=4)

