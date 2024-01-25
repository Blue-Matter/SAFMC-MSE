
source('Build_Package/1b. Set OM Parameters.R')

# ----- Define Relative Fishing Mortality for Seasonal Fleet Structure ------

### Commercial ----
## Table 2.2.1 in SEDAR 73 - Seasonal Closures
# Table 6 in SEDAR 73 - Discard mortality

current_yr <- 2019
nyears <- 70

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


### ---- General Recreational ----
# assuming same seasonal pattern as Recreational Headboat
RS_GR_relF <- RS_HB_relF
RS_GR_relF$Fleet <- 'General Recreational'


# ---- Save Seasonal F ----

seasonal_F_List <- list(RS_Comm_relF=RS_Comm_relF,
                        RS_HB_relF=RS_HB_relF,
                        RS_GR_relF=RS_GR_relF)
saveRDS(seasonal_F_List, 'Build_Package/Objects/Stock_data/RS_seasonalF.rds')

# ---- Make Plot ----
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

