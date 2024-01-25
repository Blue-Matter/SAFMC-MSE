source('Build_Package/1b. Set OM Parameters.R')

# ----- Define Relative Fishing Mortality for Seasonal Fleet Structure ------

### Commercial ----
## Table 2.7.1 in SEDAR 71 - Seasonal Closures

# Discard mortality: 0.4 for commercial, 0.25 for general rec and headboat

current_yr <- 2019
nyears <- 70

Years <- rev(seq(current_yr, by=-1, length=nyears))
Years <- Years[Years>=1999]

df_list <- list()

# 1999
year <- 1999
df_list[[1]] <- data.frame(Year=year,
                           Date_Open=lubridate::as_date(c(paste(year, '-01-01'),
                                                          paste(year, '-05-01'))),
                           Date_Closed=lubridate::as_date(c(paste(year, '-03-01'),
                                                            paste(year+1, '-01-01'))),
                           Size_Limit=MSEgraph::inch2mm(20),
                           Disc_M=0.4)

# 2000:2009
i <- 1
for (year in 2000:2009) {
  i <- i+1
  df_list[[i]] <- data.frame(Year=year,
                             Date_Open=lubridate::as_date(c(paste(year, '-01-01'),
                                                            paste(year, '-05-01'))),
                             Date_Closed=lubridate::as_date(c(paste(year, '-03-01'),
                                                              paste(year+1, '-01-01'))),
                             Size_Limit=MSEgraph::inch2mm(24),
                             Disc_M=0.4)
}

# 2010
year <- 2010
df_list[[12]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(paste(year, '-05-01')),
                            Date_Closed=lubridate::as_date(paste(year+1, '-1-01')),
                            Size_Limit=MSEgraph::inch2mm(24),
                            Disc_M=0.4)

# 2011
year <- 2011
df_list[[13]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(paste(year, '-05-01')),
                            Date_Closed=lubridate::as_date(paste(year+1, '-1-01')),
                            Size_Limit=MSEgraph::inch2mm(24),
                            Disc_M=0.4)

# 2012
year <- 2012
df_list[[14]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(c(paste(year, '-05-01'),
                                                           paste(year, '-11-13'))),
                            Date_Closed=lubridate::as_date(c(paste(year, '-10-20'),
                                                             paste(year, '-11-21'))),
                            Size_Limit=MSEgraph::inch2mm(24),
                            Disc_M=0.4)

# 2013
year <- 2013
df_list[[15]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(paste(year, '-05-01')),
                            Date_Closed=lubridate::as_date(paste(year, '-11-13')),
                            Size_Limit=MSEgraph::inch2mm(24),
                            Disc_M=0.4)

# 2014
year <- 2014
df_list[[16]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(paste(year, '-05-01')),
                            Date_Closed=lubridate::as_date(paste(year, '-11-21')),
                            Size_Limit=MSEgraph::inch2mm(24),
                            Disc_M=0.4)

# 2015
year <- 2015
df_list[[17]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(paste(year, '-05-01')),
                            Date_Closed=lubridate::as_date(paste(year, '-10-18')),
                            Size_Limit=MSEgraph::inch2mm(24),
                            Disc_M=0.4)

# 2016
year <- 2016
df_list[[18]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(paste(year, '-05-01')),
                            Date_Closed=lubridate::as_date(paste(year+1, '-1-01')),
                            Size_Limit=MSEgraph::inch2mm(24),
                            Disc_M=0.4)

# 2017
year <- 2017
df_list[[19]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(paste(year, '-05-01')),
                            Date_Closed=lubridate::as_date(paste(year+1, '-1-01')),
                            Size_Limit=MSEgraph::inch2mm(24),
                            Disc_M=0.4)

# 2018
year <- 2018
df_list[[20]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(paste(year, '-05-01')),
                            Date_Closed=lubridate::as_date(paste(year+1, '-1-01')),
                            Size_Limit=MSEgraph::inch2mm(24),
                            Disc_M=0.4)

# 2019
year <- 2019
df_list[[21]] <- data.frame(Year=year,
                            Date_Open=lubridate::as_date(paste(year, '-05-01')),
                            Date_Closed=lubridate::as_date(paste(year+1, '-1-01')),
                            Size_Limit=MSEgraph::inch2mm(24),
                            Disc_M=0.4)

GG_Comm_Season <- do.call('rbind', df_list)
saveRDS(GG_Comm_Season, 'Build_Package/Objects/Stock_data/GG_Comm_Season.rds')


### Recreational ----
## Table 2.7.2 in SEDAR 71 - Seasonal Closures

df_list <- list()

years <- 2010:2019

i <- 1
for (year in years) {
  i <- i+1
  df_list[[i]] <- data.frame(Year=year,
                             Date_Open=lubridate::as_date(paste(year, '-05-01')),
                             Date_Closed=lubridate::as_date(paste(year+1, '-01-01')),
                             Size_Limit=MSEgraph::inch2mm(24),
                             Disc_M=0.25)
}


GG_Rec_Season <- do.call('rbind', df_list)
saveRDS(GG_Rec_Season, 'Build_Package/Objects/Stock_data/GG_Rec_Season.rds')


## Calculate relative F by Season  ----

### Commercial Handline ----
GG_logbook_all <- read.csv(file.path(data.dir, 'Coastal Logbooks_GG.csv'))
GG_logbook_SA <- GG_logbook_all %>% filter(Trip.Region == 'SA')

years <- unique(GG_Comm_Season$Year)

rel_F_list <- lapply(years, calc_seasons_RS_Comm, logbook=GG_logbook_SA, season_df=GG_Comm_Season)


GG_Comm_relF <- do.call('rbind', rel_F_list)
GG_Comm_relF$Fleet <- 'Commercial Handline'

### Recreational Headboat ----
GG_logbook_all <- read.csv(file.path(data.dir, 'SE Headboat trip data 12 species.csv'))
GG_logbook_SA <- GG_logbook_all %>% filter(Species.Name == 'GAG')

years <- unique(GG_Rec_Season$Year)

rel_F_list <- lapply(years, calc_seasons_RS_HB, logbook=GG_logbook_SA, season_df=GG_Rec_Season)

GG_HB_relF <- do.call('rbind', rel_F_list)
GG_HB_relF$Fleet <- 'Recreational Headboat'

### General Recreational ----

GG_GR_relF <- GG_HB_relF
GG_GR_relF$Fleet <- 'General Recreational'

# ---- Save Seasonal F ----

seasonal_F_List <- list(GG_Comm_relF=GG_Comm_relF,
                        GG_HB_relF=GG_HB_relF,
                        GG_GR_relF=GG_GR_relF)
saveRDS(seasonal_F_List, 'Build_Package/Objects/Stock_data/GG_seasonalF.rds')

# ---- Make Plot ----

## Make Dataframe
GG_relF_DF <- bind_rows(GG_Comm_relF, GG_HB_relF, GG_GR_relF)
GG_relF_DF$Fleet <- factor(GG_relF_DF$Fleet, levels=unique(GG_relF_DF$Fleet), ordered = TRUE)

# Make plot
df <- tidyr::pivot_longer(GG_relF_DF, cols=2:3)
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

ggsave('img/OM_Construction/BaseCase/GG_relativeF.png', width=8, height=3)

