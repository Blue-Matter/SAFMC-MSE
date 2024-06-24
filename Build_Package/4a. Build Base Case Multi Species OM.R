
source('Build_Package/1b. Set OM Parameters.R')

# ----- Build RS OM ----

Fleet_Structure <- data.frame(Name=c('Commercial Handline',
                                     'Recreational Headboat',
                                     'General Recreational'),
                              Landing=c(1,2,3),
                              Discard=c(4,5,6))

saveRDS(Fleet_Structure, 'Build_Package/Objects/Stock_data/RS_Fleet_Structure.rds')

seasonal_F_List <- readRDS('Build_Package/Objects/Stock_data/RS_seasonalF.rds')

RSMOM_season <- structure_OM(rdat=rdat_RedSnapper,
                             stock_name='Red Snapper',
                             nsim=nsim,
                             pyears,
                             CAL_bins,
                             Fleet_Structure,
                             seasonal_F_List= seasonal_F_List)


saveRDS(RSMOM_season, 'Build_Package/Objects/Stock_OMs/RS_basecase.mom')

# ---- Build Gag OM ----

Fleet_Structure <- data.frame(Name=c('Commercial Handline',
                                     'Recreational Headboat',
                                     'General Recreational',
                                     'Commercial Dive'),
                              Landing=c(1,3,4, 2),
                              Discard=c(5,6,7, NA))

saveRDS(Fleet_Structure, 'Build_Package/Objects/Stock_data/GG_Fleet_Structure.rds')

seasonal_F_List <- readRDS('Build_Package/Objects/Stock_data/GG_seasonalF.rds')

GGMOM_season <- structure_OM(rdat=rdat_GagGrouper,
                             stock_name='Gag Grouper',
                             nsim=nsim,
                             pyears,
                             CAL_bins,
                             Fleet_Structure,
                             seasonal_F_List= seasonal_F_List)


GG_Comm_Season <- readRDS('Build_Package/Objects/Stock_data/GG_Comm_Season.rds')
GG_Rec_Season <- readRDS('Build_Package/Objects/Stock_data/GG_Rec_Season.rds')


Comm_SzLim <- data.frame(Year=1992:1998, Size_Limit=inch2mm(20))
RecSzLim <- bind_rows(data.frame(Year=1992:1998, Size_Limit=MSEgraph::inch2mm(20)),
                      data.frame(Year=1999:2009, Size_Limit=MSEgraph::inch2mm(24)))


GGMOM_season <- add_size_limits_GG(GGMOM_season, GG_Comm_Season, GG_Rec_Season,
                               Comm_SzLim, RecSzLim)


saveRDS(GGMOM_season, 'Build_Package/Objects/Stock_OMs/GG_basecase.mom')


# ---- Combine OMs into a Multi-Stock OM ----
RSMOM_season <- readRDS('Build_Package/Objects/Stock_OMs/RS_basecase.mom')
GGMOM_season <- readRDS('Build_Package/Objects/Stock_OMs/GG_basecase.mom')


MOM <- Combine_OMs(MOMlist=list(RSMOM_season, GGMOM_season), Name='Red Snapper & Gag Grouper MOM')


# ---- Define Spatial Structure for Stocks -----

library(MSEtool)
library(ggplot2)
library(dplyr)

assign_region <- function(DF, df, region='name') {
  ch <- chull(df$X, df$Y)
  coords <- df[c(ch, ch[1]), ] %>% select(X, Y)
  coords <- usmap::usmap_transform(coords, c('X', 'Y'),
                            c('x', 'y'))

  tt <- splancs::inpip(DF, coords)
  DF$Region[tt] <- region
  DF
}

calc_ratio <- function(df) {
  vals <- df$Relative_B
  vals/vals[2]
}


areas_df <- readRDS('data_spatial/areas_df.rds')

nage <- MOM@Stocks$`Red Snapper`@maxage+1
nsim <- MOM@nsim
nareas <- nrow(areas_df)
Ages <- 0:(nage-1)

### SCDNR Data - Bubley et al 2023
# Calculate relative biomass in each region
# doesn't cover entire Florida region
data_dir <- 'G:/Shared drives/BM shared/1. Projects/Snapper - Grouper SAFMC/Data/spatial'
RSdata <- readxl::read_excel(file.path(data_dir, 'SCDNR_SpeciesAbundance_Hordyk_02172023.xlsx'), 'SCDNR_RS')
GGdata <- readxl::read_excel(file.path(data_dir, 'SCDNR_SpeciesAbundance_Hordyk_02172023.xlsx'), 'SCDNR_Gag')

# Make data frame
DF <- bind_rows(RSdata, GGdata)%>%
  filter(is.na(TotalWt)==FALSE) %>%
  group_by(Species) %>%
  mutate(Rel_Abundance=TotalWt/SamplingDur) %>%
  usmap::usmap_transform(., c('Longitude', 'Latitude'),
                  c('x', 'y'))

# Assign survey to regions
DF$Region <- ''

Regions <- readRDS('Build_Package/Objects/Regions.rds')

NC_SC <- Regions %>% filter(Region=='North and South Carolina')
GA_CpC <- Regions %>% filter(Region=='Georgia - Cape Canaveral')
CpC_FL <- Regions %>% filter(Region=='Cape Canaveral - Florida')

DF <- assign_region(DF,  NC_SC, 'North and South Carolina')
DF <- assign_region(DF, GA_CpC, 'Georgia - Cape Canaveral')
DF <- assign_region(DF, CpC_FL, 'Cape Canaveral - Florida')
DF <- DF %>% filter(Region!='')
DF$Region <- factor(DF$Region, ordered = TRUE, levels=unique(DF$Region))

DF_abun <- DF %>% group_by(Species) %>%
  mutate(TotalAbun=sum(Rel_Abundance)) %>%
  group_by(Species, Region) %>%
  mutate(Abun=sum(Rel_Abundance)) %>%
  mutate(RelB=Abun/TotalAbun) %>%
  distinct(Species, Region, Relative_B=RelB*100)

DF_abun$Stock <- rep(c('Red Snapper', 'Gag'),each=3)

## Red Snapper Base Case Regional Spatial Structure ----

# SCDNR report (Bubley et al. 2023) used to calculate ratio of biomass in
# Carolinas relative to Georgia - Cape Canaveral (~1/4 amount in Carolinas)


RS_frac_region <- DF_abun %>% filter(Stock=='Red Snapper') %>% calc_ratio(.)

# currently ASSUMING twice as much unfished biomass south of Cape Canaveral
# compared to Cape Canaveral - Georgia border. Need to update when more information is
# available
RS_frac_region <- c(0.25, 1, 2)

frac_region_DF <- data.frame(Region=c("North and South Carolina",
                                      "Georgia - Cape Canaveral",
                                      "Cape Canaveral - Florida"),
                             `Red Snapper`=round(RS_frac_region/sum(RS_frac_region),2))



## Gag Base Case Regional Spatial Structure ----
GG_frac_region <- DF_abun %>% filter(Stock!='Red Snapper') %>% calc_ratio(.)

# ASSUME Florida region unfished biomass is 0.5 Georgia - Cape Canaveral region
GG_frac_region <- c(2.5, 1, 0.5)

GG_frac_region <- round(GG_frac_region/sum(GG_frac_region),2)

frac_region_DF$Gag <- GG_frac_region

frac_region_DF <- frac_region_DF %>% tidyr::pivot_longer(., cols=2:3, names_to ='Stock', values_to='Frac_Area')
frac_region_DF$Stock[frac_region_DF$Stock=='Red.Snapper'] <- 'Red Snapper'
saveRDS(frac_region_DF, 'Build_Package/Objects/BaseCase/Spatial_Regional_Dist.rds')

## Red Snapper Age-Depth Distribution ----

# Mitchell et al (2014) - ages < 3 mostly in shallow waters, no difference in distribution for ages > 3
Age_Frac_NS <- rep(0.5, length(Ages))
Age_Frac_NS[1:4] <- (0.8^(0:3)) - 0.012

df_NS <- data.frame(Age=Ages, Depth='Nearshore', Frac_Depth= Age_Frac_NS)
df_OS <- data.frame(Age=Ages, Depth='Offshore', Frac_Depth= 1-df_NS$Frac_Depth)
df_age_RS <- dplyr::bind_rows(df_NS, df_OS)

df_age_RS$Stock <- 'Red Snapper'

# adjust for density
df_age_RS <- left_join(areas_df, df_age_RS, relationship = "many-to-many")%>%
  group_by(Region, Age) %>%
  mutate(Relative.Size=Relative.Size/sum(Relative.Size),
         Frac_Depth=Frac_Depth*Relative.Size) %>%
  mutate(Frac_Depth=Frac_Depth/sum(Frac_Depth))



saveRDS(df_age_RS, 'Build_Package/Objects/BaseCase/RS_Age_Depth_Dist.rds')

## Gag Age-Depth Distribution ----

# Fraction of unfished age class in near-shore waters
# Carruthers et al 2015 - estimated from GOM
# also Gruss et al
Age_Frac_NS <- rep(0.015, length(Ages))

Age_Frac_NS[1:10] <- 0.65* (0.7^(0:9))

df_NS <- data.frame(Age=Ages, Depth='Nearshore', Frac_Depth= Age_Frac_NS)
df_OS <- data.frame(Age=Ages, Depth='Offshore', Frac_Depth= 1-df_NS$Frac_Depth)
df_age_GG <- dplyr::bind_rows(df_NS, df_OS)
df_age_GG$Stock <- 'Gag'

# adjust for density
df_age_GG <- left_join(areas_df, df_age_GG, relationship = "many-to-many")%>%
  group_by(Region, Age) %>%
  mutate(Relative.Size=Relative.Size/sum(Relative.Size),
         Frac_Depth=Frac_Depth*Relative.Size) %>%
  mutate(Frac_Depth=Frac_Depth/sum(Frac_Depth))

saveRDS(df_age_GG, 'Build_Package/Objects/BaseCase/GG_Age_Depth_Dist.rds')

## Calculate Regional-Depth Unfished Distribution by Age ----
df_age_RS <-  readRDS('Build_Package/Objects/BaseCase/RS_Age_Depth_Dist.rds')
df_age_GG <-  readRDS('Build_Package/Objects/BaseCase/GG_Age_Depth_Dist.rds')
frac_region_DF <- readRDS('Build_Package/Objects/BaseCase/Spatial_Regional_Dist.rds')

df_age_dist <- dplyr::bind_rows(df_age_RS, df_age_GG)
Frac_Age_Region <- calc_region_depth_dist(df_age_dist, frac_region_DF)

saveRDS(Frac_Age_Region, 'Build_Package/Objects/BaseCase/Frac_Age_Region.rds')


p <- ggplot(Frac_Age_Region,
            aes(x=Age, y=Frac_Area, color=Region)) +
  facet_grid(Stock~Region) +
  geom_line() +
  ylim(0,1) +
  theme_bw() +
  labs(y='Proportion') +
  guides(color='none')

p
ggsave('img/Spatial/BaseCase_Area_dist.png', p, width=6, height=4)


p <- ggplot(Frac_Age_Region,
            aes(x=Age, y=Frac_Depth, color=Region)) +
  facet_grid(Stock~Depth) +
  geom_line() +
  ylim(0,1) +
  theme_bw() +
  labs(y='Proportion')

p
ggsave('img/Spatial/BaseCase_NS_OF_dist.png', p, width=6, height=4)


p <- ggplot(Frac_Age_Region,
            aes(x=Age, y=Frac_Depth_Area, color=Region)) +
  facet_grid(Stock~Depth) +
  geom_line() +
  ylim(0,1) +
  theme_bw() +
  labs(y='Proportion')

p

ggsave('img/Spatial/BaseCase_dist.png', p, width=6, height=4)


## - Define Relative Probability of Moving to other Areas ----

# relative probability of moving from row area to column area
frac_other <- matrix(NA, nrow=nareas, ncol=nareas)
frac_other[1,] <- c(NA, 1, 1, 1, 0.01, 0.01)
frac_other[2,] <- c(1, NA, 1, 1, 0.01, 0.01)
frac_other[3,] <- c(1, 1, NA, 1, 1, 1)
frac_other[4,] <- c(1, 1, 1, NA, 1, 1)
frac_other[5,] <- c(0.01, 0.01, 1, 1, NA, 1)
frac_other[6,] <- c(0.01, 0.01, 1, 1, 1, NA)

saveRDS(frac_other, 'Build_Package/Objects/BaseCase/frac_other.rds')


# ---- Update OM with Stock Spatial Structure ----

# assuming 95% prob of staying within an area in a given year for both stocks
# assuming both stocks have same relative probability of moving

MOM <- add_spatial_to_OM(MOM,
                         average_prob=list(0.95,0.95),
                         frac_other=list(frac_other,frac_other),
                         Frac_Age_Region)

# ---- Save MOM as SAMSE Data Object ----
OM_01 <- MOM

usethis::use_data(OM_01, overwrite = TRUE)



