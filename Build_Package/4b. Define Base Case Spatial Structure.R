# Calculates the relative size of each area in the fishery

# 2022-12-05 MSE Technical Team Meeting:
# Decision to use a 6 area explicit model with Carolina, Georgia to Cape Canaveral, S. Cape Canaveral
# and nearshore/offshore defined at 30 m (100 ft) contour

library(sf)
library(usmap)
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(marmap)
library(digitize)
library(tmaptools)



# Part 1 -- Define Spatial Areas ----

## ---- Generate Map ----
shp <- sf::st_read(dsn = file.path("data_spatial/SA_EEZ_off_states.shp"), stringsAsFactors = F)

get_coords <- function(shp, areas, region='region') {
  tt <- shp[shp$AreaName%in% areas,]
  e <- sf::st_union(tt)
  df <- sf::st_coordinates(e)
  df <- data.frame(df[,1:2])
  df$Region <- region
  df
}


# North and South Carolina
NC_SC <- get_coords(shp, areas=c('Off NC', 'Off SC'), region='North and South Carolina')

# Georgia - Cape Canaveral
GA_FL <- get_coords(shp, c('Off GA', 'Off FL'), 'Georgia - Cape Canaveral')
GA_CpC <- GA_FL %>% dplyr::filter(Y>=28.470)

# Cape Canaveral to Florida
CpC_FL <- GA_FL %>% dplyr::filter(Y<28.470)
CpC_FL$Region <- 'Cape Canaveral - Florida'

DF <- dplyr::bind_rows(NC_SC, GA_CpC, CpC_FL)
DF$Region <- factor(DF$Region, levels=unique(DF$Region), ordered = TRUE)

# bathymetery
long_range <- c(-84, -71.37)
lat_range <- range(DF$Y)

b <- marmap::getNOAA.bathy(lon1 = long_range[1], lon2 = long_range[2],
                           lat1 = lat_range[1], lat2 = lat_range[2],
                           resolution = 1)

cols <- c('#7fc97f', '#beaed4', '#fdc086')
alpha <- 0.5

map <- ggplot()+
  geom_raster(data=b, aes(x=x, y=y,fill=z)) +
  coord_sf() +
  marmap::scale_fill_etopo() +
  guides(fill='none') +
  ggnewscale::new_scale_fill() +
  geom_polygon(data = DF, aes(x = X, y = Y, group=Region, fill=Region), alpha=alpha) +
  geom_contour(data=b, aes(x=x, y=y, z=z), breaks=c(-30), colour="black",linewidth=0.2) +
  scale_fill_manual(values=cols) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude') +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))


numbers <- data.frame(Number=1:6,
                      x=c(-79.5, -77.5, -81, -77.5, -82, -80),
                      y=c(33, 33, 30, 30, 25, 25))

map <- map + geom_text(data=numbers, aes(x=x, y=y, label=Number), size=10)

b_df <- ggplot2::fortify(b) %>%
  sf::st_as_sf(coords=c('x', 'y'), crs = 4326)
plot_ratio <- tmaptools::get_asp_ratio(b_df)

ggsave('img/Spatial/Map.png', map, width=(plot_ratio*5)+3, height=5)


## ---- Calculate size of each region and nearshore/offshore area ----

Region_Polys <- DF %>%
  sf::st_as_sf(., coords = c("X", "Y"),  crs = 4326) %>%
  group_by(Region) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

Region_Areas <- Region_Polys %>%
  sf::st_area()
names(Region_Areas) <- unique(DF$Region)

# Create polygon of 30 m contour
b_dataframe <- ggplot2::fortify(b) %>%
  dplyr::filter(z< 0) # drop land

# filter out Carribean
carib1 <- b_dataframe %>% filter(x> -79.6 & y<28)
carib2 <- b_dataframe %>% filter(x> -81 & y<24.5)

b_dataframe <- b_dataframe %>%
  dplyr::setdiff(., carib1) %>%
  dplyr::setdiff(., carib2)

p <- ggplot(b_dataframe, aes(x=x, y=y, z=z)) +
  geom_contour(breaks=c(0, -30))
pg <- ggplot2::ggplot_build(p)
df <- pg$data[[1]] %>% filter(level==-30)
tab <- df$piece %>% table()

pieces <- as.numeric(names(sort(tab,decreasing=TRUE))[1:2])

depth_30<- df %>% dplyr::filter(piece %in%pieces) %>%
  dplyr::select(x,y,order)

p <- ggplot(depth_30, aes(x=x,y=y)) +
  geom_point() +
  scale_x_continuous(expand=c(0,0), limits=c(-85, -72)) +
  scale_y_continuous(expand=c(0,0), limits=c(24, 37)) +
  theme_bw()
p

ggsave('img/Spatial/30m_depth.png', p)

# digitize the 30 m contour and import as dataframe
# -- don't re-run unless neccesary --
# tt <- digitize::digitize(image_filename='img/Spatial/30m_depth.png',
#                          x1=-85, x2=-72,
#                          y1=24, y2=37)
# saveRDS(tt, 'data_spatial/30m_digitize.rda')

tt <- readRDS('data_spatial/30m_digitize.rda')

poly_30 <- tt %>%
  sf::st_as_sf(., coords = c("x", "y"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

plot(poly_30)

nearshore_areas <- sf::st_intersection(Region_Polys, poly_30) %>%
  sf::st_area()


# Calculate Relative Size of each Area
nearshore <- data.frame(Region=unique(DF$Region),
                        Depth='Nearshore',
                        `Relative Size`=as.numeric(nearshore_areas/sum(Region_Areas)))

offshore <- data.frame(Region=unique(DF$Region),
                       Depth='Offshore',
                       `Relative Size`=as.numeric((Region_Areas-nearshore_areas)/sum(Region_Areas)))

areas_df <- dplyr::bind_rows(nearshore, offshore) %>%
  dplyr::arrange(Region, Depth) %>%
  dplyr::mutate(Area=1:6) %>%
  dplyr::relocate(Area)

saveRDS(areas_df, 'data_spatial/areas_df.rds')

# Part 2 -- Define Base Case Spatial Distribution ----

library(MSEtool)
library(ggplot2)
library(dplyr)

MOM <- get(data("OM_01"))
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
  usmap_transform(., c('Longitude', 'Latitude'),
                  c('x', 'y'))

# Assign survey to regions
DF$Region <- ''

assign_region <- function(DF, df, region='name') {
  ch <- chull(df$X, df$Y)
  coords <- df[c(ch, ch[1]), ] %>% select(X, Y)
  coords <- usmap_transform(coords, c('X', 'Y'),
                            c('x', 'y'))

  tt <- splancs::inpip(DF, coords)
  DF$Region[tt] <- region
  DF
}

DF <- assign_region(DF, NC_SC, 'North and South Carolina')
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


## Assumed Fraction of Unfished Stock in Each Region ----

# SCDNR report (Bubley et al. 2023) used to calculate ratio of biomass in
# Carolinas relative to Georgia - Cape Canaveral (~1/4 amount in Carolinas)

calc_ratio <- function(df) {
  vals <- df$Relative_B
  vals/vals[2]
}

RS_frac_region <- DF_abun %>% filter(Stock=='Red Snapper') %>% calc_ratio(.)

# probably under-estimates abundance south of Cp Canaveral - Florida.

# currently ASSUMING twice as much unfished biomass south of Cape Canaveral
# compared to Cape Canaveral - Georgia border. Need to update when more information is
# available
RS_frac_region <- c(0.25, 1, 2)

frac_region_DF <- data.frame(Region=c("North and South Carolina",
                                      "Georgia - Cape Canaveral",
                                      "Cape Canaveral - Florida"),
                             `Red Snapper`=round(RS_frac_region/sum(RS_frac_region),2))


frac_region_DF

# Gag
GG_frac_region <- DF_abun %>% filter(Stock!='Red Snapper') %>% calc_ratio(.)

GG_frac_region <- c(2.5, 1, 0.5)

GG_frac_region <- round(GG_frac_region/sum(GG_frac_region),2)

frac_region_DF$Gag <- GG_frac_region

saveRDS(frac_region_DF, 'data_spatial/Frac_region_BC.rds')



# Fraction of unfished age class in near-shore waters ----

# Red Snapper

# Mitchell et al (2014) - ages < 3 mostly in shallow waters, no difference in distribution for ages > 3
Age_Frac_NS <- rep(0.5, length(Ages))
Age_Frac_NS[1:4] <- (0.8^(0:3)) - 0.012
Age_Frac_NS_DF <- data.frame(Age=Ages, Frac=Age_Frac_NS)

df_NS <- data.frame(Age=Ages, Depth='Nearshore', Fraction= Age_Frac_NS)
df_OS <- data.frame(Age=Ages, Depth='Offshore', Fraction= 1-df_NS$Fraction)
df_age_RS <- dplyr::bind_rows(df_NS, df_OS)


Age_Region <- dplyr::left_join(areas_df, df_age_RS, relationship = "many-to-many")
Frac_Age_Region_RS <- dplyr::left_join(Age_Region, frac_region_DF) %>%
  group_by(Age) %>%
  mutate(Red.Snapper=Fraction*Red.Snapper) %>%
  select(-Gag, -Relative.Size, -Fraction)


# Gag

# Fraction of unfished age class in near-shore waters
# Carruthers et al 2015 - estimated from GOM
# also Gruss et al
Age_Frac_NS <- rep(0.015, length(Ages))

Age_Frac_NS[1:10] <- 0.65* (0.7^(0:9))
Age_Frac_NS_DF <- data.frame(Age=Ages, Frac=Age_Frac_NS)

df_NS <- data.frame(Age=Ages, Depth='Nearshore', Fraction= Age_Frac_NS)
df_OS <- data.frame(Age=Ages, Depth='Offshore', Fraction= 1-df_NS$Fraction)
df_age_GG <- dplyr::bind_rows(df_NS, df_OS)

Age_Region <- dplyr::left_join(areas_df, df_age_GG, relationship = "many-to-many")
Frac_Age_Region_GG <- dplyr::left_join(Age_Region, frac_region_DF %>% select(-Red.Snapper)) %>%
  group_by(Age) %>%
  mutate(Gag=Fraction*Gag)  %>%
  select(-Relative.Size, -Fraction)


Frac_Age_Region <- left_join(Frac_Age_Region_RS, Frac_Age_Region_GG,
                             by=c('Area', 'Region', 'Depth', 'Age')) %>%
  tidyr::pivot_longer(., cols=c('Red.Snapper', 'Gag'))

Frac_Age_Region$name[Frac_Age_Region$name=='Red.Snapper'] <- 'Red Snapper'

Frac_Age_Region$Region <- factor(Frac_Age_Region$Region, levels( areas_df$Region))

Frac_Age_Region$name <- factor(Frac_Age_Region$name,unique(Frac_Age_Region$name), ordered = TRUE)

p <- ggplot(Frac_Age_Region,
       aes(x=Age, y=value, color=Region)) +
  facet_grid(name~Depth) +
  geom_line() +
  ylim(0,1) +
  theme_bw() +
  labs(y='Proportion')

p

ggsave('img/Spatial/BaseCase_dist.png', p, width=6, height=4)


# Probability of Movement ----

# relative probability of moving from row area to column area
frac_other <- matrix(NA, nrow=nareas, ncol=nareas)
frac_other[1,] <- c(NA, 1, 1, 1, 0.01, 0.01)
frac_other[2,] <- c(1, NA, 1, 1, 0.01, 0.01)
frac_other[3,] <- c(1, 1, NA, 1, 1, 1)
frac_other[4,] <- c(1, 1, 1, NA, 1, 1)
frac_other[5,] <- c(0.01, 0.01, 1, 1, NA, 1)
frac_other[6,] <- c(0.01, 0.01, 1, 1, 1, NA)

## Red Snapper
average_prob <- 0.95

movRS <- array(NA, dim=c(nage, nareas, nareas))
for (a in seq_along(Ages)) {
  temp <- Frac_Age_Region %>% filter(Age==Ages[a], name=='Red Snapper')
  mov_age <- makemov2(temp$value, prob=average_prob, probE=1, frac_other=frac_other, plot=F)
  movRS[a,,] <- mov_age
}
movRS <- replicate(nsim, movRS) %>% aperm(., c(4,1,2,3))

OM_01@cpars$`Red Snapper`$`Commercial Handline: On-Season`$mov <- movRS
OM_01@cpars$`Red Snapper`$Asize <- matrix(areas_df$Relative.Size, nrow=nsim, ncol=nareas, byrow=T)


## Gag Grouper

average_prob <- 0.95

movGG <- array(NA, dim=c(nage, nareas, nareas))
for (a in seq_along(Ages)) {
  temp <- Frac_Age_Region %>% filter(Age==Ages[a], name=='Gag')
  mov_age <- makemov2(temp$value, prob=average_prob, probE=1, frac_other=frac_other, plot=F)
  movGG[a,,] <- mov_age
}
movGG <- replicate(nsim, movGG) %>% aperm(., c(4,1,2,3))

OM_01@cpars$`Gag Grouper`$`Commercial Handline: On-Season`$mov <- movGG
OM_01@cpars$`Gag Grouper`$Asize <- matrix(areas_df$Relative.Size, nrow=nsim, ncol=nareas, byrow=T)


# Part 3 -- Update Base Case Operating Model with Spatial Structure ----

usethis::use_data(OM_01, overwrite = TRUE)




