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


# ---- Generate Map ----
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

map

b_df <- ggplot2::fortify(b) %>%
  sf::st_as_sf(coords=c('x', 'y'), crs = 4326)
plot_ratio <- tmaptools::get_asp_ratio(b_df)

ggsave('img/Spatial/Map.png', map, width=(plot_ratio*5)+3, height=5)


# ---- Calculate size of each region and nearshore/offshore area ----

# Calculate area of each region
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




# --- Calculate Distribution of Species ----

## Red Snapper ----

# info needed:
# proportion unfished in each of the three regions
# use survey to estimate this?
# by age : proportion in offshore waters - see papers
library(MSEtool)
library(SAMSE)

n_age <- SAMSE::MOM_001@Stocks$`Red Snapper`@maxage+1
nsim <- MOM_001@nsim
mov <- array(NA, dim=c(nsim, n_age, 6, 6))

# areas
# 1 - NC on-shore
# 2 - NC off-shore
# 3 - G - on-shore
# 4 - G - off-shore
# 5 - Fl - on-shore
# 6 - Fl - off-shore

# Age 1 - 90% of recruitment on-shore areas
# Equal distribution latitude
frac_onshore_by_age <- 0.9
frac_region <- c(1/3, 1/3, 1/3)

dist <- c(frac_onshore_by_age*frac_region, (1-frac_onshore_by_age)*frac_region)
dist <- dist[c(1,4, 2,5, 3,6)]
prob <- c(0.8, 0.05, 0.8, 0.05, 0.8, 0.05)
probE <- 1

frac_other <- matrix(NA, nrow=6, ncol=6)
# prob of moving from area 1 to areas 2:6
frac_other[1,] <- c(NA, 0.1, 2, 0.1, 1, 0.01)
frac_other[2,] <- c(2, NA, 2, 0.1, 0.01, 0.001)
frac_other[3,] <- c(2, 0.1, NA, 0.1, 2, 0.1)
frac_other[4,] <- c(1, 0.1, 2, NA, 1, 0.1)
frac_other[5,] <- c(0.1, 0.01, 2, 0.01, NA, 0.1)
frac_other[6,] <- c(0.01, 0.01, 1, 0.1, 2, NA)


mov_age <- makemov2(dist, prob=prob, probE=1, frac_other=frac_other, plot=T)

mov_age1 <- replicate(nsim, mov_age)
mov_age2 <- replicate(n_age, mov_age1)
mov <- aperm(mov_age2, c(3,4,1,2))

mov_ageGG <- replicate(nsim, mov_age)
mov_ageGG <- replicate(17, mov_ageGG)
movGG <- aperm(mov_ageGG, c(3,4,1,2))

MOM_001@cpars$`Red Snapper`$`Commercial Handline: On-Season`$mov <- mov
MOM_001@cpars$`Gag Grouper`$`Commercial Handline: On-Season`$mov <- mov

# add area size
hist <- SimulateMOM(MOM_001)

fl <- tempfile()
fl
saveRDS(MOM_001, fl)

t = hist$`Red Snapper`$`Commercial Handline: On-Season`@SampPars$Stock$N[1,1,2,1,]
t
t /sum(t)



# check initdist





