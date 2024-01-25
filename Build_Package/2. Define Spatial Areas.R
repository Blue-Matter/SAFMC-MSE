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
saveRDS(DF, 'Build_Package/Objects/Regions.rds')
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
