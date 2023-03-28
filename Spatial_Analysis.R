library(readxl)
library(dplyr)
library(usmap)
library(ggplot2)
library(rgdal)
library(sp)
library(splancs)

dir <- 'G:/Shared drives/BM shared/1. Projects/Snapper - Grouper SAFMC/Data/spatial'

# 2022-12-05 MSE Technical Team Meeting:
# Decision to use a 6 area explicit model with Carolina, Georgia to Cape Canaveral, S. Cape Canaveral


shp <- readOGR(dsn = file.path(dir, "sa_eez_off_states/SA_EEZ_off_states.shp"), stringsAsFactors = F)

# Make regions

## NC + SC
NC_df <- shp@polygons[[1]]@Polygons[[1]]@coords
NC_df <- data.frame(long=NC_df[,1], lat=NC_df[,2], region='NC')
SC_df <- shp@polygons[[2]]@Polygons[[1]]@coords
SC_df <- data.frame(long=SC_df[,1], lat=SC_df[,2], region='SC')
C_df <- bind_rows(NC_df, SC_df)
## GA to Cape Canaveral
GA_df <- shp@polygons[[3]]@Polygons[[1]]@coords
GA_df <- data.frame(long=GA_df[,1], lat=GA_df[,2], region='GA')

FL_df <- shp@polygons[[4]]@Polygons[[1]]@coords
FL_df <- data.frame(long=FL_df[,1], lat=FL_df[,2], region='FL')

GA_FL_df <- bind_rows(GA_df, FL_df)

GA_CpC_df <- GA_FL_df %>% filter(lat>=28.4740)

SCpC_df <- GA_FL_df %>% filter(lat<28.4740)

# C_df$region <- 'North and South Carolina'
# GA_CpC_df$region <- 'Georgia to Cape Canaveral'
# SCpC_df$region <- 'South of Cape Canaveral'

C_df <- C_df %>% usmap_transform(., c('long', 'lat'),   c('x', 'y'))
GA_CpC_df <- GA_CpC_df %>% usmap_transform(., c('long', 'lat'),   c('x', 'y'))
SCpC_df <- SCpC_df %>% usmap_transform(., c('long', 'lat'),   c('x', 'y'))

cols <- c('#7fc97f', '#beaed4', '#fdc086')
alpha <- 0.1
map <- plot_usmap(include=c('GA', 'FL', 'NC', 'SC'), labels=T) +
  geom_polygon(data = C_df, aes(x = x, y = y, group=region), colour = "black", fill=cols[1], alpha=alpha) +
  geom_polygon(data = GA_CpC_df, aes(x = x, y = y, group=region), colour = "black", fill=cols[2], alpha=alpha) +
  geom_polygon(data = SCpC_df, aes(x = x, y = y, group=region), colour = "black", fill=cols[3], alpha=alpha)

map

# Add species

RSdata <- readxl::read_excel(file.path(dir, 'SCDNR_SpeciesAbundance_Hordyk_02172023.xlsx'), 'SCDNR_RS')

GGdata <- readxl::read_excel(file.path(dir, 'SCDNR_SpeciesAbundance_Hordyk_02172023.xlsx'), 'SCDNR_Gag')

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
  ch <- chull(df$long, df$lat)
  coords <- df[c(ch, ch[1]), ] %>% select(long, lat)
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID = 1)))
  coords <- usmap_transform(coords, c('long', 'lat'),
                            c('x', 'y'))
  tt <- inpip(DF, coords)
  DF$Region[tt] <- region
  DF
}

DF <- assign_region(DF, C_df, 'North and South Carolina')
DF <- assign_region(DF, GA_CpC_df, 'Georgia - Cape Canaveral')
DF <- assign_region(DF, SCpC_df, 'South of Cape Canaveral')
DF <- DF %>% filter(Region!='')
DF$Region <- factor(DF$Region, ordered = TRUE, levels=unique(DF$Region))

DF_abun <- DF %>% group_by(Species) %>%
  mutate(TotalAbun=sum(Rel_Abundance)) %>%
  group_by(Species, Region) %>%
  mutate(Abun=sum(Rel_Abundance)) %>%
  mutate(RelB=Abun/TotalAbun) %>%
  distinct(Species, Region, RelB*100)

DF_abun

map + geom_point(data = DF %>% filter(Species=='Lutjanus campechanus'),
                 aes(x = x, y = y, color=Region), alpha = 0.25) +
  scale_color_manual(values=cols)

map + geom_point(data = DF %>% filter(Species!='Lutjanus campechanus'), aes(x = x, y = y, color=Region), alpha = 0.25)

DF %>% filter(Region=='none')

DF$Region <- 'none'
DF$Region[DF$Latitude>min(C_df$lat)] <- 'One'
DF$Region[DF$Latitude<min(C_df$lat) & DF$Latitude>min(GA_CpC_df$lat)] <- 'Two'
DF$Region[DF$Latitude<min(GA_CpC_df$lat)] <- 'Three'

map + geom_point(data = DF, aes(x = x, y = y, size = Rel_Abundance, color=Region),
                   alpha = 0.25)


map +   geom_point(data = DF %>% filter(Latitude>30.90), aes(x = x, y = y, size = Rel_Abundance, color=Region),
                   alpha = 0.25)

C_df$lat %>% range()

map <- plot_usmap(include=c('GA', 'FL', 'NC', 'SC'), labels=T) +
  geom_polygon(data = C_df, aes(x = x, y = y, group=region), colour = "black", fill=cols[1])


C_df %>% filter(region=='SC') %>% tail()





RSdata %>% names()
eq_transformed <- usmap_transform(RSdata, c('Longitude', 'Latitude'),
                                  c('x', 'y'))


plot_usmap(include=c('GA', 'FL', 'NC', 'SC'), labels=T) +
  geom_point(data = eq_transformed, aes(x = x, y = y, size = TotalWt ),
             color = "red", alpha = 0.25)


require(rgdal)
shp <- readOGR(dsn = file.path(dir, "sa_eez_off_states/SA_EEZ_off_states.shp"), stringsAsFactors = F)

shp@polygons[[1]]

map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)

map + theme_void()

t_data <- usmap_transform(RSdata, c('Longitude', 'Latitude'),
                                  c('x', 'y)
