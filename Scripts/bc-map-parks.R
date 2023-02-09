library('dplyr')        # for data wrangling
library('sf')           # for working with spatial data
library('sp')           # for working with spatial data
# library('terra')        # for raster data (faster than raster)
library('canadianmaps') # to download a shapefile of BC
library('ggplot2')      # for fancy plots
theme_set(theme_map())

# import a shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

# import park locations
parks <- read.csv('Data/bcparks/park_coordinates.csv')

ggplot() +
  geom_sf(data = bc_shp) +
  geom_point(aes(longitude, latitude), parks, alpha = 0.3) +
  labs(x = NULL, y = NULL)
