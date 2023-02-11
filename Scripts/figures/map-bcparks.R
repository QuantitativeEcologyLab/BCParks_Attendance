library('dplyr')        # for data wrangling
library('sf')           # for working with spatial data
library('sp')           # for working with spatial data
# library('terra')        # for raster data (faster than raster)
library('canadianmaps') # to download a shapefile of BC
library('ggplot2')      # for fancy plots
theme_set(theme_map())

#Source in BC Parks data
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/datatidy/bcparks.R")
parks <- read.csv('~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/park_coordinates.csv')

# import a shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

ggplot() +
  geom_sf(data = bc_shp) +
  geom_point(aes(longitude, latitude, col = region), parks, size = 2) +
  labs(x = NULL, y = NULL) +
  scale_color_manual(name="Region", 
                     values=alpha(c('north'="orange",
                              'ok'="forestgreen",
                              'south'="mediumaquamarine",
                              'tc'="gold",
                              'west'="steelblue"), 0.6),
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black"))
