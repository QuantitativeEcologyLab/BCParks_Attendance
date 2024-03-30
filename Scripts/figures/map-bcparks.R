library('dplyr')        # for data wrangling
library('sf')           # for working with spatial data
library('sp')           # for working with spatial data
library('canadianmaps') # to download a shapefile of BC
library('ggplot2')      # for fancy plots
library('khroma')       # for colour palettes
theme_set(theme_map())


# import a shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

# Import BC Parks data
bcparks = readRDS("Data/Attendance/Park Data/bcparks.rds")
parks <- read.csv('Data/Attendance/Park Data/park_coordinates.csv')

# Remove parks with no data (were not used)
parks <- merge(bcparks, parks, by=c("park", "latitude", "longitude", "region"))
parks <- na.omit(parks)

# Filter data set such that each park only has one observation 
parks <- parks %>% distinct(park, latitude, longitude, .keep_all = TRUE)

#map <-
ggplot() +
  ggtitle("A")+
  geom_sf(data = bc_shp) +
  geom_point(aes(longitude, latitude, col = region), parks, 
             size = 3, shape = 17, alpha = 0.6) +
  guides(col = guide_legend(override.aes = list(alpha=1))) +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +
  theme(legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.box.background = element_rect(color = "black"),
        plot.margin = unit(c(-1,0,-1,0), "cm"),
        plot.title = element_text(vjust = -8.5, hjust = 0.03,
                                  size = 30, family = "sans", face = "bold")) +
  coord_sf() # ensures points don't get jittered around when figure dimensions change



