library('dplyr')        # for data wrangling
library('sf')           # for working with spatial data
library('sp')           # for working with spatial data
library('canadianmaps') # to download a shapefile of BC
library('ggplot2')      # for fancy plots
theme_set(theme_map())

# import a shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

# Import BC Parks data
bcparks = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/bcparks.rds")
parks <- read.csv('~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/park_coordinates.csv')

# Remove parks with no data (were not used)
parks <- merge(bcparks, parks, by=c("park", "latitude", "longitude", "region"))
parks <- na.omit(parks)

# Filter data set such that each park only has one observation 
parks <- parks %>% distinct(park, latitude, longitude, .keep_all = TRUE)


map <- 
ggplot() +
  ggtitle("A")+
  geom_sf(data = bc_shp) +
  geom_point(aes(longitude, latitude, col = region), parks, size = 2) +
  scale_color_manual(name="Region", 
                     values=alpha(c('ok'="#C5ABE7",
                                    'north'="#47007A",
                                    'tc'="#1f78b4",
                                    'west'="#34A02C",
                                    'south'="#85D4FF"), 0.6),
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.box.background = element_rect(color = "black"),
        legend.margin=margin(c(5,8,5,8)),
        plot.title = element_text(hjust = 0.03, size = 30, family = "sans", face = "bold")) +
  coord_sf() # ensures points don't get jittered around when figure dimensions change

# save the figure
ggsave(map,
       width = 8.21, height = 5.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/BCParks-distribution-map.png")
