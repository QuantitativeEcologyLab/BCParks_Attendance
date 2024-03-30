library('dplyr')        # for data wrangling
library('canadianmaps') # to download a shapefile of BC
library('elevatr')      # to download digital elevation models
library('purrr')        # for functional programming (map_***(), etc.)
library('sp')           # for spatial data
library('sf')           # for spatial data
library('terra')        # for raster data

#' if necessary, install the `climatenaR` package with
#' `remotes::install_github('burnett-m/climatenaR', build_vignettes = TRUE)`
#' *NOTE:* the `climatenaR` package requires the ClimateNA software.
#'         See https://register.climatena.ca/ to download it

# import a shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

# convert all telemetry datasets to spatial data points ----
locations_df <- read.csv('Data/Attendance/Park Data/park_coordinates.csv')
locations <- SpatialPoints(select(locations_df, longitude, latitude))

ctmm::projection(locations) <- '+proj=longlat'

# check locations
plot(bc_shp)
plot(locations, add = TRUE, col = 'red', pch = 19, cex = 0.5)

# import a Digital Elevation Model (DEM) for the region(s) of interest
#' requires the `progress` package
dem <- get_elev_raster(locations = locations,
                       z = 3,
                       clip = 'bbox',
                       expand = 0.1)

plot(dem)
plot(locations, add = TRUE, col = 'red', pch = 19, cex = 0.5)

# write the csv
# (circumventing climatenaR's functions because we only need 3 locations)
locations_df <- mutate(locations_df,
                       el = terra::extract(dem, locations),
                       ID1 = 1:n(),
                       ID2 = ID1) %>%
  relocate(ID1:ID2)

write.csv(locations_df, file = 'Data/Attendance/parks-dem.csv', row.names = FALSE)

# check the csv
read.csv('Data/Attendance/parks-dem.csv') %>%
  head()
