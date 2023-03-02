library(tidyverse)
library(dplyr)
library(mgcv)

# Import the historical data
bcparks = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/bcparks.rds")
# Import the model
M = readRDS("~/Desktop/bio/440/BCParks_Attendance/Scripts/models/model-alldata.rda")
# Import the climate projection data
climate = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/climate-projections/monthly-climate-projections.rds")
# Import park coordinates
coords <- read.csv("~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/park_coordinates.csv")
# Import relevant population projection data (LG/HG)
LGrates <- read.csv('~/Desktop/bio/440/BCParks_Attendance/Data/projections/LG-rate-projections.csv')
HGrates <- read.csv('~/Desktop/bio/440/BCParks_Attendance/Data/projections/HG-rate-projections.csv')


# Rename SSP levels for climate projections
climate$scenario <- recode_factor(climate$scenario, 
                             "8GCMs_ensemble_ssp126" = "1-2.6", 
                             "8GCMs_ensemble_ssp245" = "2-4.5",
                             "8GCMs_ensemble_ssp370" = "3-7.0",
                             "8GCMs_ensemble_ssp585" = "5-8.5")
# Rename columns to match model
climate <- climate %>% 
  rename(
    avgtemp = temperature,
    avgprecip = tot_precip,
    ssp = scenario)
# Model requires park column, so annotate coordinates with park names
climate <- merge(x=climate,y=coords, 
                 by=c("latitude","longitude"), all.x=TRUE)

# Predict ATTENDANCE PER 1,000 based on CC projections
climate$predicted_attendance <- predict(M, newdata = climate, type = "response")
# Remove parks for which there was no historical attendance data (therefore no projections)
climate <- na.omit(climate)


# Make datasets for different population growth scenarios: low growth (LG) and high growth (HG)
climateLG <- merge(climate,LGrates,by=c("year"))
climateHG <- merge(climate,HGrates,by=c("year"))

# Reorganize column orders
climateLG <- climateLG %>% relocate(c(park, year, month, region, avgtemp, avgprecip, 
                       predicted_attendance, latitude, longitude, elevation), .after = ssp)
climateHG <- climateHG %>% relocate(c(park, year, month, region, avgtemp, avgprecip, 
                                  predicted_attendance, latitude, longitude, elevation), .after = ssp)
# Make the SSP, year, month columns ascending & alphabetize the park column
climateLG <- climateLG[order(climateLG$ssp, climateLG$park, climateLG$year, climateLG$month),]
climateHG <- climateHG[order(climateHG$ssp, climateHG$park, climateHG$year, climateHG$month),]

# Calculate projected visitor COUNTS for each population growth scenario
climateLG$predicted_visitors <- (climateLG$population/1000)*climateLG$predicted_attendance
climateHG$predicted_visitors <- (climateHG$population/1000)*climateHG$predicted_attendance

# Calculate absolute change in attendance (relative to previous month's observation)
climateLG <- climateLG %>%
  group_by(ssp, park) %>% # don't compare different SSPs or parks
  mutate(diff = predicted_visitors - lag(predicted_visitors))
climateHG <- climateHG %>%
  group_by(ssp, park) %>% # don't compare different SSPs or parks
  mutate(diff = predicted_visitors - lag(predicted_visitors))
# Calculate relative (percent) change in attendance
climateLG$relative_visitors <- climateLG$diff/lag(climateLG$predicted_visitors)
climateHG$relative_visitors <- climateHG$diff/lag(climateHG$predicted_visitors)*100


# Save projections as RDS
saveRDS(climateLG, file = "~/Desktop/bio/440/BCParks_Attendance/Data/projections/LG-attendance-projections.rds")
saveRDS(climateHG, file = "~/Desktop/bio/440/BCParks_Attendance/Data/projections/HG-attendance-projections.rds")

# Clean up environment
rm(LGrates,HGrates,coords,M,bcparks)
