library(tidyverse)
library(dplyr)
library(mgcv)

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
LGattendance <- merge(climate,LGrates,by=c("year"))
HGattendance <- merge(climate,HGrates,by=c("year"))

# Reorganize column orders
LGattendance <- LGattendance %>% relocate(c(park, year, month, region, avgtemp, avgprecip, 
                       predicted_attendance, latitude, longitude, elevation), .after = ssp)
HGattendance <- HGattendance %>% relocate(c(park, year, month, region, avgtemp, avgprecip, 
                                  predicted_attendance, latitude, longitude, elevation), .after = ssp)
# Make the SSP, year, month columns ascending & alphabetize the park column
LGattendance <- LGattendance[order(LGattendance$ssp, LGattendance$park, LGattendance$year, LGattendance$month),]
HGattendance <- HGattendance[order(HGattendance$ssp, HGattendance$park, HGattendance$year, HGattendance$month),]

# Calculate projected visitor COUNTS for each population growth scenario
LGattendance$predicted_visitors <- (LGattendance$predicted_attendance/1000)*LGattendance$population
HGattendance$predicted_visitors <- (HGattendance$predicted_attendance/1000)*HGattendance$population

# Fix formatting for projected visitor counts
LGattendance$predicted_visitors <- as.numeric(LGattendance$predicted_visitors)
HGattendance$predicted_visitors <- as.numeric(HGattendance$predicted_visitors)
 
# Save projections as RDS
saveRDS(LGattendance, file = "~/Desktop/bio/440/BCParks_Attendance/Data/projections/LG-attendance-projections.rds")
saveRDS(HGattendance, file = "~/Desktop/bio/440/BCParks_Attendance/Data/projections/HG-attendance-projections.rds")

# Clean up environment
rm(LGrates,HGrates,coords,M,climate)
