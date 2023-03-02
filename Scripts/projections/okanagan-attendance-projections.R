#Source in the climate projection data
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/datatidy/climatedata.R")

#Import the relevant population projection data (LG/HG)
LGrates <- read.csv('~/Desktop/bio/440/BCParks_Attendance/Data/population/LG-rate-projections.csv')
HGrates <- read.csv('~/Desktop/bio/440/BCParks_Attendance/Data/population/HG-rate-projections.csv')

#Source in the models and historical data
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/models/okanagan-model.R")
#Label historical data as having period/ssp "Historical"
ok2019_2022$period <- "Historical"

#Divide avg TOTAL precipitation in a month to get avg DAILY precip over a month
climate$avgprecip <- climate$avgprecip/30

#Predict Attendance per 1,000 based on CC projections
climate$predicted_attendance <- predict(M_olddata, newdata = climate, type = "response")

#Make datasets for different population growth scenarios: low growth (LG) and high growth (HG)
climateLG <- merge(climate,LGrates,by=c("year"))
climateHG <- merge(climate,HGrates,by=c("year"))

#Clean up environment
rm(LGrates,HGrates,ssp1,ssp2,ssp3,ssp5)

#Project visitor COUNTS for each population growth scenario
climateLG[i,'predicted_visitors'] <- 
  climateLG[i,12]/1000*climateLG[i,9] # population/1000*predicted attendance
climateHG[i,'predicted_visitors'] <- 
  climateHG[i,12]/1000*climateHG[i,9] # population/1000*predicted attendance


