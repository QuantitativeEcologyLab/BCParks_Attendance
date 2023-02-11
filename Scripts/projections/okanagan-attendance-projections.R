#Source in the climate projection data
source("~/Desktop/bio/440/BCParks_Attendance/Data/climatedata.R")

#Source in the population projection data for LG/HG
source("~/Desktop/bio/440/BCParks_Attendance/Data/LG_HGpopprojs.R")

#Source in the models and historical data
source("~/Desktop/bio/440/BCParks_Attendance/Data/modelfinal.R")
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

#Project visitor counts for each population growth scenario
for(i in 1:nrow(climateLG)){
  climateLG[i,'predicted_visitors'] <- climateLG[i,12]/1000*climateLG[i,9]
}
for(i in 1:nrow(climateHG)){
  climateHG[i,'predicted_visitors'] <- climateHG[i,12]/1000*climateHG[i,9]
}


