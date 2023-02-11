library(mgcv)

#Source in BC parks data
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/datatidy/bcparks.R")

#New model with BC parks 2010-2019 day-use data
M_newdata <- gam(attendance ~
                   s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 5) +
                   s(avgtemp) + s(log(avgprecip + 1e-10)) + avgtemp:log(avgprecip + 1e-10) +
                   month:avgtemp + month:log(avgprecip + 1e-10),  #add a tiny number to avgprecip so we don't take log of 0
                 family = Gamma(link = 'log'),
                 data = filter(bcparks, attendancetype == "dayuse"),
                 method = 'REML',
                 knots = list(month = c(0.5, 12.5)))


#Source in Okanagan 2019-2022 data & old model 
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/models/okanagan-model.R")
#Classify region/type of attendance
ok2019_2022$region <- "ok"
ok2019_2022$attendancetype <- "dayuse"
#Add in coordinates for each park 
ok2019_2022 <- ok2019_2022 %>%
  mutate(latitude = case_when(
    park == "Fintry Park" ~ 50.138676,
    park == "Skaha Bluffs Park" ~ 49.433939,
    park == "E.C. Manning Park" ~ 49.119421
  ))
ok2019_2022 <- ok2019_2022 %>%
  mutate(longitude = case_when(
    park == "Fintry Park" ~ -119.501595,
    park == "Skaha Bluffs Park" ~ -119.546110,
    park == "E.C. Manning Park" ~ -120.856781
  ))

#ADD NEW ROWS TO DATASET: OKANAGAN DATA FROM 2019-2022
#Bind the datasets
alldata <- rbind(bcparks,ok2019_2022) 

#Model with all day-use data (recent Okanagan data included)
M_alldata <- gam(attendance ~
                   s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 5) +
                   s(avgtemp) + s(log(avgprecip + 1e-10)) + avgtemp:log(avgprecip + 1e-10) +
                   month:avgtemp + month:log(avgprecip + 1e-10), #add a tiny number to avgprecip so we don't take log of 0
                 family = Gamma(link = 'log'),
                 data = filter(alldata, attendancetype == "dayuse"),
                 method = 'REML',
                 knots = list(month = c(0.5, 12.5)))

