library(mgcv)
library(tidyverse)

# Import the climate projection data
climate = readRDS("Data/Attendance/Climate/climate-projections/monthly-climate-projections.rds")
# Import park coordinates
park_coordinates <- read.csv("Data/Attendance/Park Data/park_coordinates.csv")
# Import relevant population projection data (LG/HG)
LGrates <- read.csv('Data/Attendance/projections/Population Growth/LG-rate-projections.csv')
HGrates <- read.csv('Data/Attendance/projections/Population Growth/HG-rate-projections.csv')
# Import the attendance projections for relevant pop growth scenarios
LGattendance = readRDS("Data/Attendance/projections/Attendance/LG-attendance-projections.rds")
HGattendance = readRDS("Data/Attendance/projections/Attendance/HG-attendance-projections.rds")
# Import historical attendance
bcparks <- readRDS("Data/Attendance/Park Data/bcparks.rds")

#..................................

# Remove camping data (unused in this study)
bcparks <- bcparks[!bcparks$attendancetype == "camping",] 
# Remove NA values
bcparks <- na.omit(bcparks)
bcparks$region <- as.factor(bcparks$region)
bcparks <- droplevels(bcparks)
#..................................

START <-  Sys.time()
model <- bam(attendance ~
               s(park, bs = 're') +
               s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 5) +         #random intercept & slope effect of park level trend
               s(avgtemp, bs = 'tp', k = 8) +                                   #global effect of temperature, specify k explicitly (even if it is the default)
               s(log(avgprecip + 1e-10), bs = 'tp', k = 6) +                    #global effect of precipitation, add a tiny number to avgprecip so we don't take log of 0
               ti(avgtemp, log(avgprecip + 1e-10), bs = c('tp', 'tp'), k = 5) + #response to snow
               ti(month, avgtemp, k = 5, bs = c('cc', 'tp')) +                  #what is hot in january is cold in july
               ti(month, log(avgprecip + 1e-10), bs = c('cc', 'tp'), k = 5),
             family = Gamma(link = 'log'),
             data = bcparks,
             method = 'fREML',
             discrete = TRUE,
             control = gam.control(nthreads = 10, trace = TRUE),
             knots = list(month = c(0.5, 12.5)))
saveRDS(model, file = "Scripts/Attendance/models/attendance_model.RDS")
END <-  Sys.time()
TIME_DURATION <- END - START