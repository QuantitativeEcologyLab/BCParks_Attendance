library(lme4)
library(ggplot2)
library(dplyr)
library(mgcv)

# Import the data
ok2019_2022 <- read.csv('~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/ok1922NA.csv')

# Clean the data
ok2019_2022 <- na.omit(ok2019_2022) # remove NA values
ok2019_2022$park <- as.factor(ok2019_2022$park) # change class of variable 'park'
names(ok2019_2022)[names(ok2019_2022) == 'visitorcorrected'] <- 'attendance' # rename column 
ok2019_2022$date <- paste(paste(ok2019_2022$year, ok2019_2022$month, sep = "-"), 15, sep = "-") # make a Y-M-D column
ok2019_2022$date <- as.POSIXct(ok2019_2022$date, format = "%Y-%b-%d") # change class of variable 'date'
ok2019_2022$month <- format(ok2019_2022$date, "%m") # make a month column by pulling from date column
ok2019_2022$month <- as.numeric(ok2019_2022$month) # change class of variable 'month'
ok2019_2022 <- ok2019_2022[-which(ok2019_2022$attendance>120),] #remove outlier from dataset

M_olddata <- gam(attendance ~
           s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 11) +
           s(avgtemp) + s(log(avgprecip)) + avgtemp:log(avgprecip) +
           month:avgtemp + month:log(avgprecip),
         family = Gamma(link = 'log'),
         data = ok2019_2022,
         method = 'REML',
         knots = list(month = c(0.5, 12.5)))
