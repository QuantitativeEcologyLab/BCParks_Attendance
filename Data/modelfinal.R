setwd("~/Desktop/bio/440/BCParks_Attendance")
library(lme4)
library(ggplot2)
library(dplyr)
library(mgcv)

ok1922NA <- read.csv(file = 'Data/ok1922NA.csv')
ok2019_2022 <- na.omit(ok1922NA)
rm(ok1922NA)
ok2019_2022$park <- as.factor(ok2019_2022$park)
names(ok2019_2022)[names(ok2019_2022) == 'visitorcorrected'] <- 'attendance'
ok2019_2022$month2 <- factor(ok2019_2022$month,
                             ordered = TRUE,
                             levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
ok2019_2022$date <- paste(paste(ok2019_2022$year, ok2019_2022$month, sep = "-"), 15, sep = "-")
ok2019_2022$date <- as.POSIXct(ok2019_2022$date, format = "%Y-%b-%d")
ok2019_2022$month <- format(ok2019_2022$date, "%m")
ok2019_2022$month <- as.numeric(ok2019_2022$month)

M <- gam(attendance ~
           s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 11) +
           s(avgtemp) + s(log(avgprecip)) + avgtemp:log(avgprecip) +
           month:avgtemp + month:log(avgprecip),
         family = Gamma(link = 'log'),
         data = ok2019_2022[-which(ok2019_2022$attendance>120),],
         method = 'REML',
         knots = list(month = c(0.5, 12.5)))
