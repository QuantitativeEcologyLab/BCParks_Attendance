setwd("~/Desktop/bio/440/BCParks_Attendance/Scripts/datatidy")
library(lme4)
library(ggplot2)
library(dplyr)
library(mgcv)

#Source in BC parks data
source("~/bcparks.R")

#Model with only Okanagan 2019-2022 day-use data
okM <- gam(attendance ~
           s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 11) +
           s(avgtemp) + s(log(avgprecip)) + avgtemp:log(avgprecip) +
           month:avgtemp + month:log(avgprecip),
         family = Gamma(link = 'log'),
         data = ok2019_2022,
         method = 'REML',
         knots = list(month = c(0.5, 12.5)))

#Model with only BC parks 2010-2019 day-use data
okM <- gam(attendance ~
             s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 11) +
             s(avgtemp) + s(log(avgprecip)) + avgtemp:log(avgprecip) +
             month:avgtemp + month:log(avgprecip),
           family = Gamma(link = 'log'),
           data = bcparks,
           method = 'REML',
           knots = list(month = c(0.5, 12.5)))

#Model with all day-use data
okM <- gam(attendance ~
             s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 11) +
             s(avgtemp) + s(log(avgprecip)) + avgtemp:log(avgprecip) +
             month:avgtemp + month:log(avgprecip),
           family = Gamma(link = 'log'),
           data = bcparks,
           method = 'REML',
           knots = list(month = c(0.5, 12.5)))
