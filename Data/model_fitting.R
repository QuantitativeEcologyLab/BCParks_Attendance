setwd("~/Desktop/bio/440/BCParks_Attendance")
ok1922NA <- read.csv(file = 'Data/ok1922NA.csv')
ok2019_2022 <- na.omit(ok1922NA)
ok2019_2022$park <- as.factor(ok2019_2022$park)
names(ok2019_2022)[names(ok2019_2022) == 'visitorcorrected'] <- 'attendance'

library(lme4)
library(dplyr)
library(mgcv)

ok2019_2022$month2 <- factor(ok2019_2022$month,
                             ordered = TRUE,
                             levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

#Monthly Models
m_original <- glm(attendance ~ month2,
                family = Gamma(link="log"),
                data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m_original)

m0 <- glm(attendance ~ month2 + 
             month2 + avgtemp + avgprecip,
           family = Gamma(link="log"),
           data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m0)
#better AIC than monthfit

m1 <- glm(attendance ~ month2 + 
              month2 + avgtemp + avgprecip +
              avgtemp*month2 + 
              avgprecip*month2 + 
              avgtemp*avgprecip,
            family = Gamma(link="log"),
            data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m1)
#AIC much better with these interactions

m3 <- glm(attendance ~ month2 + 
            avgtemp + avgprecip +
            avgtemp*month2 + 
            avgprecip*month2 + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m3)
#adding month2 doesn't improve AIC, so leave it out

m4 <- glm(attendance ~ month2 + 
            avgtemp +
            avgtemp*month2 + 
            avgprecip*month2 + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m4)
#adding avgprecip doesn't improve AIC, so leave it out

m5 <- glm(attendance ~ month2 + 
            avgtemp*month2 + 
            avgprecip*month2 + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m5)
#adding avgtemp doesn't improve AIC, so leave it out

m6 <- glm(attendance ~ month2 +
            avgtemp*month2 + 
            avgprecip*month2,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m6)
#AIC is worse! include avgtemp*avgprecip

m7 <- glm(attendance ~ month2 + 
            avgtemp*month2 + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m7)
#AIC is worse! include avgprecip*month2

m8 <- glm(attendance ~ month2 + 
            avgprecip*month2 + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m8)
#AIC is worse! include avgtemp*month2
#it looks like all 3 interactions are worth keeping in the model

AIC(monthfit, m0, m1, m2, m3, m4, m5, m6, m7, m8)
#m0, m3, m4, m5 all give the same AIC, so including month2, avgtemp, and avgprecip makes no difference
#looks like m1 is best 

m1 <- glmer(attendance ~ month2 + 
                   month2 + avgtemp + avgprecip +
                   avgtemp*month2 + 
                   avgprecip*month2 + 
                   avgtemp*avgprecip +
                   (month2|park),
                 family = Gamma(link="log"),
                 data = ok2019_2022[which(ok2019_2022$park == "manning"),])
#this is what it should be, but glmer doesn't run with the random effects term (or without it!!)
