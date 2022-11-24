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
ok2019_2022$month <- paste(paste(ok2019_2022$year, ok2019_2022$month, sep = "-"), 15, sep = "-")
ok2019_2022$month <- as.POSIXct(ok2019_2022$month, format = "%Y-%b-%d")

##FINTRY MONTHLY
f_original <- glm(attendance ~ month,
                  family = Gamma(link="log"),
                  data = ok2019_2022[which(ok2019_2022$park == "fintry"),])
AIC(f_original)

f0 <- glm(attendance ~ month + 
            month + avgtemp + avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "fintry"),])
AIC(f0)
#better AIC than original

f1 <- glm(attendance ~ month + 
            month + avgtemp + avgprecip +
            avgtemp*month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "fintry"),])
AIC(f1)
#way better

f2 <- glm(attendance ~ month + 
            avgtemp + avgprecip +
            avgtemp*month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "fintry"),])
AIC(f2)
#adding month doesn't improve AIC, so we can leave it out

f3 <- glm(attendance ~ month + 
            avgtemp +
            avgtemp*month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "fintry"),])
AIC(f3)
#adding avgprecip doesn't improve AIC, so leave it out

f4 <- glm(attendance ~ month + 
            avgtemp*month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "fintry"),])
AIC(f4)
#adding avgtemp doesn't improve AIC, so leave it out

f5 <- glm(attendance ~ month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "fintry"),])
AIC(f5)
#AIC is worse, so include avgtemp*month

f6 <- glm(attendance ~ month +
            avgtemp*month +
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "fintry"),])
AIC(f6)
#AIC is worse than f1! keep avgprecip*month

f7 <- glm(attendance ~ month +
            avgtemp*month +
            avgprecip*month,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "fintry"),])
AIC(f7)
#AIC is better! leave out avgtemp*avgprecip

AIC(f_original, f0, f1, f2, f3, f4, f5, f6, f7)
#f1, f2,  f3, f4 all give the same AIC, so including month, avgtemp, and avgprecip makes no difference
#looks like f7 is best model (temperature-month is only interaction worth keeping in the model)


##SKAHA MONTHLY
s_original <- glm(attendance ~ month,
                  family = Gamma(link="log"),
                  data = ok2019_2022[which(ok2019_2022$park == "skaha"),])
AIC(s_original)

s0 <- glm(attendance ~ month + 
            month + avgtemp + avgprecip +
            avgtemp*month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "skaha"),])
AIC(s0)
#better with these interactions

s1 <- glm(attendance ~ month + 
            avgtemp + avgprecip +
            avgtemp*month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "skaha"),])
AIC(s1)
#adding month doesn't improve AIC, so leave it out

s2 <- glm(attendance ~ month + 
            avgtemp +
            avgtemp*month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "skaha"),])
AIC(s2)
#adding avgprecip doesn't improve AIC, so leave it out

s3 <- glm(attendance ~ month + 
            avgtemp*month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "skaha"),])
AIC(s3)
#adding avgtemp doesn't improve AIC, so leave it out

s4 <- glm(attendance ~ month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "skaha"),])
AIC(s4)
#AIC is better! leave out avgtemp*month

s5 <- glm(attendance ~ month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "skaha"),])
AIC(s5)
#AIC is worse! include avgprecip*month

s6 <- glm(attendance ~ month + 
            avgprecip*month,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "skaha"),])
AIC(s6)
#AIC is worse! include avgtemp*avgprecip

AIC(s_original, s0, s1, s2, s3, s4, s5, s6)
#s0, s1, s2,  s3 all give the same AIC, so including month, avgtemp, and avgprecip makes no difference
#looks like s4 is best (temperature-month interaction not worth keeping in the model)

##MANNING MONTHLY
m_original <- glm(attendance ~ month,
                family = Gamma(link="log"),
                data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m_original)

m0 <- glm(attendance ~ month + 
             month + avgtemp + avgprecip,
           family = Gamma(link="log"),
           data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m0)
#better AIC than m_original

m1 <- glm(attendance ~ month + 
              month + avgtemp + avgprecip +
              avgtemp*month + 
              avgprecip*month + 
              avgtemp*avgprecip,
            family = Gamma(link="log"),
            data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m1)
#AIC much better with these interactions

m2 <- glm(attendance ~ month + 
            avgtemp + avgprecip +
            avgtemp*month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m2)
#adding month doesn't improve AIC, so leave it out

m3 <- glm(attendance ~ month + 
            avgtemp +
            avgtemp*month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m3)
#adding avgprecip doesn't improve AIC, so leave it out

m4 <- glm(attendance ~ month + 
            avgtemp*month + 
            avgprecip*month + 
            avgtemp*avgprecip,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m4)
#adding avgtemp doesn't improve AIC, so leave it out

m5 <- glm(attendance ~ month +
            avgtemp*month + 
            avgprecip*month,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m5)
#AIC is worse! include avgtemp*avgprecip

m6 <- glm(attendance ~ month + 
            avgtemp*avgprecip +
            avgtemp*month,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m6)
#AIC is worse! include avgprecip*month

m7 <- glm(attendance ~ month + 
            avgtemp*avgprecip +
            avgprecip*month,
          family = Gamma(link="log"),
          data = ok2019_2022[which(ok2019_2022$park == "manning"),])
AIC(m7)
#AIC is worse! include avgtemp*month

AIC(m_original, m0, m1, m2, m3, m4, m5, m6, m7)
#m1, m2, m3, m4 all give the same AIC so including month, avgtemp, and avgprecip makes no difference
#looks like m1 is best (all interactions are worth keeping in the model)