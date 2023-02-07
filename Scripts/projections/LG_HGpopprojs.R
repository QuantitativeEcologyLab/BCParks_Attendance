library(lme4)
setwd("~/Desktop/bio/440/BCParks_Attendance/Data")

data <- read.csv("popgrowth.csv")
data$scenario <- as.factor(data$scenario)
data$growth2 <- data$growthrate/100

#Fit the model
FIT <- glmer(growthrate ~ year  + (year|scenario),
             family = Gamma(link="log"),
             data = data)

summary(FIT)

Logit_Fit <- function(x) {
  B_0 <- 54.64
  B_1 <- -0.02585
  mu = exp(B_0 + B_1*x)
  mu
}

#Make data frame for HG
LGrates <- data.frame(year = seq(2022,2100, 1),
                      scenario = "LG")
LGrates$growthrate <- predict(FIT, newdata = LGrates, type = "response")
LGrates$population <- 0 
LGrates[1,4] = LGrates[1,4] + 5286528 #BC's current population

for(i in 2:nrow(LGrates)){
  LGrates[i,4] <- (LGrates[i,3]/1000*LGrates[i-1,4])+LGrates[i-1,4]
}

#Make data frame for HG
HGrates <- data.frame(year = seq(2022,2100, 1),
                    scenario = "HG")
HGrates$growthrate <- predict(FIT, newdata = HGrates, type = "response")
HGrates$population <- 0
HGrates[1,4] = HGrates[1,4] + 5286528 #BC's current population

for(i in 2:nrow(HGrates)){
  HGrates[i,4] <- (HGrates[i,3]/1000*HGrates[i-1,4])+HGrates[i-1,4]
}


rm(FIT,Logit_Fit,data)
