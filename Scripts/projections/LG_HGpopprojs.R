library(lme4)

data <- read.csv("~/Desktop/bio/440/BCParks_Attendance/Data/population/popgrowth.csv")
data$scenario <- as.factor(data$scenario)
data$growth2 <- data$growthrate/100

#Fit the model
FIT <- glmer(growthrate ~ year  + (year|scenario),
             family = Gamma(link="log"),
             data = data)

Logit_Fit <- function(x) {
  B_0 <- 54.64
  B_1 <- -0.02585
  mu = exp(B_0 + B_1*x)
  mu
}

#Make data frame for FA
FArates <- data.frame(year = seq(2020,2100, 1),
                      scenario = "FA")
FArates$growthrate <- predict(FIT, newdata = FArates, type = "response")
FArates$population <- 0
FArates[1,4] = FArates[1,4] + 5319324 #BC's current population

for(i in 2:nrow(FArates)){
  FArates[i,4] <- (FArates[i,3]/1000*FArates[i-1,4])+FArates[i-1,4]
}

#Make data frame for HG
HGrates <- data.frame(year = seq(2020,2100, 1),
                    scenario = "HG")
HGrates$growthrate <- predict(FIT, newdata = HGrates, type = "response")
HGrates$population <- 0
HGrates[1,4] = HGrates[1,4] + 5319324 #BC's current population

for(i in 2:nrow(HGrates)){
  HGrates[i,4] <- (HGrates[i,3]/1000*HGrates[i-1,4])+HGrates[i-1,4]
}

#Make data frame for LG
LGrates <- data.frame(year = seq(2020,2100, 1),
                      scenario = "LG")
LGrates$growthrate <- predict(FIT, newdata = LGrates, type = "response")
LGrates$population <- 0 
LGrates[1,4] = LGrates[1,4] + 5319324 #BC's current population

for(i in 2:nrow(LGrates)){
  LGrates[i,4] <- (LGrates[i,3]/1000*LGrates[i-1,4])+LGrates[i-1,4]
}

#Make data frame for M1
M1rates <- data.frame(year = seq(2020,2100, 1),
                      scenario = "M1")
M1rates$growthrate <- predict(FIT, newdata = M1rates, type = "response")
M1rates$population <- 0 
M1rates[1,4] = M1rates[1,4] + 5319324 #BC's current population

for(i in 2:nrow(M1rates)){
  M1rates[i,4] <- (M1rates[i,3]/1000*M1rates[i-1,4])+M1rates[i-1,4]
}

#Make data frame for M2
M2rates <- data.frame(year = seq(2020,2100, 1),
                      scenario = "M2")
M2rates$growthrate <- predict(FIT, newdata = M2rates, type = "response")
M2rates$population <- 0 
M2rates[1,4] = M2rates[1,4] + 5319324 #BC's current population

for(i in 2:nrow(M2rates)){
  M2rates[i,4] <- (M2rates[i,3]/1000*M2rates[i-1,4])+M2rates[i-1,4]
}

#Make data frame for M3
M3rates <- data.frame(year = seq(2020,2100, 1),
                      scenario = "M3")
M3rates$growthrate <- predict(FIT, newdata = M3rates, type = "response")
M3rates$population <- 0 
M3rates[1,4] = M3rates[1,4] + 5319324 #BC's current population

for(i in 2:nrow(M3rates)){
  M3rates[i,4] <- (M3rates[i,3]/1000*M3rates[i-1,4])+M3rates[i-1,4]
}

#Make data frame for M4
M4rates <- data.frame(year = seq(2020,2100, 1),
                      scenario = "M4")
M4rates$growthrate <- predict(FIT, newdata = M4rates, type = "response")
M4rates$population <- 0 
M4rates[1,4] = M4rates[1,4] + 5319324 #BC's current population

for(i in 2:nrow(M4rates)){
  M4rates[i,4] <- (M4rates[i,3]/1000*M4rates[i-1,4])+M4rates[i-1,4]
}

#Make data frame for M5
M5rates <- data.frame(year = seq(2020,2100, 1),
                      scenario = "M5")
M5rates$growthrate <- predict(FIT, newdata = M5rates, type = "response")
M5rates$population <- 0 
M5rates[1,4] = M5rates[1,4] + 5319324 #BC's current population

for(i in 2:nrow(M5rates)){
  M5rates[i,4] <- (M5rates[i,3]/1000*M5rates[i-1,4])+M5rates[i-1,4]
}

#Make data frame for SA
SArates <- data.frame(year = seq(2020,2100, 1),
                      scenario = "SA")
SArates$growthrate <- predict(FIT, newdata = SArates, type = "response")
SArates$population <- 0 
SArates[1,4] = SArates[1,4] + 5319324 #BC's current population

for(i in 2:nrow(SArates)){
  SArates[i,4] <- (SArates[i,3]/1000*SArates[i-1,4])+SArates[i-1,4]
}

# Save each data frame 
write.csv(FArates, "~/Desktop/bio/440/BCParks_Attendance/Data/population/FA-rate-projections.csv", row.names=FALSE)
write.csv(HGrates, "~/Desktop/bio/440/BCParks_Attendance/Data/population/HG-rate-projections.csv", row.names=FALSE)
write.csv(LGrates, "~/Desktop/bio/440/BCParks_Attendance/Data/population/LG-rate-projections.csv", row.names=FALSE)
write.csv(M1rates, "~/Desktop/bio/440/BCParks_Attendance/Data/population/M1-rate-projections.csv", row.names=FALSE)
write.csv(M2rates, "~/Desktop/bio/440/BCParks_Attendance/Data/population/M2-rate-projections.csv", row.names=FALSE)
write.csv(M3rates, "~/Desktop/bio/440/BCParks_Attendance/Data/population/M3-rate-projections.csv", row.names=FALSE)
write.csv(M4rates, "~/Desktop/bio/440/BCParks_Attendance/Data/population/M4-rate-projections.csv", row.names=FALSE)
write.csv(M5rates, "~/Desktop/bio/440/BCParks_Attendance/Data/population/M5-rate-projections.csv", row.names=FALSE)
write.csv(SArates, "~/Desktop/bio/440/BCParks_Attendance/Data/population/SA-rate-projections.csv", row.names=FALSE)

# Clean up environment
rm(FIT,Logit_Fit,data)
