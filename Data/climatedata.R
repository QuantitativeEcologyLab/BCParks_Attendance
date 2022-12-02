library(dplyr)
library(tidyverse)

fintry <- rep("fintry",times=12)
skaha <- rep("skaha",times=12)
manning <- rep("manning",times=12)

##SSP 1-2.6 
#2021-2040 
ssp126_2021_2040 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp126_2021-2040.csv") #import data
ssp126_2021_2040 <- ssp126_2021_2040 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg1_2140 <- ssp126_2021_2040[,30:41] #columns of avg temp for each month
tavg1_2140 <- as.data.frame(t(tavg1_2140)) 

ppt1_2140 <- ssp126_2021_2040[,42:53] #columns of avg precip for each month
ppt1_2140 <- as.data.frame(t(ppt1_2140))

f1_21_40 <- data.frame(fintry,tavg1_2140$V1,ppt1_2140$V1, 1:12, rep("2021-2040",times=12)) #dataframe for fintry
colnames(f1_21_40) <- c("park","temp", "ppt", "month", "period")
s1_21_40 <- data.frame(skaha,tavg1_2140$V2,ppt1_2140$V2, 1:12, rep("2021-2040",times=12)) #dataframe for skaha
colnames(s1_21_40) <- c("park","temp", "ppt", "month", "period")
m1_21_40 <- data.frame(manning,tavg1_2140$V3,ppt1_2140$V3, 1:12, rep("2021-2040",times=12)) #dataframe for manning
colnames(m1_21_40) <- c("park","temp", "ppt", "month", "period")

ssp1_21_40 <- rbind(f1_21_40, s1_21_40, m1_21_40) #combine 3 park dataframes into one for this scenario & period



#2041-2060 
ssp126_2041_2060 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp126_2041-2060.csv") 
ssp126_2041_2060 <- ssp126_2041_2060 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg1_4160 <- ssp126_2041_2060[,30:41] #columns of avg temp for each month
tavg1_4160 <- as.data.frame(t(tavg1_4160)) 

ppt1_4160 <- ssp126_2041_2060[,42:53] #columns of avg precip for each month
ppt1_4160 <- as.data.frame(t(ppt1_4160))

f1_41_60 <- data.frame(fintry,tavg1_4160$V1,ppt1_4160$V1, 1:12, rep("2041-2060",times=12)) #dataframe for fintry
colnames(f1_41_60) <- c("park","temp", "ppt", "month", "period")
s1_41_60 <- data.frame(skaha,tavg1_4160$V2,ppt1_4160$V2, 1:12, rep("2041-2060",times=12)) #dataframe for skaha
colnames(s1_41_60) <- c("park","temp", "ppt", "month", "period")
m1_41_60 <- data.frame(manning,tavg1_4160$V3,ppt1_4160$V3, 1:12, rep("2041-2060",times=12)) #dataframe for manning
colnames(m1_41_60) <- c("park","temp", "ppt", "month", "period")

ssp1_41_60 <- rbind(f1_41_60, s1_41_60, m1_41_60) #combine 3 park dataframes into one for this scenario & period



#2061-2080 
ssp126_2061_2080 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp126_2061-2080.csv") 
ssp126_2061_2080 <- ssp126_2061_2080 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg1_6180 <- ssp126_2061_2080[,30:41] #columns of avg temp for each month
tavg1_6180 <- as.data.frame(t(tavg1_6180)) 

ppt1_6180 <- ssp126_2061_2080[,42:53] #columns of avg precip for each month
ppt1_6180 <- as.data.frame(t(ppt1_6180))

f1_61_80 <- data.frame(fintry,tavg1_6180$V1,ppt1_6180$V1, 1:12, rep("2061-2080",times=12)) #dataframe for fintry
colnames(f1_61_80) <- c("park","temp", "ppt", "month", "period")
s1_61_80 <- data.frame(skaha,tavg1_6180$V2,ppt1_6180$V2, 1:12, rep("2061-2080",times=12)) #dataframe for skaha
colnames(s1_61_80) <- c("park","temp", "ppt", "month", "period")
m1_61_80 <- data.frame(manning,tavg1_6180$V3,ppt1_6180$V3, 1:12, rep("2061-2080",times=12)) #dataframe for manning
colnames(m1_61_80) <- c("park","temp", "ppt", "month", "period")

ssp1_61_80 <- rbind(f1_61_80, s1_61_80, m1_61_80) #combine 3 park dataframes into one for this scenario & period



#2081-2100 
ssp126_2081_2100 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp126_2081-2100.csv") 
ssp126_2081_2100 <- ssp126_2081_2100 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg1_81100 <- ssp126_2081_2100[,30:41] #columns of avg temp for each month
tavg1_81100 <- as.data.frame(t(tavg1_81100)) 

ppt1_81100 <- ssp126_2081_2100[,42:53] #columns of avg precip for each month
ppt1_81100 <- as.data.frame(t(ppt1_81100))

f1_81_100 <- data.frame(fintry,tavg1_81100$V1,ppt1_81100$V1, 1:12, rep("2081-2100",times=12)) #dataframe for fintry
colnames(f1_81_100) <- c("park","temp", "ppt", "month", "period")
s1_81_100 <- data.frame(skaha,tavg1_81100$V2,ppt1_81100$V2, 1:12, rep("2081-2100",times=12)) #dataframe for skaha
colnames(s1_81_100) <- c("park","temp", "ppt", "month", "period")
m1_81_100 <- data.frame(manning,tavg1_81100$V3,ppt1_81100$V3, 1:12, rep("2081-2100",times=12)) #dataframe for manning
colnames(m1_81_100) <- c("park","temp", "ppt", "month", "period")

ssp1_81_100 <- rbind(f1_81_100, s1_81_100, m1_81_100) #combine 3 park dataframes into one for this scenario & period

ssp1 <- rbind(ssp1_21_40, ssp1_41_60, ssp1_61_80, ssp1_81_100) #combine all periods for this scenario
ssp1$ssp <- rep("1-2.6",times=144)



##SSP 2-4.5
#2021-2040 
ssp245_2021_2040 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp245_2021-2040.csv") #import data
ssp245_2021_2040 <- ssp245_2021_2040 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg2_2140 <- ssp245_2021_2040[,30:41] #columns of avg temp for each month
tavg2_2140 <- as.data.frame(t(tavg2_2140)) 

ppt2_2140 <- ssp245_2021_2040[,42:53] #columns of avg precip for each month
ppt2_2140 <- as.data.frame(t(ppt2_2140))

f2_21_40 <- data.frame(fintry,tavg2_2140$V1,ppt2_2140$V1, 1:12, rep("2021-2040",times=12)) #dataframe for fintry
colnames(f2_21_40) <- c("park","temp", "ppt", "month", "period")
s2_21_40 <- data.frame(skaha,tavg2_2140$V2,ppt2_2140$V2, 1:12, rep("2021-2040",times=12)) #dataframe for skaha
colnames(s2_21_40) <- c("park","temp", "ppt", "month", "period")
m2_21_40 <- data.frame(manning,tavg2_2140$V3,ppt2_2140$V3, 1:12, rep("2021-2040",times=12)) #dataframe for manning
colnames(m2_21_40) <- c("park","temp", "ppt", "month", "period")

ssp2_21_40 <- rbind(f2_21_40, s2_21_40, m2_21_40) #combine 3 park dataframes into one for this scenario & period

#2041-2060 
ssp245_2041_2060 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp245_2041-2060.csv") 
ssp245_2041_2060 <- ssp245_2041_2060 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg2_4160 <- ssp245_2041_2060[,30:41] #columns of avg temp for each month
tavg2_4160 <- as.data.frame(t(tavg2_4160)) 

ppt2_4160 <- ssp245_2041_2060[,42:53] #columns of avg precip for each month
ppt2_4160 <- as.data.frame(t(ppt2_4160))

f2_41_60 <- data.frame(fintry,tavg2_4160$V1,ppt2_4160$V1, 1:12, rep("2041-2060",times=12)) #dataframe for fintry
colnames(f2_41_60) <- c("park","temp", "ppt", "month", "period")
s2_41_60 <- data.frame(skaha,tavg2_4160$V2,ppt2_4160$V2, 1:12, rep("2041-2060",times=12)) #dataframe for skaha
colnames(s2_41_60) <- c("park","temp", "ppt", "month", "period")
m2_41_60 <- data.frame(manning,tavg2_4160$V3,ppt2_4160$V3, 1:12, rep("2041-2060",times=12)) #dataframe for manning
colnames(m2_41_60) <- c("park","temp", "ppt", "month", "period")

ssp2_41_60 <- rbind(f2_41_60, s2_41_60, m2_41_60) #combine 3 park dataframes into one for this scenario & period



#2061-2080 
ssp245_2061_2080 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp245_2061-2080.csv") 
ssp245_2061_2080 <- ssp245_2061_2080 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg2_6180 <- ssp245_2061_2080[,30:41] #columns of avg temp for each month
tavg2_6180 <- as.data.frame(t(tavg2_6180)) 

ppt2_6180 <- ssp245_2061_2080[,42:53] #columns of avg precip for each month
ppt2_6180 <- as.data.frame(t(ppt2_6180))

f2_61_80 <- data.frame(fintry,tavg2_6180$V1,ppt2_6180$V1, 1:12, rep("2061-2080",times=12)) #dataframe for fintry
colnames(f2_61_80) <- c("park","temp", "ppt", "month", "period")
s2_61_80 <- data.frame(skaha,tavg2_6180$V2,ppt2_6180$V2, 1:12, rep("2061-2080",times=12)) #dataframe for skaha
colnames(s2_61_80) <- c("park","temp", "ppt", "month", "period")
m2_61_80 <- data.frame(manning,tavg2_6180$V3,ppt2_6180$V3, 1:12, rep("2061-2080",times=12)) #dataframe for manning
colnames(m2_61_80) <- c("park","temp", "ppt", "month", "period")

ssp2_61_80 <- rbind(f2_61_80, s2_61_80, m2_61_80) #combine 3 park dataframes into one for this scenario & period


#2081-2100 
ssp245_2081_2100 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp245_2081-2100.csv") 
ssp245_2081_2100 <- ssp245_2081_2100 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg2_81100 <- ssp245_2081_2100[,30:41] #columns of avg temp for each month
tavg2_81100 <- as.data.frame(t(tavg2_81100)) 

ppt2_81100 <- ssp245_2081_2100[,42:53] #columns of avg precip for each month
ppt2_81100 <- as.data.frame(t(ppt2_81100))

f2_81_100 <- data.frame(fintry,tavg2_81100$V1,ppt2_81100$V1, 1:12, rep("2081-2100",times=12)) #dataframe for fintry
colnames(f2_81_100) <- c("park","temp", "ppt", "month", "period")
s2_81_100 <- data.frame(skaha,tavg2_81100$V2,ppt2_81100$V2, 1:12, rep("2081-2100",times=12)) #dataframe for skaha
colnames(s2_81_100) <- c("park","temp", "ppt", "month", "period")
m2_81_100 <- data.frame(manning,tavg2_81100$V3,ppt2_81100$V3, 1:12, rep("2081-2100",times=12)) #dataframe for manning
colnames(m2_81_100) <- c("park","temp", "ppt", "month", "period")

ssp2_81_100 <- rbind(f2_81_100, s2_81_100, m2_81_100) #combine 3 park dataframes into one for this scenario & period

ssp2 <- rbind(ssp2_21_40, ssp2_41_60, ssp2_61_80, ssp2_81_100) #combine all periods for this scenario
ssp2$ssp <- rep("2-4.5",times=144)



##SSP 3-7.0
#2021-2040 
ssp370_2021_2040 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp370_2021-2040.csv") #import data
ssp370_2021_2040 <- ssp370_2021_2040 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg3_2140 <- ssp370_2021_2040[,30:41] #columns of avg temp for each month
tavg3_2140 <- as.data.frame(t(tavg3_2140)) 

ppt3_2140 <- ssp370_2021_2040[,42:53] #columns of avg precip for each month
ppt3_2140 <- as.data.frame(t(ppt3_2140))

f3_21_40 <- data.frame(fintry,tavg3_2140$V1,ppt3_2140$V1, 1:12, rep("2021-2040",times=12)) #dataframe for fintry
colnames(f3_21_40) <- c("park","temp", "ppt", "month", "period")
s3_21_40 <- data.frame(skaha,tavg3_2140$V2,ppt3_2140$V2, 1:12, rep("2021-2040",times=12)) #dataframe for skaha
colnames(s3_21_40) <- c("park","temp", "ppt", "month", "period")
m3_21_40 <- data.frame(manning,tavg3_2140$V3,ppt3_2140$V3, 1:12, rep("2021-2040",times=12)) #dataframe for manning
colnames(m3_21_40) <- c("park","temp", "ppt", "month", "period")

ssp3_21_40 <- rbind(f3_21_40, s3_21_40, m3_21_40) #combine 3 park dataframes into one for this scenario & period

#2041-2060 
ssp370_2041_2060 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp370_2041-2060.csv") 
ssp370_2041_2060 <- ssp370_2041_2060 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg3_4160 <- ssp370_2041_2060[,30:41] #columns of avg temp for each month
tavg3_4160 <- as.data.frame(t(tavg3_4160)) 

ppt3_4160 <- ssp370_2041_2060[,42:53] #columns of avg precip for each month
ppt3_4160 <- as.data.frame(t(ppt3_4160))

f3_41_60 <- data.frame(fintry,tavg3_4160$V1,ppt3_4160$V1, 1:12, rep("2041-2060",times=12)) #dataframe for fintry
colnames(f3_41_60) <- c("park","temp", "ppt", "month", "period")
s3_41_60 <- data.frame(skaha,tavg3_4160$V2,ppt3_4160$V2, 1:12, rep("2041-2060",times=12)) #dataframe for skaha
colnames(s3_41_60) <- c("park","temp", "ppt", "month", "period")
m3_41_60 <- data.frame(manning,tavg3_4160$V3,ppt3_4160$V3, 1:12, rep("2041-2060",times=12)) #dataframe for manning
colnames(m3_41_60) <- c("park","temp", "ppt", "month", "period")

ssp3_41_60 <- rbind(f3_41_60, s3_41_60, m3_41_60) #combine 3 park dataframes into one for this scenario & period



#2061-2080 
ssp370_2061_2080 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp370_2061-2080.csv") 
ssp370_2061_2080 <- ssp370_2061_2080 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg3_6180 <- ssp370_2061_2080[,30:41] #columns of avg temp for each month
tavg3_6180 <- as.data.frame(t(tavg3_6180)) 

ppt3_6180 <- ssp370_2061_2080[,42:53] #columns of avg precip for each month
ppt3_6180 <- as.data.frame(t(ppt3_6180))

f3_61_80 <- data.frame(fintry,tavg3_6180$V1,ppt3_6180$V1, 1:12, rep("2061-2080",times=12)) #dataframe for fintry
colnames(f3_61_80) <- c("park","temp", "ppt", "month", "period")
s3_61_80 <- data.frame(skaha,tavg3_6180$V2,ppt3_6180$V2, 1:12, rep("2061-2080",times=12)) #dataframe for skaha
colnames(s3_61_80) <- c("park","temp", "ppt", "month", "period")
m3_61_80 <- data.frame(manning,tavg3_6180$V3,ppt3_6180$V3, 1:12, rep("2061-2080",times=12)) #dataframe for manning
colnames(m3_61_80) <- c("park","temp", "ppt", "month", "period")

ssp3_61_80 <- rbind(f3_61_80, s3_61_80, m3_61_80) #combine 3 park dataframes into one for this scenario & period


#2081-2100 
ssp370_2081_2100 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp370_2081-2100.csv") 
ssp370_2081_2100 <- ssp370_2081_2100 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg3_81100 <- ssp370_2081_2100[,30:41] #columns of avg temp for each month
tavg3_81100 <- as.data.frame(t(tavg3_81100)) 

ppt3_81100 <- ssp370_2081_2100[,42:53] #columns of avg precip for each month
ppt3_81100 <- as.data.frame(t(ppt3_81100))

f3_81_100 <- data.frame(fintry,tavg3_81100$V1,ppt3_81100$V1, 1:12, rep("2081-2100",times=12)) #dataframe for fintry
colnames(f3_81_100) <- c("park","temp", "ppt", "month", "period")
s3_81_100 <- data.frame(skaha,tavg3_81100$V2,ppt3_81100$V2, 1:12, rep("2081-2100",times=12)) #dataframe for skaha
colnames(s3_81_100) <- c("park","temp", "ppt", "month", "period")
m3_81_100 <- data.frame(manning,tavg3_81100$V3,ppt3_81100$V3, 1:12, rep("2081-2100",times=12)) #dataframe for manning
colnames(m3_81_100) <- c("park","temp", "ppt", "month", "period")

ssp3_81_100 <- rbind(f3_81_100, s3_81_100, m3_81_100) #combine 3 park dataframes into one for this scenario & period

ssp3 <- rbind(ssp3_21_40, ssp3_41_60, ssp3_61_80, ssp3_81_100) #combine all periods for this scenario
ssp3$ssp <- rep("3-7.0",times=144)



##SSP 5-8.5
#2021-2040 
ssp585_2021_2040 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp585_2021-2040.csv") #import data
ssp585_2021_2040 <- ssp585_2021_2040 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg5_2140 <- ssp585_2021_2040[,30:41] #columns of avg temp for each month
tavg5_2140 <- as.data.frame(t(tavg5_2140)) 

ppt5_2140 <- ssp585_2021_2040[,42:53] #columns of avg precip for each month
ppt5_2140 <- as.data.frame(t(ppt5_2140))

f5_21_40 <- data.frame(fintry,tavg5_2140$V1,ppt5_2140$V1, 1:12, rep("2021-2040",times=12)) #dataframe for fintry
colnames(f5_21_40) <- c("park","temp", "ppt", "month", "period")
s5_21_40 <- data.frame(skaha,tavg5_2140$V2,ppt5_2140$V2, 1:12, rep("2021-2040",times=12)) #dataframe for skaha
colnames(s5_21_40) <- c("park","temp", "ppt", "month", "period")
m5_21_40 <- data.frame(manning,tavg5_2140$V3,ppt5_2140$V3, 1:12, rep("2021-2040",times=12)) #dataframe for manning
colnames(m5_21_40) <- c("park","temp", "ppt", "month", "period")

ssp5_21_40 <- rbind(f5_21_40, s5_21_40, m5_21_40) #combine 3 park dataframes into one for this scenario & period

#2041-2060 
ssp585_2041_2060 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp585_2041-2060.csv") 
ssp585_2041_2060 <- ssp585_2041_2060 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg5_4160 <- ssp585_2041_2060[,30:41] #columns of avg temp for each month
tavg5_4160 <- as.data.frame(t(tavg5_4160)) 

ppt5_4160 <- ssp585_2041_2060[,42:53] #columns of avg precip for each month
ppt5_4160 <- as.data.frame(t(ppt5_4160))

f5_41_60 <- data.frame(fintry,tavg5_4160$V1,ppt5_4160$V1, 1:12, rep("2041-2060",times=12)) #dataframe for fintry
colnames(f5_41_60) <- c("park","temp", "ppt", "month", "period")
s5_41_60 <- data.frame(skaha,tavg5_4160$V2,ppt5_4160$V2, 1:12, rep("2041-2060",times=12)) #dataframe for skaha
colnames(s5_41_60) <- c("park","temp", "ppt", "month", "period")
m5_41_60 <- data.frame(manning,tavg5_4160$V3,ppt5_4160$V3, 1:12, rep("2041-2060",times=12)) #dataframe for manning
colnames(m5_41_60) <- c("park","temp", "ppt", "month", "period")

ssp5_41_60 <- rbind(f5_41_60, s5_41_60, m5_41_60) #combine 3 park dataframes into one for this scenario & period



#2061-2080 
ssp585_2061_2080 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp585_2061-2080.csv") 
ssp585_2061_2080 <- ssp585_2061_2080 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg5_6180 <- ssp585_2061_2080[,30:41] #columns of avg temp for each month
tavg5_6180 <- as.data.frame(t(tavg5_6180)) 

ppt5_6180 <- ssp585_2061_2080[,42:53] #columns of avg precip for each month
ppt5_6180 <- as.data.frame(t(ppt5_6180))

f5_61_80 <- data.frame(fintry,tavg5_6180$V1,ppt5_6180$V1, 1:12, rep("2061-2080",times=12)) #dataframe for fintry
colnames(f5_61_80) <- c("park","temp", "ppt", "month", "period")
s5_61_80 <- data.frame(skaha,tavg5_6180$V2,ppt5_6180$V2, 1:12, rep("2061-2080",times=12)) #dataframe for skaha
colnames(s5_61_80) <- c("park","temp", "ppt", "month", "period")
m5_61_80 <- data.frame(manning,tavg5_6180$V3,ppt5_6180$V3, 1:12, rep("2061-2080",times=12)) #dataframe for manning
colnames(m5_61_80) <- c("park","temp", "ppt", "month", "period")

ssp5_61_80 <- rbind(f5_61_80, s5_61_80, m5_61_80) #combine 3 park dataframes into one for this scenario & period


#2081-2100 
ssp585_2081_2100 <- read_csv("Data/area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp585_2081-2100.csv") 
ssp585_2081_2100 <- ssp585_2081_2100 %>% slice(7012,8282,8771) #select rows of coordinates closest to each park center

tavg5_81100 <- ssp585_2081_2100[,30:41] #columns of avg temp for each month
tavg5_81100 <- as.data.frame(t(tavg5_81100)) 

ppt5_81100 <- ssp585_2081_2100[,42:53] #columns of avg precip for each month
ppt5_81100 <- as.data.frame(t(ppt5_81100))

f5_81_100 <- data.frame(fintry,tavg5_81100$V1,ppt5_81100$V1, 1:12, rep("2081-2100",times=12)) #dataframe for fintry
colnames(f5_81_100) <- c("park","temp", "ppt", "month", "period")
s5_81_100 <- data.frame(skaha,tavg5_81100$V2,ppt5_81100$V2, 1:12, rep("2081-2100",times=12)) #dataframe for skaha
colnames(s5_81_100) <- c("park","temp", "ppt", "month", "period")
m5_81_100 <- data.frame(manning,tavg5_81100$V3,ppt5_81100$V3, 1:12, rep("2081-2100",times=12)) #dataframe for manning
colnames(m5_81_100) <- c("park","temp", "ppt", "month", "period")

ssp5_81_100 <- rbind(f5_81_100, s5_81_100, m5_81_100) #combine 3 park dataframes into one for this scenario & period

ssp5 <- rbind(ssp5_21_40, ssp5_41_60, ssp5_61_80, ssp5_81_100) #combine all periods for this scenario
ssp5$ssp <- rep("5-8.5",times=144)


##Combine all SSPs into one dataset

climate <- rbind(ssp1, ssp2, ssp3, ssp5)
