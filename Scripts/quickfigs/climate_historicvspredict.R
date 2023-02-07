library(ggplot2)
library(viridis)

#Source in the climate projection data
source("~/Desktop/bio/440/BCParks_Attendance/Data/climatedata.R")

#Source in the population projection data for LG/HG
source("~/Desktop/bio/440/BCParks_Attendance/Data/LG_HGpopprojs.R")

#Source in the models and historical data
source("~/Desktop/bio/440/BCParks_Attendance/Data/modelfinal.R")
#Label historical data as having period/ssp "Historical"
ok2019_2022$period <- "Historical"

#Divide avg TOTAL precipitation in a month to get avg DAILY precip over a month
climate$avgprecip <- climate$avgprecip/30

#Predict Attendance per 1,000 based on CC projections
climate$predicted_attendance <- predict(M, newdata = climate, type = "response")

#Make datasets for different population growth scenarios: low growth (LG) and high growth (HG)
climateLG <- merge(climate,LGrates,by=c("year"))

climateHG <- merge(climate,HGrates,by=c("year"))

rm(LGrates,HGrates,ssp1,ssp2,ssp3,ssp5,fintry,manning,skaha)

#Project visitor counts for each population growth scenario
for(i in 1:nrow(climateLG)){
  climateLG[i,'predicted_visitors'] <- climateLG[i,12]/1000*climateLG[i,9]
}
for(i in 1:nrow(climateHG)){
  climateHG[i,'predicted_visitors'] <- climateHG[i,12]/1000*climateHG[i,9]
}

#Compare historical temps vs. predicted temps for each park and SSP

#Fintry
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "fintry" & climate$ssp == "1-2.6"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Fintry Temperature (SSP 1-2.6)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "fintry" & climate$ssp == "2-4.5"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Fintry Temperature (SSP 2-4.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "fintry" & climate$ssp == "3-7.0"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Fintry Temperature (SSP 3-7.0)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "fintry" & climate$ssp == "5-8.5"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Fintry Temperature (SSP 5-8.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Manning
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "manning"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "manning" & climate$ssp == "1-2.6"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Manning Temperature (SSP 1-2.6)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "manning"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "manning" & climate$ssp == "2-4.5"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Manning Temperature (SSP 2-4.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "manning"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "manning" & climate$ssp == "3-7.0"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Manning Temperature (SSP 3-7.0)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "manning"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "manning" & climate$ssp == "5-8.5"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Manning Temperature (SSP 5-8.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


#Skaha
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "skaha" & climate$ssp == "1-2.6"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Skaha Temperature (SSP 1-2.6)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "skaha" & climate$ssp == "2-4.5"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Skaha Temperature (SSP 2-4.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "skaha" & climate$ssp == "3-7.0"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Skaha Temperature (SSP 3-7.0)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], aes(x = month, y = avgtemp, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "skaha" & climate$ssp == "5-8.5"),], aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Temperature (ºC)") +
  ggtitle("Skaha Temperature (SSP 5-8.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Compare historical precipitation vs. predicted precipitation for each park and SSP

#Fintry
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "fintry" & climate$ssp == "1-2.6"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Fintry Precipitation (SSP 1-2.6)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "fintry" & climate$ssp == "2-4.5"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Fintry Precipitation (SSP 2-4.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "fintry" & climate$ssp == "3-7.0"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Fintry Precipitation (SSP 3-7.0)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "fintry" & climate$ssp == "5-8.5"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Fintry Precipitation (SSP 5-8.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Manning
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "manning"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "manning" & climate$ssp == "1-2.6"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Manning Precipitation (SSP 1-2.6)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "manning"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "manning" & climate$ssp == "2-4.5"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Manning Precipitation (SSP 2-4.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "manning"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "manning" & climate$ssp == "3-7.0"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Manning Precipitation (SSP 3-7.0)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "manning"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "manning" & climate$ssp == "5-8.5"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Manning Precipitation (SSP 5-8.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


#Skaha
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "skaha" & climate$ssp == "1-2.6"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Skaha Precipitation (SSP 1-2.6)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "skaha" & climate$ssp == "2-4.5"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Skaha Precipitation (SSP 2-4.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "skaha" & climate$ssp == "3-7.0"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Skaha Precipitation (SSP 3-7.0)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], aes(x = month, y = avgprecip, group = cut_width(month, 1))) +
  geom_point(data = climate[which(climate$park == "skaha" & climate$ssp == "5-8.5"),], aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(-10,30)) +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Avg Precipitation (mm)") +
  ggtitle("Skaha Precipitation (SSP 5-8.5)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
