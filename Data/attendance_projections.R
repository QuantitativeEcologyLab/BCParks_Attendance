library(ggplot2)

#Source in the climate projection data
source("~/Desktop/bio/440/BCParks_Attendance/Data/climatedata.R")

#Source in the population projection data for LG/HG
source("~/Desktop/bio/440/BCParks_Attendance/Data/LG_HGpopprojs.R")

#Source in the models and historical data
source("~/Desktop/bio/440/BCParks_Attendance/Data/modelfinal.R")
#Label historical data as having period "2019-2022"
ok2019_2022$period <- "2019-2022"

#DOUBLECHECK !! divide avg TOTAL precipitation in a month to get avg DAILY precip over a month
climate$avgprecip <- climate$avgprecip/30

#Predict Attendance per 1,000 based on CC projections
climate$predicted_attendance <- predict(M, newdata = climate, type = "response")

#Make datasets for different population growth scenarios: low growth (LG) and high growth (HG)
climateLG <- merge(climate,LGrates,by=c("year"))
climateHG <- merge(climate,HGrates,by=c("year"))
rm(LGrates,HGrates)


#Project visitor counts for each population growth scenario
for(i in 1:nrow(climateLG)){
  climateLG[i,'predicted_visitors'] <- climateLG[i,12]/1000*climateLG[i,9]
}
for(i in 1:nrow(climateHG)){
  climateHG[i,'predicted_visitors'] <- climateHG[i,12]/1000*climateHG[i,9]
}


#Plot Visitor Counts under Low Population Growth
ggplot(data = climateLG[which(climateLG$park == "manning"),], 
       aes(x = month, y = predicted_visitors, col = period)) +
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_point() +
  geom_smooth(size=0.5, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "manning"),], 
              aes(x = month, y = visitortotal), size = 1, linetype = "solid", se = F) +
  xlab("Month") +
  ylab("Predicted Visitors") +
  ggtitle("Manning Attendance under Low Population Growth") +
  scale_color_manual(name="Period", 
                     values=c('2019-2022'="black",
                              '2021-2040'="#3b528b",
                              '2041-2060'="#21918c",
                              '2061-2080'="#5ec962",
                              '2081-2100'="#fde725")) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans", angle = 55, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


#Plot Visitor Counts under High Population Growth
ggplot(data = climateHG[which(climateHG$park == "manning"),], 
       aes(x = month, y = predicted_visitors, col = period)) +
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_point() +
  geom_smooth(size=0.5, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "manning"),], 
              aes(x = month, y = visitortotal), size = 1, linetype = "solid", se = F) +
  xlab("Month") +
  ylab("Predicted Visitors") +
  ggtitle("Manning Attendance under High Population Growth") +
  scale_color_manual(name="Period", 
                     values=c('2019-2022'="black",
                              '2021-2040'="#3b528b",
                              '2041-2060'="#21918c",
                              '2061-2080'="#5ec962",
                              '2081-2100'="#fde725")) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans", angle = 55, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Compare historical temps vs. predicted temps
ggplot() +
  geom_boxplot(data = ok2019_2022, aes(x = month, y = avgtemp, group = cut_width(month, 1), alpha = 0.5)) +
  geom_point(data = climate, aes(x = month, y = avgtemp, col = period), size = 2) +
  scale_y_continuous(limits = c(0,30)) +
  scale_color_viridis(discrete = T) +
  xlab("Month") +
  ylab("Temperature") +
  ggtitle("Temperature") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Compare historical precipitation vs. predicted precipitation
ggplot() +
  geom_boxplot(data = ok2019_2022, aes(x = month, y = avgprecip, group = cut_width(month, 1), alpha = 0.5)) +
  geom_point(data = climate, aes(x = month, y = avgprecip, col = period), size = 2) +
  #scale_y_continuous(limits = c(0,80)) +
  scale_color_viridis(discrete = T) +
  xlab("Month") +
  ylab("Total precipitation") +
  ggtitle("Total precipitation") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
