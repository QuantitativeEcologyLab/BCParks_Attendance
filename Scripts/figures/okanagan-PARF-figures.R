library(ggplot2)

#Source in the attendance projections
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/projections/okanagan-attendance-projections.R")

#Plot Seasonal Visitor Counts under Low Population Growth
ggplot(data = climateLG[which(climateLG$park == "manning"),], 
       aes(x = month, y = predicted_visitors, col = period)) +
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_point() +
  geom_smooth(size=0.5, span = 0.5, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "manning"),], 
              aes(x = month, y = visitortotal), size = 1, span = 0.95, se = F) +
  xlab("Month") +
  ylab("Predicted Visitors") +
  ggtitle("Manning Attendance under Low Population Growth") +
  scale_color_manual(name="Period", 
                     values=c('Historical'="black",
                              '2021-2040'="#3b528b",
                              '2041-2060'="#21918c",
                              '2061-2080'="#5ec962",
                              '2081-2100'="#E2CC00")) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(labels = scales::comma, limits = c(0,400000)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 55, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



ggplot(data = climateLG[which(climateLG$park == "fintry"),], 
       aes(x = month, y = predicted_visitors, col = period)) +
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_point() +
  geom_smooth(size=0.5, span = 0.5, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], 
              aes(x = month, y = visitortotal), size = 1, span = 0.95, se = F) +
  xlab("Month") +
  ylab("Predicted Visitors") +
  ggtitle("Fintry Attendance under Low Population Growth") +
  scale_color_manual(name="Period", 
                     values=c('Historical'="black",
                              '2021-2040'="#3b528b",
                              '2041-2060'="#21918c",
                              '2061-2080'="#5ec962",
                              '2081-2100'="#E2CC00")) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(labels = scales::comma, limits = c(0,40000)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 55, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



ggplot(data = climateLG[which(climateLG$park == "skaha"),], 
       aes(x = month, y = predicted_visitors, col = period)) +
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_point() +
  geom_smooth(size=0.5, span = 0.75, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], 
              aes(x = month, y = visitortotal), size = 1, span = 0.95, se = F) +
  xlab("Month") +
  ylab("Predicted Visitors") +
  ggtitle("Skaha Attendance under Low Population Growth") +
  scale_color_manual(name="Period", 
                     values=c('Historical'="black",
                              '2021-2040'="#3b528b",
                              '2041-2060'="#21918c",
                              '2061-2080'="#5ec962",
                              '2081-2100'="#E2CC00")) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(labels = scales::comma, limits = c(0,40000)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 55, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


#Plot Seasonal Visitor Counts under High Population Growth
options(scipen = 999) #disable scientific notation (for y axis)

ggplot(data = climateHG[which(climateHG$park == "manning"),], 
       aes(x = month, y = predicted_visitors, col = period)) +
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_point() +
  geom_smooth(size=0.5, span = 0.7, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "manning"),], 
              aes(x = month, y = visitortotal), size = 1, span = 0.95, linetype = "solid", se = F) +
  xlab("Month") +
  ylab("Predicted Visitors") +
  ggtitle("Manning Attendance under High Population Growth") +
  scale_color_manual(name="Period", 
                     values=c('Historical'="black",
                              '2021-2040'="#3b528b",
                              '2041-2060'="#21918c",
                              '2061-2080'="#5ec962",
                              '2081-2100'="#E2CC00")) +
  scale_fill_brewer(type = 'div', palette = 5, direction = -1) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 55, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "left",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


ggplot(data = climateHG[which(climateHG$park == "fintry"),], 
       aes(x = month, y = predicted_visitors, col = period)) +
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_point() +
  geom_smooth(size=0.5, span = 0.5, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], 
              aes(x = month, y = visitortotal), size = 1, span = 0.95, se = F) +
  xlab("Month") +
  ylab("Predicted Visitors") +
  ggtitle("Fintry Attendance under High Population Growth") +
  scale_color_manual(name="Period", 
                     values=c('Historical'="black",
                              '2021-2040'="#3b528b",
                              '2041-2060'="#21918c",
                              '2061-2080'="#5ec962",
                              '2081-2100'="#E2CC00")) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 55, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "left",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


ggplot(data = climateHG[which(climateHG$park == "skaha"),], 
       aes(x = month, y = predicted_visitors, col = period)) +
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_point() +
  geom_smooth(size=0.4, span = 0.75, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], 
              aes(x = month, y = visitortotal), size = 1, span = 0.95, se = F) +
  xlab("Month") +
  ylab("Predicted Visitors") +
  ggtitle("Skaha Attendance under High Population Growth") +
  scale_color_manual(name="Period", 
                     values=c('Historical'="black",
                              '2021-2040'="#3b528b",
                              '2041-2060'="#21918c",
                              '2061-2080'="#5ec962",
                              '2081-2100'="#E2CC00")) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 55, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "left",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


#HG: Plot attendance vs years up to 2100 
ok2019_2022$ssp <- "Historical" #dataset needs an 'ssp' column

ggplot(data = climateHG[which(climateHG$park == "manning"),], 
       aes(x = year, y = predicted_visitors, col = ssp)) +
  geom_smooth(size=1, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "manning"),], aes(x = year, y = visitortotal), size = 1, col = "black", se = F) +
  xlab("Year") +
  ylab("Predicted Monthly Visitors") +
  ggtitle("Manning Attendance under High Population Growth") +
  scale_color_brewer(type = 'div', palette = 5, direction = -1, name="Climate Scenario") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 25, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans"),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot(data = climateHG[which(climateHG$park == "fintry"),], 
       aes(x = year, y = predicted_visitors, col = ssp)) +
  geom_smooth(size=1, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], aes(x = year, y = visitortotal),  col = "black", size = 1, se = F) +
  xlab("Year") +
  ylab("Predicted Monthly Visitors") +
  ggtitle("Fintry Attendance under High Population Growth") +
  scale_color_brewer(type = 'div', palette = 5, direction = -1, name="Climate Scenario") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 25, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 14, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 10, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot(data = climateHG[which(climateHG$park == "skaha"),], 
       aes(x = year, y = predicted_visitors, col = ssp)) +
  geom_smooth(size=1, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], aes(x = year, y = visitortotal),  col = "black", size = 1, se = F) +
  xlab("Year") +
  ylab("Predicted Monthly Visitors") +
  ggtitle("Skaha Attendance under High Population Growth") +
  scale_color_brewer(type = 'div', palette = 5, direction = -1, name="Climate Scenario") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 25, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 14, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans"),
        legend.text=element_text(size=10),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


#LG: Plot attendance vs years up to 2100 
ggplot(data = climateLG[which(climateLG$park == "manning"),], 
       aes(x = year, y = predicted_visitors, col = ssp)) +
  geom_smooth(size=1, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "manning"),], aes(x = year, y = visitortotal), col = "black",  size = 1, se = F) +
  xlab("Year") +
  ylab("Predicted Monthly Visitors") +
  ggtitle("Manning Attendance under Low Population Growth") +
  scale_color_brewer(type = 'div', palette = 5, direction = -1, name="Climate Scenario") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 25, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 14, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 10, family = "sans"),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


ggplot(data = climateLG[which(climateLG$park == "fintry"),], 
       aes(x = year, y = predicted_visitors, col = ssp)) +
  geom_smooth(size=1, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], aes(x = year, y = visitortotal), col = "black", size = 1, se = F) +
  xlab("Year") +
  ylab("Predicted Monthly Visitors") +
  ggtitle("Fintry Attendance under Low Population Growth") +
  scale_color_brewer(type = 'div', palette = 5, direction = -1, name="Climate Scenario") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 25, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 14, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 10, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggplot(data = climateLG[which(climateLG$park == "skaha"),], 
       aes(x = year, y = predicted_visitors, col = ssp)) +
  geom_smooth(size=1, se = F) +
  geom_smooth(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], aes(x = year, y = visitortotal), col = "black",  size = 1, se = F) +
  xlab("Year") +
  ylab("Predicted Monthly Visitors") +
  ggtitle("Skaha Attendance under Low Population Growth") +
  scale_color_brewer(type = 'div', palette = 5, direction = -1, name="Climate Scenario") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 25, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 14, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))