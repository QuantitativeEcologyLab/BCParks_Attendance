library(ggplot2)

# Import the attendance projections for relevant pop growth scenarios
LGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/LG-attendance-projections.rds")
HGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/HG-attendance-projections.rds")

#Figures for Kootenay-Okanagan Region
ggplot(data = LGattendance[which(LGattendance$region == "ok"),], 
       aes(x = year, y = relative_visitors)) +
  geom_smooth(size=0.3,se=F, aes(group = park), col = "#E8D6FF") + # show a line for each park
  geom_smooth(se = F, col = "#C5ABE7", size = 1.5) + # show average trend
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("Kootenay-Okanagan Park Attendance under Low Population Growth") +
  scale_y_continuous(limits = c(-0.5,0.5)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot(data = HGattendance[which(HGattendance$region == "ok"),], 
       aes(x = year, y = relative_visitors)) +
  geom_smooth(size=0.3,se=F, aes(group = park), col = "#E8D6FF") + # show a line for each park
  geom_smooth(se = F, col = "#C5ABE7", size = 1.5) + # show average trend
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("Kootenay-Okanagan Park Attendance under High Population Growth") +
  scale_y_continuous(limits = c(-15,30)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Figures for Northern Region
ggplot(data = LGattendance[which(LGattendance$region == "north"),], 
       aes(x = year, y = relative_visitors)) +
  geom_smooth(size=0.3,se=F, aes(group = park), col = "#E5C2FF") + # show a line for each park
  geom_smooth(se = F, col = "#47007A", size = 1.5) + # show average trend
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("Northern Park Attendance under Low Population Growth") +
  scale_y_continuous(limits = c(-0.3,0.15)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot(data = HGattendance[which(HGattendance$region == "north"),], 
       aes(x = year, y = relative_visitors)) +
  geom_smooth(size=0.3,se=F, aes(group = park), col = "#E5C2FF") + # show a line for each park
  geom_smooth(se = F, col = "#47007A", size = 1.5) + # show average trend
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("Northern Park Attendance under High Population Growth") +
  scale_y_continuous(limits = c(-5,20)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Figures for South Coast Region
ggplot(data = LGattendance[which(LGattendance$region == "south"),], 
       aes(x = year, y = relative_visitors)) +
  geom_smooth(size=0.3,se=F, aes(group = park), col = "#C2EAFF") + # show a line for each park
  geom_smooth(se = F, col = "#85D4FF", size = 1.5) + # show average trend
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("South Coast Park Attendance under Low Population Growth") +
  scale_y_continuous(limits = c(-0.25,0.15)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot(data = HGattendance[which(HGattendance$region == "south"),], 
       aes(x = year, y = relative_visitors)) +
  geom_smooth(size=0.3,se=F, aes(group = park), col = "#C2EAFF") + # show a line for each park
  geom_smooth(se = F, col = "#85D4FF", size = 1.5) + # show average trend
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("South Coast Park Attendance under High Population Growth") +
  scale_y_continuous(limits = c(-10,20)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Figures for Thompson-Cariboo Region
ggplot(data = LGattendance[which(LGattendance$region == "tc"),], 
       aes(x = year, y = relative_visitors)) +
  geom_smooth(size=0.3,se=F, aes(group = park), col = "#A8D3F0") + # show a line for each park
  geom_smooth(se = F, col = "#1f78b4", size = 1.5) + # show average trend
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("Thompson-Cariboo Park Attendance under Low Population Growth") +
  scale_y_continuous(limits = c(-0.1,0.25)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot(data = HGattendance[which(HGattendance$region == "tc"),], 
       aes(x = year, y = relative_visitors)) +
  geom_smooth(size=0.3,se=F, aes(group = park), col = "#A8D3F0") + # show a line for each park
  geom_smooth(se = F, col = "#1f78b4", size = 1.5) + # show average trend
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("Thompson-Cariboo Park Attendance under High Population Growth") +
  scale_y_continuous(limits = c(-5,20)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Figures for West Coast Region
ggplot(data = LGattendance[which(LGattendance$region == "west"),], 
       aes(x = year, y = relative_visitors)) +
  geom_smooth(size=0.3,se=F, aes(group = park), col = "#C2EDBF") + # show a line for each park
  geom_smooth(se = F, col = "#34A02C", size = 1.5) + # show average trend
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("West Coast Park Attendance under Low Population Growth") +
  scale_y_continuous(limits = c(-0.1,0.3)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot(data = HGattendance[which(HGattendance$region == "west"),], 
       aes(x = year, y = relative_visitors)) +
  geom_smooth(size=0.3,se=F, aes(group = park), col = "#C2EDBF") + # show a line for each park
  geom_smooth(se = F, col = "#34A02C", size = 1.5) + # show average trend
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("West Coast Park Attendance under High Population Growth") +
  scale_y_continuous(limits = c(-10,20)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



# Figures combining all regions
ggplot(data = LGattendance, 
       aes(x = year, y = relative_visitors)) +
  # geom_smooth(size=0.3, se=F, aes(group = park, col = region)) + # show a line for each park
  geom_smooth(se = F, size = 1.5, aes(col = region), alpha = 0.5) + # show average trends
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  scale_color_manual(name = "Region",
                     values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF"),
                     labels=c('Kootenay-Okanagan', 
                              'Northern', 
                              'Thompson-Cariboo',
                              'West Coast',
                              'South Coast')) + 
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("BC Parks Attendance under Low Population Growth") +
  # scale_y_continuous(limits = c(0.07,0.21)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot(data = HGattendance, 
       aes(x = year, y = predicted_visitors)) +
  # geom_smooth(size=0.3, se=F, aes(group = park, col = region)) + # show a line for each park
  geom_smooth(se = F, size = 1, aes(col = region)) + # show average trends
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  scale_color_manual(name = "Park in Region...",
                     values=c('north'="#47007A",
                              'ok'="#C5ABE7",
                              'south'="#85D4FF",
                              'tc'="#1f78b4",
                              'west'="#34A02C"),
                     labels=c('Northern', 
                              'Kootenay-Okanagan',
                              'South Coast',
                              'Thompson-Cariboo',
                              'West Coast')) + 
  xlab("Year") +
  ylab("Average Visitors for a Single Park") +
  ggtitle("Average Regional Park Attendance under High Population Growth") +
  # scale_y_continuous(limits = c(0.07,0.21)) +
  scale_x_continuous(limits = c(2020,2100)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.2, size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
