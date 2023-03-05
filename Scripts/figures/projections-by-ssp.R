library(ggplot2)

# Import the attendance projections for relevant pop growth scenarios
LGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/LG-attendance-projections.rds")
HGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/HG-attendance-projections.rds")

# Make a date column
LGattendance$date <- paste(paste(LGattendance$year, LGattendance$month, sep = "-"), 15, sep = "-")
LGattendance$date <- as.POSIXct(LGattendance$date, format = "%Y-%m-%d")

# Compare all regions by relative growth in visitors
ggplot(data = LGattendance, 
       aes(x = date, y = relative_visitors)) +
  geom_smooth(size=0.5,se=F, aes(group = park, col = region)) + # line for each park
  geom_line(y=1, size = 0.5, color = "grey40") + 
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  ggtitle("Park Attendance under Low Population Growth") +
  scale_y_continuous(limits = c(0.5,1.75)) +
  scale_color_manual(name="Region", 
                     values=alpha(c('ok'="#C5ABE7",
                                    'north'="#47007A",
                                    'tc'="#85D4FF",
                                    'west'="#34A02C",
                                    'south'="#1f78b4"), 0.1),
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +
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
        legend.box.background = element_rect(color = "black"),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
