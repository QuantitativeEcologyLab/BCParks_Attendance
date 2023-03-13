library(ggplot2)
library(ggh4x) # to fill in facet wrap title boxes

# Import the attendance projections for relevant pop growth scenarios
LGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/LG-attendance-projections.rds")
HGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/HG-attendance-projections.rds")

# Make a date column
LGattendance$date <- paste(paste(LGattendance$year, LGattendance$month, sep = "-"), 15, sep = "-")
LGattendance$date <- as.POSIXct(LGattendance$date, format = "%Y-%m-%d")
HGattendance$date <- paste(paste(HGattendance$year, HGattendance$month, sep = "-"), 15, sep = "-")
HGattendance$date <- as.POSIXct(HGattendance$date, format = "%Y-%m-%d")

# Remove outlier park (?)
LGattendance <- LGattendance[-which(LGattendance$park == "Mount Assiniboine Park"),]
HGattendance <- HGattendance[-which(HGattendance$park == "Mount Assiniboine Park"),]
 
# Calculate relative change in attendance (relative to first prediction)
LGattendance <- LGattendance %>%
  group_by(ssp, year, park) %>% # don't compare different SSPs or parks to each other
  mutate(predicted_attendance = sum(predicted_attendance))
LGattendance <- group_by(LGattendance, park, ssp) %>%
  mutate(relative_visitors = predicted_attendance / first(predicted_attendance))
# Calculate relative change in attendance (relative to first prediction)
HGattendance <- HGattendance %>%
  group_by(ssp, year, park) %>% # don't compare different SSPs or parks to each other
  mutate(predicted_attendance = sum(predicted_attendance))
HGattendance <- group_by(HGattendance, park, ssp) %>%
  mutate(relative_visitors = predicted_attendance / first(predicted_attendance))

# make colour strips in x-direction for panel title boxes
strip <- strip_themed(background_x = 
                        elem_list_rect(fill = c("#0571b0", "#92c5de", "#f4a582", "#ca0020")))

# Compare all regions by relative growth in visitors (same values for 
# both LG and HG, so just pick one dataset)
projections <-
  ggplot(LGattendance, aes(x = year, y = relative_visitors)) + 
  geom_line(y=1, size = 0.5, color = "grey70") + # line at 1
  geom_line(size=0.5, aes(group = park, col = region), alpha = 0.1) + # line for each park
  facet_wrap2(~ ssp, strip = strip, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  xlab("Year") +
  ylab("Relative Change in Visitors") +
  # ggtitle("BC Parks Attendance Predictions") +
  scale_y_continuous(limits = c(0.75,1.9)) +
  guides(col = guide_legend(override.aes = list(alpha=1))) +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.title = element_text(face = "bold"),
        legend.box.background = element_rect(color = "black"),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# save the figure
ggsave(projections,
       width = 8.21, height = 5.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/figure2.png")
