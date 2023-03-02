library(ggplot2)

# Import the historical data
bcparks = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/bcparks.rds")
bcparks$year <- as.numeric(bcparks$year) # fix year format
bcparks <- bcparks[!bcparks$attendancetype == "camping",] # remove historical camping data (not relevant)

# Import the attendance projections for relevant pop growth scenarios
LGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/LG-attendance-projections.rds")
HGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/HG-attendance-projections.rds")

# Disable scientific notation (for y-axis)
options(scipen = 999)

# Show change in attendance for population growth scenarios, plot all SSP scenarios
ggplot() + 
  geom_smooth(data = LGattendance[which(LGattendance$park == "Cypress Park"),], 
              aes(x = year, y = predicted_visitors, col = ssp), 
              size=1, se = F) +  # predictions for each ssp
  geom_smooth(data = bcparks[which(bcparks$park == "Cypress Park"),],
              aes(x = year, y = visitortotal),
              col = "black",  size = 1, se = F) + # include historical data
  xlab("Year") +
  ylab("Predicted Visitors") +
  ggtitle("Attendance under Low Population Growth") +
  scale_color_brewer(type = 'div', palette = 5, direction = -1, name="Climate Scenario") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 14, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 10, family = "sans"),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
ggplot() + 
  geom_smooth(data = HGattendance[which(HGattendance$park == "Cypress Park"),], 
              aes(x = year, y = predicted_visitors, col = ssp), 
              size=1, se = F) +  # predictions for each ssp
  geom_smooth(data = bcparks[which(bcparks$park == "Cypress Park"),],
              aes(x = year, y = visitortotal),
              col = "black",  size = 1, se = F) + # include historical data
  xlab("Year") +
  ylab("Predicted Visitors") +
  ggtitle("Attendance under High Population Growth") +
  scale_color_brewer(type = 'div', palette = 5, direction = -1, name="Climate Scenario") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 14, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 10, family = "sans"),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# Show seasonal trends separated by SSP
ggplot() +
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_smooth(data = LGattendance[which(LGattendance$park == "Cypress Park"),], 
              aes(x = month, y = predicted_attendance, group = year, col = year), 
              size=0.1, se = F) + # lines for each year
  geom_smooth(data = bcparks[which(bcparks$park == "Cypress Park"),],
              aes(x = month, y = attendance),
              col = "black", size = 1, span = 1,se = F) + # for historical attendance
  xlab("Month") +
  ylab("Predicted Visitors") +
  ggtitle("Cypress Park Attendance under Low Population Growth") +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_colour_viridis_c(name = "Year") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans", angle = 45, hjust = 1),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "left",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        legend.margin=margin(c(8,8,8,8)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


