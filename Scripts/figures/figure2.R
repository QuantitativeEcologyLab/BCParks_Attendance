library(ggplot2)
library(gridExtra)
library(khroma)

# Import the historical data
bcparks = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/bcparks.rds")

# Plot attendance in response to temperature
temp <-
  ggplot(data = bcparks[which(bcparks$attendancetype == "dayuse"),], 
         aes(y = attendance, x = avgtemp)) +
  geom_point(aes(col = region),
             alpha = 0.01) + # points for each observation
  geom_line(stat="smooth",method = "gam",
            aes(col = region),
            alpha = 0.9, size = 0.7, se = F) + # line for each region
  geom_line(stat="smooth",method = "gam",
            alpha = 1, col = "black", size = 1.2, se = T) + # line for overall trend
  scale_y_continuous(limits = c(0,10)) +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  xlab("Average Monthly Temperature (ºC)") +
  ylab("Monthly Park Visitors (per 1000 people)") +
  ggtitle("A") +
  # labs(subtitle = "Attendance and Temperature") +
  guides(col = guide_legend(override.aes = list(alpha=1))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = 0.04, vjust = -5, size = 35, 
                                  family = "sans", face = "bold"),
        # plot.subtitle = element_text(size = 14, family = "sans", face = "bold"),
        legend.position=c(0.21,0.66),
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# Plot attendance in response to precipitation
precip <-
  ggplot(data = bcparks[which(bcparks$attendancetype == "dayuse"),], 
         aes(y = attendance, x = avgprecip)) +
  geom_point(aes(col = region),
             alpha = 0.01) + # points for each observation
  geom_line(stat="smooth",method = "gam",
            aes(col = region),
            alpha = 0.9, size = 0.7, se = F) + # line for each region
  geom_line(stat="smooth",method = "gam",
            alpha = 1, col = "black", size = 1.2, se = T) + # line for overall trend
  scale_y_continuous(limits = c(0,8)) +
  scale_x_continuous(limits = c(0,30)) +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  xlab("Average Monthly Precipitation (mm)") +
  ylab("Monthly Park Visitors (per 1000 people)") +
  ggtitle("B") +
  # labs(subtitle = "Attendance and Precipitation") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = 0.04, vjust = -5, size = 35, 
                                  family = "sans", face = "bold"),
        # plot.subtitle = element_text(size = 10, family = "sans", face = "bold"),
        legend.position = "none",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# Combine the 2 panels
FIG2 <- grid.arrange(temp, precip, 
                     ncol=2, widths = c(6,5.2))

# Save the figure
ggsave(FIG2,
       width = 10, height = 5, units = "in",
       dpi = 600,
       bg = "white",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/figure2.png")

