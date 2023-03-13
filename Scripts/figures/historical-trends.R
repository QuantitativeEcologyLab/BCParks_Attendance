library(ggplot2)
library(gridExtra)

# Import the historical data
bcparks = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/bcparks.rds")

# Attendance by month: to see seasonal trends
seasonal <-
  ggplot() +
  geom_jitter(data = bcparks[which(bcparks$attendancetype == "dayuse"),],
             aes(y = attendance, x = month, col = region),
             alpha = 0.05, size = 1, width = 0.25, shape = 2) +
  geom_smooth(data = bcparks[which(bcparks$attendancetype == "dayuse"),], 
              aes(y = attendance, x = month, col = region),
              size = 0.6, se = F) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(limits = c(0, 15)) +
  scale_colour_muted(name="Region",
                      labels=c('Northern', 
                               'Kootenay-Okanagan', 
                               'South Coast', 
                               'Thompson-Cariboo', 
                               'West Coast')) +
  xlab("Month") +
  ylab("Visitors (per 1000 people)") +
  ggtitle("B") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans"),
        axis.title.x = element_text(size=10, family = "sans"),
        axis.text.y = element_text(size=7, family = "sans"),
        axis.text.x  = element_text(size=7, family = "sans"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 25, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# Attendance in relation to temperature & precipitation: to see effect of weather in each month

january <-
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "1"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "1"),]$attendance),
             # alpha = 0.5, 
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in January") +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

february <- 
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "2"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "2"),]$attendance),
             # alpha = 0.5,
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in February") +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

march <- 
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "3"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "3"),]$attendance),
             # alpha = 0.5,
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in March") +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

april <-
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "4"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "4"),]$attendance),
             # alpha = 0.5,
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in April") +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

may <- 
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "5"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = log(bcparks[which(bcparks$month == "5"),]$attendance),
             # alpha = 0.5,
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in May") +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

june <- 
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "6"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "6"),]$attendance),
             # alpha = 0.5,
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  scale_y_continuous(limits = c(0,10)) +
  ggtitle("Attendance in June") +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

july <- 
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "7"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "7"),]$attendance),
             # alpha = 0.5,
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in July") +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

august <-
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "8"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "8"),]$attendance),
             alpha = 0.5,
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("C") +
  labs(subtitle = "August Attendance") +
  scale_y_continuous(limits = c(0,7)) +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans"),
        axis.title.x = element_text(size=10, family = "sans"),
        axis.text.y = element_text(size=7, family = "sans"),
        axis.text.x  = element_text(size=7, family = "sans"),
        plot.subtitle = element_text(size = 10, family = "sans", face = "bold"),
        plot.title = element_text(size = 25, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

september <- 
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "9"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "9"),]$attendance),
             # alpha = 0.5,
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in September") +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

october <- 
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "10"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "10"),]$attendance),
             # alpha = 0.5,
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in October") +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

november <- 
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "11"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "11"),]$attendance),
             # alpha = 0.5,
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in November") +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

december <-
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "12"),], 
             aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "12"),]$attendance),
             # alpha = 0.5,
             shape = 1) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("D") +
  labs(subtitle = "December Attendance") +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans"),
        axis.title.x = element_text(size=10, family = "sans"),
        axis.text.y = element_text(size=7, family = "sans"),
        axis.text.x  = element_text(size=7, family = "sans"),
        plot.subtitle = element_text(size = 10, family = "sans", face = "bold"),
        plot.title = element_text(size = 25, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# make a plot combining august/december observations to get a common legend 
plot <-
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "8"),], 
             aes(y = avgprecip, x = avgtemp, 
                 size = sqrt(bcparks[which(bcparks$month == "8"),]$attendance)),
             # alpha = 0.2,
             shape = 1) +
  geom_point(data = bcparks[which(bcparks$month == "12"),], 
             aes(y = avgprecip, x = avgtemp, 
                 size = sqrt(bcparks[which(bcparks$month == "12"),]$attendance)),
             # alpha = 0.2,
             shape = 1) +
  labs(size='Visitors \n(per 1000 people)') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=7, family = "sans"),
        axis.text.x  = element_text(size=7, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 25, family = "sans", face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.box.background = element_rect(color = "black"),
        legend.position = "right",
        legend.direction = "vertical",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#save all plots
ggsave(plot,
       width = 8, height = 6, units = "in",
       dpi = 600,
       bg = "white",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/figure1.png")
ggsave(seasonal,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/monthly-regional-attendance.png")
ggsave(january,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/jan-temp-precip.png")
ggsave(february,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/feb-temp-precip.png")
ggsave(march,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/mar-temp-precip.png")
ggsave(april,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/apr-temp-precip.png")
ggsave(may,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/may-temp-precip.png")
ggsave(june,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/jun-temp-precip.png")
ggsave(july,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/jul-temp-precip.png")
ggsave(august,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/aug-temp-precip.png")
ggsave(september,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/sep-temp-precip.png")
ggsave(october,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/oct-temp-precip.png")
ggsave(november,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/nov-temp-precip.png")
ggsave(december,
       width = 7.21, height = 6.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/historical trends/dec-temp-precip.png")

