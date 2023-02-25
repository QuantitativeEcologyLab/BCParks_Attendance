library(ggplot2)
library(gridExtra)

# Import the historical data
bcparks = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/bcparks.rds")

# Attendance by month: to see seasonal trends
seasonal <-
  ggplot() +
  geom_jitter(data = bcparks[which(bcparks$attendancetype == "dayuse"),],
             aes(y = attendance, x = month, col = region),
             alpha = 0.05, size = 0.5, width = 0.25) +
  geom_smooth(data = bcparks[which(bcparks$attendancetype == "dayuse"),], 
              aes(y = attendance, x = month, col = region),
              size = 0.6, se = F) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(limits = c(0, 15)) +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +  
  xlab("Month") +
  ylab("Visitors (per 1000 people)") +
  ggtitle("B") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=7, family = "sans"),
        axis.text.x  = element_text(size=7, family = "sans"),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = -0.03, size = 25, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# Attendance in relation to temperature & precipitation: to see effect of weather in each month

january <- 
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "1"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "1"),]$attendance),
             alpha = 0.5) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in January") +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
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
  geom_point(data = bcparks[which(bcparks$month == "2"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "2"),]$attendance),
             alpha = 0.5) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in February") +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
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
  geom_point(data = bcparks[which(bcparks$month == "3"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "3"),]$attendance),
             alpha = 0.5) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in March") +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
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
  geom_point(data = bcparks[which(bcparks$month == "4"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "4"),]$attendance),
             alpha = 0.5) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in April") +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
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
  geom_point(data = bcparks[which(bcparks$month == "5"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = log(bcparks[which(bcparks$month == "5"),]$attendance),
             alpha = 0.5) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in May") +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
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
  geom_point(data = bcparks[which(bcparks$month == "6"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "6"),]$attendance),
             alpha = 0.3) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  scale_y_continuous(limits = c(0,10)) +
  ggtitle("Attendance in June") +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
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
  geom_point(data = bcparks[which(bcparks$month == "7"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "7"),]$attendance),
             alpha = 0.2) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in July") +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
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
  geom_point(data = bcparks[which(bcparks$month == "8"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "8"),]$attendance),
             alpha = 0.075) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("C") +
  scale_y_continuous(limits = c(0,7)) +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=7, family = "sans"),
        axis.text.x  = element_text(size=7, family = "sans"),
        plot.title = element_text(size = 25, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

september <- 
ggplot() +
  geom_point(data = bcparks[which(bcparks$month == "9"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "9"),]$attendance),
             alpha = 0.3) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in September") +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
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
  geom_point(data = bcparks[which(bcparks$month == "10"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "10"),]$attendance),
             alpha = 0.5) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in October") +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
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
  geom_point(data = bcparks[which(bcparks$month == "11"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "11"),]$attendance),
             alpha = 0.5) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Attendance in November") +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
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
  geom_point(data = bcparks[which(bcparks$month == "12"),], aes(y = avgprecip, x = avgtemp, col = region),
             size = sqrt(bcparks[which(bcparks$month == "12"),]$attendance),
             alpha = 0.2) +
  xlab("Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("D") +
  scale_color_manual(values=c('ok'="#C5ABE7",
                              'north'="#47007A",
                              'tc'="#1f78b4",
                              'west'="#34A02C",
                              'south'="#85D4FF")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=7, family = "sans"),
        axis.text.x  = element_text(size=7, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 25, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


# create Figure 1
# source in map of parks 
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/figures/map-bcparks.R")
# make function to grab legend from map figure
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
# save legend from map
legend <- get_legend(map)
# remove legends from map plot
map <- map + theme(legend.position="none")
# combine all 4 panels
FIG1 <- grid.arrange(arrangeGrob(map + theme(legend.position="none"),
                                 arrangeGrob(seasonal + theme(legend.position="none"),
                                             august + theme(legend.position="none"),
                                             december + theme(legend.position="none")),
                                 ncol=2, widths=c(2,1.3)),
                     legend, nrow=2,heights=c(10, 1))
#save all plots
ggsave(FIG1,
       width = 8, height = 6, units = "in",
       dpi = 600,
       bg = "transparent",
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
