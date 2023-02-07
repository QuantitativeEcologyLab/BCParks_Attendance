#Source in BC Parks data
source("~/Desktop/bio/440/BCParks_Attendance/Data/newdata_tidying.R")


#Attendance/temp relationship for camping
ggplot() +
  geom_smooth(data = bcparks[which(bcparks$attendancetype == "camping"),], aes(y = attendance, x = avgtemp, col = region),
             alpha = 0.5, size = 1, se = F, span =0.1) +
  scale_color_viridis(discrete = T) +
  xlab("Average Monthly Temperature (ºC)") +
  ylab("Park Visitors (per 1000 people)") +
  ggtitle("Camping Attendance and Temperature") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
#Attendance/temp relationship for day use
ggplot() +
  geom_smooth(data = bcparks[which(bcparks$attendancetype == "dayuse"),], aes(y = attendance, x = avgtemp, col = region),
              alpha = 0.5, size = 1, se = F) +
  scale_y_continuous(expand = c(0,1)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Monthly Temperature (ºC)") +
  ylab("Park Visitors (per 1000 people)") +
  ggtitle("Dayuse Attendance and Temperature") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Camping attendance by season
ggplot() +
  geom_smooth(data = bcparks[which(bcparks$attendancetype == "camping"),], aes(y = attendance, x = month, col = region),
              alpha = 0.5, size = 1, se = F) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(expand = c(0,1)) +
  scale_color_viridis(discrete = T) +
  xlab("Month") +
  ylab("Park Visitors (per 1000 people)") +
  ggtitle("Camping Attendance by Season") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Day use attendance by season
ggplot() +
  geom_smooth(data = bcparks[which(bcparks$attendancetype == "dayuse"),], aes(y = attendance, x = month, col = region),
              alpha = 0.5, size = 1, se = F) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_y_continuous(expand = c(0,1)) +
  scale_color_viridis(discrete = T) +
  xlab("Month") +
  ylab("Park Visitors (per 1000 people)") +
  ggtitle("Day-use Attendance by Season") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Camping attendance over the last decade
ggplot() +
  geom_smooth(data = bcparks[which(bcparks$attendancetype == "camping"),], aes(y = attendance, x = date, col = region),
              alpha = 0.5, size = 1, se = F) +
  scale_y_continuous(expand = c(0,1)) +
  scale_color_viridis(discrete = T) +
  xlab("Year") +
  ylab("Park Visitors (per 1000 people)") +
  ggtitle("Camping over the last decade") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

#Day use attendance over the last decade
#FIG <- 
ggplot() +
  #geom_point(data = bcparks[which(bcparks$attendancetype == "dayuse"),], aes(y = attendance, x = date, col = region),
   #          alpha = 0.3, size = 1) +
  
  geom_smooth(data = bcparks[which(bcparks$attendancetype == "dayuse"),], aes(y = attendance, x = date, col = region),
              alpha = 0.5, size = 1, se = F) +
  scale_y_continuous(expand = c(0,1)) +
  scale_color_viridis(discrete = T) +
  xlab("Year") +
  ylab("Park Visitors (per 1000 people)") +
  ggtitle("Day use over the last decade") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

ggsave(FIG, )