library(ggplot2)

#Source in the data 
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/models/okanagan-model.R")

##PRECIP/TEMP BY MONTH
january <- filter(ok2019_2022, month == "jan")
february <- filter(ok2019_2022, month == "feb")
march <- filter(ok2019_2022, month == "mar")
april <- filter(ok2019_2022, month == "apr")
may <- filter(ok2019_2022, month == "may")
june <- filter(ok2019_2022, month == "jun")
july <- filter(ok2019_2022, month == "jul")
august <- filter(ok2019_2022, month == "aug")
september <- filter(ok2019_2022, month == "sep")
october <- filter(ok2019_2022, month == "oct")
november <- filter(ok2019_2022, month == "nov")
december <- filter(ok2019_2022, month == "dec")

#January
janfig <- ggplot() +
  geom_point(data = january, aes(y = avgprecip, x = avgtemp), size = log(january$attendance)) +
  
  scale_y_continuous(limits = c(5,6)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in January") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
janfig

#February
febfig <- ggplot() +
  geom_point(data = february, aes(y = avgprecip, x = avgtemp), size = log(february$attendance)) +
  
  scale_y_continuous(limits = c(3,10)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in February") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
febfig

#March
marfig <- ggplot() +
  geom_point(data = march, aes(y = avgprecip, x = avgtemp), size = log(march$attendance)) +
  
  scale_y_continuous(limits = c(0,3)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in March") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
marfig

#April
aprfig <- ggplot() +
  geom_point(data = april, aes(y = avgprecip, x = avgtemp), size = log(april$attendance)) +
  
  scale_y_continuous(limits = c(0,5)) +
  scale_x_continuous(limits = c(3,10)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in April") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
aprfig

#May
mayfig <- ggplot() +
  geom_point(data = may, aes(y = avgprecip, x = avgtemp), size = log(may$attendance)) +
  
  scale_y_continuous(limits = c(0,2.5)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in May") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
mayfig

#June
junefig <- ggplot() +
  geom_point(data = june, aes(y = avgprecip, x = avgtemp), size = log(june$attendance)) +
  
  scale_y_continuous(limits = c(0,3)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in June") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
junefig

#July
julyfig <- ggplot() +
  geom_point(data = july, aes(y = avgprecip, x = avgtemp), size = log(july$attendance)) +
  
  scale_y_continuous(limits = c(0,2)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in July") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
julyfig

#August
augfig <- ggplot() +
  geom_point(data = august, aes(y = avgprecip, x = avgtemp), size = log(august$attendance)) +
  
  scale_y_continuous(limits = c(0,3)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in August") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
augfig


#September
septfig <- ggplot() +
  geom_point(data = september, aes(y = avgprecip, x = avgtemp), size = log(september$attendance)) +
  
  scale_y_continuous(limits = c(0,3)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in September") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
septfig

#Oct
octfig <- ggplot() +
  geom_point(data = october, aes(y = avgprecip, x = avgtemp), size = log(october$attendance)) +
  
  scale_y_continuous(limits = c(0.5,2)) +
  scale_x_continuous(limits = c(6.75,9.25)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in October") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))
octfig


#November
novfig <- ggplot() +
  geom_point(data = november, aes(y = avgprecip, x = avgtemp), size = log(november$attendance)) +
  
  scale_y_continuous(limits = c(0,17)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in November") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
novfig

#December
decfig <- ggplot() +
  geom_point(data = december, aes(y = avgprecip, x = avgtemp), size = log(december$attendance)) +
  
  scale_y_continuous(limits = c(8,9)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Temperature (ºC)") +
  ylab("Precipitation (mm)") +
  ggtitle("Park Attendance in December") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 16, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
decfig
