library(ggplot2)
library(viridis)

# Source in growth rate datasets
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/projections/popgrowth-projections.R") 

# Plot growth rates
population <- 
  ggplot() +
  geom_point(data = data, aes(y = growthrate, x = year, col = scenario),
             alpha = 1, stroke = 0, shape=16, size = 2) +
  geom_smooth(data = DATA, aes(x = year, y = growthrate), se = F, span = 1.5, col = "black", size = 1.1) +
  geom_line(data = LGrates, aes(x = year, y = LG), col = "grey40", size = 0.5) +
  geom_line(data = M1rates, aes(x = year, y = M1), col = "grey40", size = 0.5) +
  geom_line(data = M2rates, aes(x = year, y = M2), col = "grey40", size = 0.5) +
  geom_line(data = M3rates, aes(x = year, y = M3), col = "grey40", size = 0.5) +
  geom_line(data = M4rates, aes(x = year, y = M4), col = "grey40", size = 0.5) +
  geom_line(data = M5rates, aes(x = year, y = M5), col = "grey40", size = 0.5) +
  geom_line(data = HGrates, aes(x = year, y = HG), col = "grey40", size = 0.5) +
  geom_line(data = SArates, aes(x = year, y = SA), col = "grey40", size = 0.5) +
  geom_line(data = FArates, aes(x = year, y = FA), col = "grey40", size = 0.5) +
  scale_y_continuous(limits = c(0,15), expand = c(0,1)) +
  scale_color_viridis(discrete = T, 
                      name = "Scenario",
                      labels = c("Fast-Aging", 
                                 "High Growth", 
                                 "Low Growth",
                                 "Medium Growth 1",
                                 "Medium Growth 2",
                                 "Medium Growth 3",
                                 "Medium Growth 4",
                                 "Medium Growth 5",
                                 "Slow-Aging")) +
  xlab("Year") +
  ylab("BC population growth rate (people per 1000)") +
  ggtitle("B.C. Population Growth Rate Projections") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
population

# Save Figure
ggsave(population,
       width = 7.21, height = 5.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/projections/population-growth-rates.png")