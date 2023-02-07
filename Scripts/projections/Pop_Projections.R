setwd("~/Downloads")
library(lme4)
library(ggplot2)
library(viridis)

data <- read.csv("Data/popgrowth.csv")
data$scenario <- as.factor(data$scenario)
data$growth2 <- data$growthrate/100

#Fit the model
FIT <- glmer(growthrate ~ year  + (year|scenario),
             family = Gamma(link="log"),
             data = data)

summary(FIT)

Logit_Fit <- function(x) {
  B_0 <- 54.64
  B_1 <- -0.02585
  mu = exp(B_0 + B_1*x)
  mu
}

DATA <- data.frame(year = seq(2022,2100, 0.1),
                   growthrate = Logit_Fit(seq(2022,2100, 0.1)))


LGrates <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "LG")

M1rates <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "M1")

M2rates <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "M2")

M3rates <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "M3")

M4rates <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "M4")

M5rates <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "M5")

HGrates <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "HG")

SArates <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "SA")

FArates <- data.frame(year = seq(2022,2100, 0.1),
                     scenario = "FA")

LGrates$LG <- predict(FIT, newdata = LGrates, type = "response")
M1rates$M1 <- predict(FIT, newdata = M1rates, type = "response")
M2rates$M2 <- predict(FIT, newdata = M2rates, type = "response")
M3rates$M3 <- predict(FIT, newdata = M3rates, type = "response")
M4rates$M4 <- predict(FIT, newdata = M4rates, type = "response")
M5rates$M5 <- predict(FIT, newdata = M5rates, type = "response")
HGrates$HG <- predict(FIT, newdata = HGrates, type = "response")
SArates$SA <- predict(FIT, newdata = SArates, type = "response")
FArates$FA <- predict(FIT, newdata = FArates, type = "response")

#FIG <- 
ggplot() +
  geom_point(data = data, aes(y = growthrate, x = year, col = scenario),
             alpha = 1, stroke = 0, shape=16, size = 2) +
  geom_smooth(data = DATA, aes(x = year, y = growthrate), se = F, span = 1.5, col = "black", size = 1.5) +
  geom_line(data = LGrates, aes(x = year, y = LG), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = M1rates, aes(x = year, y = M1), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = M2rates, aes(x = year, y = M2), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = M3rates, aes(x = year, y = M3), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = M4rates, aes(x = year, y = M4), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = M5rates, aes(x = year, y = M5), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = HGrates, aes(x = year, y = HG), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = SArates, aes(x = year, y = SA), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = FArates, aes(x = year, y = FA), se = F, span = 1.5, col = "grey40", size = 0.5) +
  
  scale_y_continuous(limits = c(0,20), expand = c(0,1)) +
  #scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015)) +
  scale_color_viridis(discrete = T) +
  ylab("BC population growth rate (people per 1000)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
