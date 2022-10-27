setwd("~/Downloads")
library(lme4)
library(ggplot2)
library(viridis)

data <- read.csv("popgrowth.csv")
data$scenario <- as.factor(data$scenario)
data$growth2 <- data$growthrate/100

#Fit the model
FIT <- glmer(growthrate ~ year + (year|scenario),
             family = Gamma(link="log"),
             data = data)

summary(FIT)

Logit_Fit <- function(x) {
  B_0 <- 52.4796834
  B_1 <- -0.0247877
  mu = exp(B_0 + B_1*x)
  mu
}

DATA <- data.frame(year = seq(2022,2100, 0.1),
                   growthrate = Logit_Fit(seq(2022,2100, 0.1)))


DATA2 <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "LG")

DATA3 <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "M1")

DATA4 <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "M2")

DATA5 <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "M3")

DATA6 <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "M4")

DATA7 <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "M5")

DATA8 <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "HG")

DATA9 <- data.frame(year = seq(2022,2100, 0.1),
                    scenario = "SA")

DATA10 <- data.frame(year = seq(2022,2100, 0.1),
                     scenario = "FA")

DATA2$LG <- predict(FIT, newdata = DATA2, type = "response")
DATA3$M1 <- predict(FIT, newdata = DATA3, type = "response")
DATA4$M2 <- predict(FIT, newdata = DATA4, type = "response")
DATA5$M3 <- predict(FIT, newdata = DATA5, type = "response")
DATA6$M4 <- predict(FIT, newdata = DATA6, type = "response")
DATA7$M5 <- predict(FIT, newdata = DATA7, type = "response")
DATA8$HG <- predict(FIT, newdata = DATA8, type = "response")
DATA9$SA <- predict(FIT, newdata = DATA9, type = "response")
DATA10$FA <- predict(FIT, newdata = DATA10, type = "response")

#FIG <- 
ggplot() +
  geom_point(data = data, aes(y = growthrate, x = year, col = scenario),
             alpha = 1, stroke = 0, shape=16, size = 2) +
  geom_smooth(data = DATA, aes(x = year, y = growthrate), se = F, span = 1.5, col = "black") +
  geom_line(data = DATA2, aes(x = year, y = LG), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = DATA3, aes(x = year, y = M1), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = DATA4, aes(x = year, y = M2), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = DATA5, aes(x = year, y = M3), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = DATA6, aes(x = year, y = M4), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = DATA7, aes(x = year, y = M5), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = DATA8, aes(x = year, y = HG), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = DATA9, aes(x = year, y = SA), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = DATA10, aes(x = year, y = FA), se = F, span = 1.5, col = "grey40", size = 0.5) +
  
  scale_y_continuous(limits = c(0,20), expand = c(0,1)) +
  #scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015)) +
  scale_color_viridis(discrete = T) +
  ylab("BC population growth rate") +
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

