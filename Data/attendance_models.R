ok2019_2022$park <- as.factor(ok2019_2022$park)
names(ok2019_2022)[names(ok2019_2022) == 'visitorcorrected'] <- 'attendance'

#Fit the model
tempfit <- glmer(attendance ~ avgtemp  + (avgtemp|park),
             family = Gamma(link="log"),
             data = ok2019_2022)

summary(tempfit)

logit_tempfit <- function(x) {
  B_0 <- 0.73072
  B_1 <- 0.06345
  mu = exp(B_0 + B_1*x)
  mu
}

tempdata <- data.frame(avgtemp = seq(-10,30, 0.1),
                       attendance = logit_fit(seq(-10,30, 0.1)))
  

fintrytemp <- data.frame(avgtemp = seq(-10,30, 0.1),
                    park = "fintry")

skahatemp <- data.frame(avgtemp = seq(-10,30, 0.1),
                    park = "skaha")

manningtemp <- data.frame(avgtemp = seq(-10,30, 0.1),
                    park = "manning")

fintrytemp$fintry <- predict(tempfit, newdata = fintrytemp, type = "response")
skahatemp$skaha <- predict(tempfit, newdata = skahatemp, type = "response")
manningtemp$manning <- predict(tempfit, newdata = manningtemp, type = "response")

#FIG <- 
ggplot() +
  geom_point(data = ok2019_2022, aes(y = attendance, x = avgtemp, col = park),
             alpha = 1, stroke = 0, shape=16, size = 2) +
  geom_smooth(data = tempdata, aes(x = avgtemp, y = attendance), se = F, span = 1.5, col = "black", size = 1.25) +
  geom_line(data = fintrytemp, aes(x = avgtemp, y = fintry), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = skahatemp, aes(x = avgtemp, y = skaha), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = manningtemp, aes(x = avgtemp, y = manning), se = F, span = 1.5, col = "grey40", size = 0.5) +
  
  scale_y_continuous(limits = c(0,15), expand = c(0,1)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Monthly Temperature (ºC)") +
  ylab("Park Visitors (per 1000 people)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "left",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
