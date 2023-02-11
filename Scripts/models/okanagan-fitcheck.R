library(lme4)
library(ggplot2)
library(viridis)
library(dplyr)
library(mgcv)

#Source in Okanagan Model
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/models/okanagan-model.R")

#Make predictions
ok2019_2022[-which(ok2019_2022$attendance>120),]$prediction <- predict(M_olddata, type = "response")
fintry <- filter(ok2019_2022, park == "fintry")
skaha <- filter(ok2019_2022, park == "skaha")
manning <- filter(ok2019_2022, park == "manning")


fintrymonth <- aggregate(avgtemp ~ month, data = ok2019_2022, FUN = "median")
fintrymonth$avgprecip <- aggregate(avgprecip ~ month, data = ok2019_2022, FUN = "median")$avgprecip
fintrymonth$park <- "fintry"

fintrymonth$prediction <- predict(M_olddata, newdata = fintrymonth, type = "response")

fintrygam <- 
  ggplot() +
  geom_boxplot(data = fintry, aes(x = month, y = attendance, group = cut_width(month, 1), alpha = 0.5)) +
  geom_point(data = fintrymonth, aes(x = month, y = prediction), size = 2, col = "royalblue") +
  scale_y_continuous(limits = c(0,5)) +
  scale_x_continuous(limits = c(0,12)) +
  scale_color_viridis(discrete = T) +
  xlab("Month") +
  ylab("Park Visitors (per 1000 people)") +
  ggtitle("Fintry Attendance") +
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
fintrygam

skahamonth <- aggregate(avgtemp ~ month, data = ok2019_2022, FUN = "median")
skahamonth$avgprecip <- aggregate(avgprecip ~ month, data = ok2019_2022, FUN = "median")$avgprecip
skahamonth$park <- "skaha"
skahamonth$prediction <- predict(M_olddata, newdata = skahamonth, type = "response")

skahagam <- 
  ggplot() +
  geom_boxplot(data = skaha, aes(x = month, y = attendance, group = cut_width(month, 1), alpha = 0.5)) +
  geom_point(data = skahamonth, aes(x = month, y = prediction), size = 2, col = "royalblue") +
  scale_y_continuous(limits = c(0,5)) +
  scale_color_viridis(discrete = T) +
  xlab("Month") +
  ylab("Park Visitors (per 1000 people)") +
  ggtitle("Skaha Attendance") +
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
skahagam

manningmonth <- aggregate(avgtemp ~ month, data = ok2019_2022, FUN = "median")
manningmonth$avgprecip <- aggregate(avgprecip ~ month, data = ok2019_2022, FUN = "median")$avgprecip
manningmonth$park <- "manning"
manningmonth$prediction <- predict(M_olddata, newdata = manningmonth, type = "response")

manninggam <- 
  ggplot() +
  geom_boxplot(data = manning, aes(x = month, y = attendance, group = cut_width(month, 1), alpha = 0.5)) +
  geom_point(data = manningmonth, aes(x = month, y = prediction), size = 2, col = "royalblue") +
  scale_y_continuous(limits = c(0,46)) +
  scale_color_viridis(discrete = T) +
  xlab("Month") +
  ylab("Park Visitors (per 1000 people)") +
  ggtitle("Manning Attendance") +
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
manninggam

#-------------------------------------------
# The following figures are for the original (unused) GLMs we fit
#-------------------------------------------

##TEMPERATURE MODEL
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
                       attendance = logit_tempfit(seq(-10,30, 0.1)))


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
tempfig <- ggplot() +
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
tempfig

##PRECIPITATION MODEL
precipfit <- glmer(attendance ~ avgprecip  + avgtemp + (avgprecip|park),
                   family = Gamma(link="log"),
                   data = ok2019_2022)

summary(precipfit)

logit_precipfit <- function(x) {
  B_0 <- 0.720613
  B_1 <- -0.007362
  B_2 <- 0.064408
  mu = exp(B_0 + B_1*x)
  mu
}

precipdata <- data.frame(avgprecip = seq(0.1,17, 0.01),
                         attendance = logit_precipfit(seq(0.1,17, 0.01)))


fintryprecip <- data.frame(avgprecip = seq(0.1,17, 0.01),
                           avgtemp = 10,
                           park = "fintry")

skahaprecip <- data.frame(avgprecip = seq(0.1,17, 0.01),
                          avgtemp = 10,
                          park = "skaha")

manningprecip <- data.frame(avgprecip = seq(0.1,17, 0.01),
                            avgtemp = 10,
                            park = "manning")

fintryprecip$fintry <- predict(precipfit, newdata = fintryprecip, type = "response")
skahaprecip$skaha <- predict(precipfit, newdata = skahaprecip, type = "response")
manningprecip$manning <- predict(precipfit, newdata = manningprecip, type = "response")

#FIG <- 
precipfig <- ggplot() +
  geom_point(data = ok2019_2022, aes(y = attendance, x = avgprecip, col = park),
             alpha = 1, stroke = 0, shape=16, size = 2) +
  geom_smooth(data = precipdata, aes(x = avgprecip, y = attendance), se = F, span = 1.5, col = "black", size = 1.25) +
  geom_line(data = fintryprecip, aes(x = avgprecip, y = fintry), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = skahaprecip, aes(x = avgprecip, y = skaha), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = manningprecip, aes(x = avgprecip, y = manning), se = F, span = 1.5, col = "grey40", size = 0.5) +
  
  scale_y_continuous(limits = c(0,45), expand = c(0,1)) +
  scale_color_viridis(discrete = T) +
  xlab("Average Monthly Precipitation (mm)") +
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
precipfig

##POPULATION MODEL
ok2019_2022$population <- ok2019_2022$BCpop/1000000
#glmer won't run with numbers in the BCpop column, i think they're too big

popfit <- glmer(attendance ~ population  + (population|park),
                family = Gamma(link="log"),
                data = ok2019_2022)

summary(popfit)

logit_popfit <- function(x) {
  B_0 <- -5.108
  B_1 <- 1.299
  mu = exp(B_0 + B_1*x)
  mu
}

popdata <- data.frame(population = seq(5.05,5.3, 0.01),
                      attendance = logit_popfit(seq(5.05,5.3, 0.01)))


fintrypop <- data.frame(population = seq(5.05,5.3, 0.01),
                        park = "fintry")

skahapop <- data.frame(population = seq(5.05,5.3, 0.01),
                       park = "skaha")

manningpop <- data.frame(population = seq(5.05,5.3, 0.01),
                         park = "manning")

fintrypop$fintry <- predict(popfit, newdata = fintrypop, type = "response")
skahapop$skaha <- predict(popfit, newdata = skahapop, type = "response")
manningpop$manning <- predict(popfit, newdata = manningpop, type = "response")

#FIG <- 
popfig <- ggplot() +
  geom_point(data = ok2019_2022, aes(y = attendance, x = population, col = park),
             alpha = 1, stroke = 0, shape=16, size = 2) +
  geom_smooth(data = popdata, aes(x = population, y = attendance), se = F, span = 1.5, col = "black", size = 1.25) +
  geom_line(data = fintrypop, aes(x = population, y = fintry), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = skahapop, aes(x = population, y = skaha), se = F, span = 1.5, col = "grey40", size = 0.5) +
  geom_line(data = manningpop, aes(x = population, y = manning), se = F, span = 1.5, col = "grey40", size = 0.5) +
  
  scale_y_continuous(limits = c(0,30), expand = c(0,1)) +
  scale_color_viridis(discrete = T) +
  xlab("BC Population (millions)") +
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
popfig

##MONTH MODEL FOR EACH PARK
monthfit <- glmer(attendance ~ month + (month|park),
                  family = Gamma(link="log"),
                  data = ok2019_2022)

fintrymonth <- data.frame(date = unique(ok2019_2022$date),
                          avgtemp = median(ok2019_2022$avgtemp),
                          avgprecip = median(ok2019_2022$avgprecip),
                          park = "fintry")
fintrymonth$attendance <- predict(monthfit, newdata = fintrymonth, type = "response")
fintrymonth$month <- format(fintrymonth$date, "%m")

skahamonth <- data.frame(date = unique(ok2019_2022$date),
                         avgtemp = median(ok2019_2022$avgtemp),
                         avgprecip = median(ok2019_2022$avgprecip),
                         park = "skaha")
skahamonth$attendance <- predict(monthfit, newdata = skahamonth, type = "response")
skahamonth$month <- format(skahamonth$date, "%m")

manningmonth <- data.frame(date = unique(ok2019_2022$date),
                           avgtemp = median(ok2019_2022$avgtemp),
                           avgprecip = median(ok2019_2022$avgprecip),
                           park = "manning")
manningmonth$attendance <- predict(monthfit, newdata = manningmonth, type = "response")
manningmonth$month <- format(manningmonth$date, "%m")

fintrymonthly <- 
  ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "fintry"),], aes(x = month, y = attendance), alpha = 0.5) +
  geom_point(data = fintrymonth, aes(x = month, y = attendance), size = 2, col = "royalblue") +
  scale_y_continuous(limits = c(0,17), expand = c(0,1)) +
  scale_x_discrete(limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_color_viridis(discrete = T) +
  xlab("Month") +
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
fintrymonthly

skahamonthly <- 
  ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "skaha"),], aes(x = month, y = attendance), alpha = 0.5) +
  geom_point(data = skahamonth, aes(x = month2, y = attendance), size = 2, col = "royalblue") +
  scale_y_continuous(limits = c(0,20), expand = c(0,1)) +
  scale_x_discrete(limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_color_viridis(discrete = T) +
  xlab("Month") +
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
skahamonthly

manningmonthly <- 
  ggplot() +
  geom_boxplot(data = ok2019_2022[which(ok2019_2022$park == "manning"),], aes(x = month2, y = attendance), alpha = 0.5) +
  geom_point(data = manningmonth, aes(x = month2, y = attendance), size = 2, col = "royalblue") +
  scale_y_continuous(limits = c(0,60), expand = c(0,1)) +
  scale_x_discrete(limits = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")) +
  scale_color_viridis(discrete = T) +
  xlab("Month") +
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
manningmonthly


