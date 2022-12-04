setwd("~/Desktop/bio/440/BCParks_Attendance")
ok1922NA <- read.csv(file = 'Data/ok1922NA.csv')
ok2019_2022 <- na.omit(ok1922NA)
ok2019_2022$park <- as.factor(ok2019_2022$park)
names(ok2019_2022)[names(ok2019_2022) == 'visitorcorrected'] <- 'attendance'

library(lme4)
library(ggplot2)
library(viridis)
library(dplyr)
library(mgcv)

ok2019_2022$month2 <- factor(ok2019_2022$month,
                             ordered = TRUE,
                             levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
ok2019_2022$date <- paste(paste(ok2019_2022$year, ok2019_2022$month, sep = "-"), 15, sep = "-")
ok2019_2022$date <- as.POSIXct(ok2019_2022$date, format = "%Y-%b-%d")
ok2019_2022$month <- format(ok2019_2022$date, "%m")
ok2019_2022$month <- as.numeric(ok2019_2022$month)


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

##NEW MONTHLY FIGS using GAM
M <- gam(attendance ~
           s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 11) +
           s(avgtemp) + s(log(avgprecip)) + avgtemp:log(avgprecip) +
           month:avgtemp + month:log(avgprecip),
         family = Gamma(link = 'log'),
         data = ok2019_2022[-which(ok2019_2022$attendance>120),],
         method = 'REML',
         knots = list(month = c(0.5, 12.5)))
ok2019_2022[-which(ok2019_2022$attendance>120),]$prediction <- predict(M, type = "response")
fintry <- filter(ok2019_2022, park == "fintry")
skaha <- filter(ok2019_2022, park == "skaha")
manning <- filter(ok2019_2022, park == "manning")


fintrymonth <- aggregate(avgtemp ~ month, data = ok2019_2022, FUN = "median")
fintrymonth$avgprecip <- aggregate(avgprecip ~ month, data = ok2019_2022, FUN = "median")$avgprecip
fintrymonth$park <- "fintry"

str(fintrymonth)
str(ok2019_2022)
fintrymonth$prediction <- predict(M, newdata = fintrymonth, type = "response")

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
skahamonth$prediction <- predict(M, newdata = skahamonth, type = "response")

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
manningmonth$prediction <- predict(M, newdata = manningmonth, type = "response")

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
