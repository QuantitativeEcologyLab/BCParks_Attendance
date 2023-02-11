library(ggplot2)
library(viridis)
library(tidyverse)

#Source in the models and historical data (TAKES A HOT MINUTE)
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/models/dayuse-models.R")

#Build datasets of a couple random parks in each region
group_var <- bcparks %>% 
  group_by(park) %>%
  groups %>%
  unlist %>% 
  as.character
#NORTHERN REGION
north_parks <- bcparks[which(bcparks$region == "north" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  sample_n(2) %>% 
  mutate(unique_id=1:NROW(.))
northpredict <- bcparks[which(bcparks$region == "north" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park)  %>% 
  right_join(north_parks, by=group_var) %>%
  group_by_(group_var)
#SOUTH COAST
south_parks <- bcparks[which(bcparks$region == "south" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  sample_n(2) %>% 
  mutate(unique_id=1:NROW(.))
southpredict <- bcparks[which(bcparks$region == "south" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park)  %>% 
  right_join(south_parks, by=group_var) %>%
  group_by_(group_var)
#WEST COAST
west_parks <- bcparks[which(bcparks$region == "west" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  sample_n(2) %>% 
  mutate(unique_id=1:NROW(.))
westpredict <- bcparks[which(bcparks$region == "west" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park)  %>% 
  right_join(west_parks, by=group_var) %>%
  group_by_(group_var)
#THOMPSON-CARIBOO
tc_parks <- bcparks[which(bcparks$region == "tc" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  sample_n(2) %>% 
  mutate(unique_id=1:NROW(.))
tcpredict <- bcparks[which(bcparks$region == "tc" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park)  %>% 
  right_join(tc_parks, by=group_var) %>%
  group_by_(group_var)
#KOOTENAY-OKANAGAN
ok_parks <- bcparks[which(bcparks$region == "ok" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  sample_n(2) %>% 
  mutate(unique_id=1:NROW(.))
okpredict <- bcparks[which(bcparks$region == "ok" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park)  %>% 
  right_join(ok_parks, by=group_var) %>%
  group_by_(group_var)
#clean up a bit!
rm(north_parks,south_parks,west_parks,tc_parks,ok_parks)

#"Predict" 2010-2019 attendance for these few parks using both models separately
#NORTHERN REGION
northpredict$alldatapredict <- predict(M_alldata, newdata = northpredict, type = "response")
northpredict$newdatapredict <- predict(M_newdata, newdata = northpredict, type = "response")
#SOUTH COAST
southpredict$alldatapredict <- predict(M_alldata, newdata = southpredict, type = "response")
southpredict$newdatapredict <- predict(M_newdata, newdata = southpredict, type = "response")
#WEST COAST
westpredict$alldatapredict <- predict(M_alldata, newdata = westpredict, type = "response")
westpredict$newdatapredict <- predict(M_newdata, newdata = westpredict, type = "response")
#THOMPSON-CARIBOO
tcpredict$alldatapredict <- predict(M_alldata, newdata = tcpredict, type = "response")
tcpredict$newdatapredict <- predict(M_newdata, newdata = tcpredict, type = "response")
#KOOTENAY-OKANAGAN
okpredict$alldatapredict <- predict(M_alldata, newdata = okpredict, type = "response")
okpredict$newdatapredict <- predict(M_newdata, newdata = okpredict, type = "response")


#Visually compare historical vs. model-predicted attendance to make sure models are behaving 
#NORTHERN REGION
#predictions based on all data
ggplot() +
  geom_boxplot(data = northpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = northpredict, aes(x = month, y = alldatapredict), size = 2, alpha = 0.5, col = "orange") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Park visitors (per 1000 people)") +
  ggtitle("Northern Parks: historical vs projected attendance (all data)") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
#predictions based on only 2010-2019 BC parks data (excluding Okanagan 2019-2022 data)
ggplot() +
  geom_boxplot(data = northpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = northpredict, aes(x = month, y = newdatapredict), size = 2, alpha = 0.5, col = "orange") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Park visitors (per 1000 people)") +
  ggtitle("Northern Parks: historical vs projected attendance (new data)") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
#SOUTH COAST REGION
#predictions based on all data
ggplot() +
  geom_boxplot(data = southpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = southpredict, aes(x = month, y = alldatapredict), size = 2, alpha = 0.5, col = "mediumaquamarine") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Park visitors (per 1000 people)") +
  ggtitle("South Coast Parks: historical vs projected attendance (all data)") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
#predictions based on only 2010-2019 BC parks data (excluding Okanagan 2019-2022 data)
ggplot() +
  geom_boxplot(data = southpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = southpredict, aes(x = month, y = newdatapredict), size = 2, alpha = 0.5, col = "mediumaquamarine") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Park visitors (per 1000 people)") +
  ggtitle("South Coast Parks: historical vs projected attendance (new data)") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
#WEST COAST REGION
#predictions based on all data
ggplot() +
  geom_boxplot(data = westpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = westpredict, aes(x = month, y = alldatapredict), size = 2, alpha = 0.5, col = "steelblue") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Park visitors (per 1000 people)") +
  ggtitle("West Coast Parks: historical vs projected attendance (all data)") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
#predictions based on only 2010-2019 BC parks data (excluding Okanagan 2019-2022 data)
ggplot() +
  geom_boxplot(data = westpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = westpredict, aes(x = month, y = newdatapredict), size = 2, alpha = 0.5, col = "steelblue") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Park visitors (per 1000 people)") +
  ggtitle("West Coast Parks: historical vs projected attendance (new data)") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
#THOMPSON-CARIBOO REGION
#predictions based on all data
ggplot() +
  geom_boxplot(data = tcpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = tcpredict, aes(x = month, y = alldatapredict), size = 2, alpha = 0.5, col = "gold") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Park visitors (per 1000 people)") +
  ggtitle("Thompson-Cariboo Parks: historical vs projected attendance (all data)") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
#predictions based on only 2010-2019 BC parks data (excluding Okanagan 2019-2022 data)
ggplot() +
  geom_boxplot(data = tcpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = tcpredict, aes(x = month, y = newdatapredict), size = 2, alpha = 0.5, col = "gold") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Park visitors (per 1000 people)") +
  ggtitle("Thompson-Cariboo Parks: historical vs projected attendance (new data)") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
#KOOTENAY-OKANAGAN REGION
#predictions based on all data
ggplot() +
  geom_boxplot(data = okpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = okpredict, aes(x = month, y = alldatapredict), size = 2, alpha = 0.5, col = "forestgreen") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Park visitors (per 1000 people)") +
  ggtitle("Kootenay-Okanagan Parks: historical vs projected attendance (all data)") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
#predictions based on only 2010-2019 BC parks data (excluding Okanagan 2019-2022 data)
ggplot() +
  geom_boxplot(data = okpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = okpredict, aes(x = month, y = newdatapredict), size = 2, alpha = 0.5, col = "forestgreen") +
  scale_color_viridis(discrete = T) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  xlab("Month") +
  ylab("Park visitors (per 1000 people)") +
  ggtitle("Kootenay-Okanagan Parks: historical vs projected attendance (new data)") +
  facet_wrap(~ unique_id, labeller = as_labeller(c(
    `1` = "Random park 1",
    `2` = "Random park 2"))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=9, family = "sans"),
        axis.text.x  = element_text(size=9, angle = 45, family = "sans"),
        plot.title = element_text(hjust = -0.05, size = 12, family = "sans", face = "bold"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
