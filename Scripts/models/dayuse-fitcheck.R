library(ggplot2)
library(viridis)
library(dplyr)
library(mgcv)

# Import the historical data
bcparks = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/bcparks.rds")
# Import the model
M = readRDS("~/Desktop/bio/440/BCParks_Attendance/Scripts/models/model.rda")

#Build datasets of a couple random parks in each region
group <- bcparks %>% 
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
  right_join(north_parks, by=group) %>%
  group_by_(group)
#SOUTH COAST
south_parks <- bcparks[which(bcparks$region == "south" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  sample_n(2) %>% 
  mutate(unique_id=1:NROW(.))
southpredict <- bcparks[which(bcparks$region == "south" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park)  %>% 
  right_join(south_parks, by=group) %>%
  group_by_(group)
#WEST COAST
west_parks <- bcparks[which(bcparks$region == "west" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  sample_n(2) %>% 
  mutate(unique_id=1:NROW(.))
westpredict <- bcparks[which(bcparks$region == "west" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park)  %>% 
  right_join(west_parks, by=group) %>%
  group_by_(group)
#THOMPSON-CARIBOO
tc_parks <- bcparks[which(bcparks$region == "tc" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  sample_n(2) %>% 
  mutate(unique_id=1:NROW(.))
tcpredict <- bcparks[which(bcparks$region == "tc" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park)  %>% 
  right_join(tc_parks, by=group) %>%
  group_by_(group)
#KOOTENAY-OKANAGAN
ok_parks <- bcparks[which(bcparks$region == "ok" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park) %>% 
  summarise() %>% 
  sample_n(2) %>% 
  mutate(unique_id=1:NROW(.))
okpredict <- bcparks[which(bcparks$region == "ok" & bcparks$attendancetype == "dayuse"),] %>% 
  group_by(park)  %>% 
  right_join(ok_parks, by=group) %>%
  group_by_(group)
#clean up a bit!
rm(north_parks,south_parks,west_parks,tc_parks,ok_parks)

#"Predict" 2010-2019 attendance for these few parks using both models separately
#NORTHERN REGION
northpredict$predict <- predict(M, newdata = northpredict, type = "response")
#SOUTH COAST
southpredict$predict <- predict(M, newdata = southpredict, type = "response")
#WEST COAST
westpredict$predict <- predict(M, newdata = westpredict, type = "response")
#THOMPSON-CARIBOO
tcpredict$predict <- predict(M, newdata = tcpredict, type = "response")
#KOOTENAY-OKANAGAN
okpredict$predict <- predict(M, newdata = okpredict, type = "response")


#Visually compare historical vs. model-predicted attendance to make sure models are behaving 
#NORTHERN REGION
ggplot() +
  geom_boxplot(data = northpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = northpredict, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "orange") +
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

#SOUTH COAST REGION
ggplot() +
  geom_boxplot(data = southpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = southpredict, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "mediumaquamarine") +
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

#WEST COAST REGION
ggplot() +
  geom_boxplot(data = westpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = westpredict, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "steelblue") +
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

#THOMPSON-CARIBOO REGION
ggplot() +
  geom_boxplot(data = tcpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = tcpredict, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "gold") +
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

#KOOTENAY-OKANAGAN REGION
ggplot() +
  geom_boxplot(data = okpredict, aes(x = month, y = attendance, group = cut_width(month, 1))) +
  geom_point(data = okpredict, aes(x = month, y = predict), size = 2, alpha = 0.5, col = "forestgreen") +
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

rm(northpredict,southpredict,okpredict,tcpredict,westpredict)

# model that includes all data (M_all data) seems to behave just fine...let's use that