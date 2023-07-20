library(mgcv)
library(tidyverse)

setwd("C:/Users/achhen/OneDrive - UBC/Github/bcparks")
# Import the climate projection data
climate = readRDS("RDS/monthly-climate-projections.rds")
# Import park coordinates
park_coordinates <- read.csv("data/park_coordinates.csv")
# Import relevant population projection data (LG/HG)
LGrates <- read.csv('data/LG-rate-projections.csv')
HGrates <- read.csv('data/HG-rate-projections.csv')
# Import attendance projections
LGattendance = readRDS("RDS/LG-attendance-projections.rds")
HGattendance = readRDS("RDS/HG-attendance-projections.rds")
# Import historical attendance
bcparks <- readRDS("RDS/bcparks.rds")

#..................................

# Remove camping data (unused in this study)
bcparks <- bcparks[!bcparks$attendancetype == "camping",] 
# Remove NA values
bcparks <- na.omit(bcparks)
bcparks$region <- as.factor(bcparks$region)
bcparks <- droplevels(bcparks)
#..................................

START <-  Sys.time()
model <- bam(attendance ~
               s(park, bs = 're') +
               s(month, park, bs = 'fs', xt = list(bs = 'cc'), k = 5) +         #random intercept & slope effect of park level trend
               s(avgtemp, bs = 'tp', k = 8) +                                   #global effect of temperature, specify k explicitly (even if it is the default)
               s(log(avgprecip + 1e-10), bs = 'tp', k = 6) +                    #global effect of precipitation, add a tiny number to avgprecip so we don't take log of 0
               ti(avgtemp, log(avgprecip + 1e-10), bs = c('tp', 'tp'), k = 5) + #response to snow
               ti(month, avgtemp, k = 5, bs = c('cc', 'tp')) +                  #what is hot in january is cold in july
               ti(month, log(avgprecip + 1e-10), bs = c('cc', 'tp'), k = 5),
             family = Gamma(link = 'log'),
             data = bcparks,
             method = 'fREML',
             discrete = TRUE,
             control = gam.control(nthreads = 10, trace = TRUE),
             knots = list(month = c(0.5, 12.5)))
saveRDS(model, file = "RDS/model.RDS")
END <-  Sys.time()
TIME_DURATION <- END - START

# Import the model
model <- readRDS("model.RDS")

# Figure 2 ----
# Note: this script relies on previously fitted data and model objects.
# It can take several hours to run 
library(mgcv)
library(marginaleffects)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(khroma)

#Load in the data and model
regions <- aggregate(region ~ park, data = bcparks, FUN = "unique")
#model <- readRDS("~/Downloads/model.rds") #note: this needed to be manually changed from rda to rds

#Number of parks to loop over
PARKS <- levels(bcparks$park)
N <- length(PARKS)

#.......................................................
## Generate the temperature panel (A) ----
#.......................................................
START2 <-  Sys.time()
#Loop over the number of parks
for(i in 1:N){
  
  new_data <- try(datagrid(model = model,
                       park = PARKS[i],#levels(bcparks$park)[i],
                       month = 1:12,
                       avgtemp = seq(min(bcparks$avgtemp),
                                     max(bcparks$avgtemp),
                                     by = 0.1)))
  new_data$estimate <- try(predict(model, newdata = new_data, type = "response"))
  # preds <- try(predictions(model,
  #                          datagrid(park = levels(bcparks$park)[i],
  #                                   month = 1:12,
  #                                   avgtemp = seq(min(bcparks$avgtemp),
  #                                                 max(bcparks$avgtemp),
  #                                                 by = 0.1))))
  #Bind the results
  if(i == 1){res <- new_data}
  if(i>1 && class(new_data$estimate) == "numeric"){res <- rbind(res, new_data)}
  
  #Clear the intermediate variable
  rm(new_data)
}

END2 <-  Sys.time()
END2 - START2

#Save predictions
#save(res, file = "temp_preds.Rda")
#load("temp_preds.Rda")

#Merge and aggregate by region
res2 <- merge(res, regions, by.x = "park", by = "park", all.x = T)

# Average temp effect for each region
temp_preds <- aggregate(estimate ~ avgtemp + region, data = res2, FUN = "mean")

# Average temp effect for each park
temp_park_level <- aggregate(estimate ~ avgtemp + park, data = res2, FUN = "mean")

# Average, population level effect
temp_pop_level <- aggregate(estimate ~ avgtemp, data = res2, FUN = "mean")


### Plot attendance in response to temperature ----
temp <-
  ggplot() +
  geom_point(data = bcparks[which(bcparks$attendancetype == "dayuse"),], 
             aes(y = attendance, x = avgtemp, col = region),
             alpha = 0.05, shape = 16) + # points for each observation
   geom_line(data = temp_pop_level,
             aes(x = avgtemp, estimate),
             alpha = 1, col = "black", linewidth = 1.2) + # line for overall trend
  geom_line(data = temp_preds,
            aes(x = avgtemp, estimate, col = region),
            alpha = 0.9, size = 0.7) + # line for each region
  scale_y_continuous(limits = c(0,15), expand = c(0,0.1)) +
  scale_colour_muted(name="Region",
                     labels=c('Northern',
                              'Kootenay-Okanagan',
                              'South Coast',
                              'Thompson-Cariboo',
                              'West Coast')) +
  xlab("Average Monthly Temperature (ºC)") +
  ylab("Monthly Park Visitors (per 1000 people)") +
  ggtitle("A") +
  guides(col = guide_legend(override.aes = list(alpha=1))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = 0.04, vjust = -6, size = 20, 
                                  family = "sans", face = "bold"),
        # plot.subtitle = element_text(size = 14, family = "sans", face = "bold"),
        legend.position=c(0.20,0.66),
        legend.key = element_rect(color = NA),
        #legend.box.background = element_rect(color = "black"),
        legend.title = element_text(face = "bold"),
        legend.background=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
temp
# ggsave(temp, filename = "figures/temp.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)


#.......................................................
## Generate the precipitation panel (B) ----
#.......................................................

for(i in 1:N){
  
  new_data <- try(datagrid(model = model,
                           park = PARKS[i],#levels(bcparks$park)[i],
                           month = 1:12,
                           avgprecip = seq(min(bcparks$avgprecip),
                                         max(bcparks$avgprecip),
                                         by = 0.1)))
  new_data$estimate <- try(predict(model, newdata = new_data, type = "response"))

  #Bind the results
  if(i == 1){res <- new_data}
  if(i>1 && class(new_data$estimate) == "numeric"){res <- rbind(res, new_data)}
  
}

# Merge and aggregate by region
res3 <- merge(res, regions, by.x = "park", by.y = "park", all.x = T)

# Average temp effect for each region
precip_preds <- aggregate(estimate ~ avgprecip + region, data = res3, FUN = "mean")

# Average precip effect for each park
precip_park_level <- aggregate(estimate ~ avgprecip + park, data = res3, FUN = "mean")

# Average, population level effect
precip_pop_level <- aggregate(estimate ~ avgprecip, data = res3, FUN = "mean")



### Plot attendance in response to precipitation ----
precip <-
  ggplot() +
  geom_point(data = bcparks[which(bcparks$attendancetype == "dayuse"),], 
             aes(y = attendance, x = avgprecip, col = region),
             alpha = 0.05, shape = 16) + # points for each observation
  geom_line(data = precip_preds,
            aes(x = avgprecip, estimate, col = region),
            alpha = 0.9, size = 0.7) + # line for each region
  geom_line(data = precip_pop_level,
            aes(x = avgprecip, estimate),
            alpha = 1, col = "black", size = 1.2) + # line for overall trend
  scale_y_continuous(limits = c(0,15), expand = c(0,0.1)) +
  scale_x_continuous(limits = c(0,30)) +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +  
  xlab("Average Monthly Precipitation (mm)") +
  ylab("Monthly Park Visitors (per 1000 people)") +
  ggtitle("B") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(hjust = 0.04, vjust = -6, size = 20, 
                                  family = "sans", face = "bold"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
precip
# ggsave(precip, filename = "figures/precip.png", device = NULL, path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)

# Combine the 2 panels
FIG2 <- grid.arrange(temp, precip, 
                     ncol=2, widths = c(6,6))

# Save the figure
ggsave(FIG2,
       width = 10, height = 5, units = "in",
       dpi = 600,
       bg = "white",
       file="figures/figure2.png")


# Figure 3 ----

library(dplyr)
library(ggplot2)
library(khroma)
library(ggh4x) # to fill in facet wrap title boxes

# Import the attendance projections for relevant pop growth scenarios
LGattendance = readRDS("RDS/LG-attendance-projections.rds")
HGattendance = readRDS("RDS/HG-attendance-projections.rds")

# Calculate change in attendance relative to first prediction
LGattendance <- LGattendance %>%
  group_by(ssp, year, park) %>% # don't compare different SSPs, years, or parks to each other
  mutate(annual_visitors = sum(predicted_visitors)) # get annual totals
LGattendance <- LGattendance %>%
  group_by(park, ssp) %>% # don't compare different SSPs or parks to each other
  mutate(relative_visitors = annual_visitors / first(annual_visitors)) 

# Calculate change in attendance relative to first prediction
HGattendance <- HGattendance %>%
  group_by(ssp, year, park) %>% # don't compare different SSPs, years, or parks to each other
  mutate(annual_visitors = sum(predicted_visitors)) # get annual totals
HGattendance <- HGattendance %>%
  group_by(park, ssp) %>% # don't compare different SSPs or parks to each other
  mutate(relative_visitors = annual_visitors / first(annual_visitors)) 

# make colour strips in x-direction for panel title boxes
strip <- strip_themed(background_x = 
                        elem_list_rect(fill = c("#4EBAF9", "#C0DEED", "#FFC1B5", "#FF7B7B")))

# assign a letter for each SSP/panel
data_text <- data.frame(label = c("A", "B", "C", "D"),
                        ssp = names(table(LGattendance$ssp)),
                        x = c(2023, 2023, 2023, 2023), 
                        y = c(2.1, 2.1, 2.1, 2.1))

# Compare all regions by relative growth in visitors (same values for 
# both LG and HG, so just pick one dataset)
projections <-
  ggplot(LGattendance, aes(x = year, y = relative_visitors)) + 
  geom_hline(yintercept = 1, linewidth = 0.5, color = "grey70") + # line at 1
  geom_line(size=0.5, aes(group = park, col = region), alpha = 0.1) + # line for each park
  facet_wrap2(~ ssp, strip = strip, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) + # create a panel for each climate change scenario
  geom_text(data = data_text,
            mapping = aes(x = x,
                          y = y,
                          label = label),
            check_overlap = TRUE,
            size = 8, fontface = "bold") +
  xlab("Year") +
  ylab("Relative Change in Annual Visitors") +
  # ggtitle("BC Parks Attendance Predictions") +
  scale_y_continuous(limits = c(0.75,2.2)) +
  guides(col = guide_legend(override.aes = list(alpha=1))) +
  scale_colour_muted(name="Region",
                     labels=c('Northern', 
                              'Kootenay-Okanagan', 
                              'South Coast', 
                              'Thompson-Cariboo', 
                              'West Coast')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        legend.position=c(0.4,0.86),
        legend.key.size = unit(0.4, 'cm'),
        legend.key = element_rect(color = NA),
        legend.title = element_text(size = 8, face = "bold"),
        legend.text=element_text(size=7),
        legend.background=element_blank(),
        #legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
projections

# save the figure
ggsave(projections,
       width = 8.21, height = 5.53, units = "in",
       dpi = 600,
       bg = "white",
       file="figures/figure3_oldmodel.png")

# Figure 4 ----

library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggh4x) # to fill in facet wrap title boxes

# Import the historical data
bcparks$year <- as.numeric(bcparks$year) # fix year format

# Disable scientific notation (for y-axis)
options(scipen = 999)

# Show seasonal trends separated by SSP
# make colour strips in x-direction for panel title boxes
strip <- strip_themed(background_x = 
                        elem_list_rect(fill = c("#4EBAF9", "#C0DEED", "#FFC1B5", "#FF7B7B")))
# Plot attendance under low population growth
LG <-
  ggplot() +
  facet_wrap2(~ ssp, strip = strip, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_line(data = LGattendance[which(LGattendance$park == "Golden Ears Park"),], 
            aes(month, predicted_visitors, group = year, col = year), 
            size=0.1) + # lines for each year
  geom_point(data = bcparks[which(bcparks$park == "Golden Ears Park"),] %>%
               group_by(month) %>%
               summarise(visitortotal = mean(visitortotal)), 
             aes(month, visitortotal), 
             col = "black", size = 1, na.rm = TRUE) + # plot historical monthly averages as points
  xlab("Month") +
  ylab("Monthly Visitors") +
  ggtitle("A") +
  labs(subtitle = "Golden Ears Park Attendance under Low Population Growth") +
  scale_y_continuous(labels = scales::comma, limits = c(0,500000)) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_colour_viridis_c(name = "Year") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans", angle = 45, hjust = 1),
        plot.subtitle = element_text(size = 10, family = "sans", face = "bold"),
        plot.title = element_text(size = 25, family = "sans", face = "bold"),
        legend.position=c(0.11,0.84),
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.6, 'cm'),
        legend.key.size = unit(0.25, 'cm'),
        legend.key = element_rect(color = NA),
        legend.title = element_text(size = 8, family = "sans", face = "bold"),
        legend.text=element_text(size=7),
        legend.margin=margin(c(8,8,8,8)),
        legend.background=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
HG <-
  ggplot() +
  facet_wrap2(~ ssp, strip = strip, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_line(data = HGattendance[which(HGattendance$park == "Golden Ears Park"),], 
            aes(month, predicted_visitors, group = year, col = year), 
            size=0.1) + # lines for each year
  geom_point(data = bcparks[which(bcparks$park == "Golden Ears Park"),] %>%
               group_by(month) %>%
               summarise(visitortotal = mean(visitortotal)), 
             aes(month, visitortotal), 
             col = "black", size = 1, na.rm = TRUE) + # plot historical monthly averages as points
  xlab("Month") +
  ylab("Monthly Visitors") +
  ggtitle("B") +
  labs(subtitle = "Golden Ears Park Attendance under High Population Growth") +
  scale_y_continuous(labels = scales::comma, limits = c(0,500000)) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  scale_colour_viridis_c(name = "Year") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x  = element_text(size=8, family = "sans", angle = 45, hjust = 1),
        plot.subtitle = element_text(size = 10, family = "sans", face = "bold"),
        plot.title = element_text(size = 25, family = "sans", face = "bold"),
        legend.position = "none",
        legend.title = element_text(size = 12, family = "sans"),
        legend.box.background = element_rect(color = "black"),
        legend.margin=margin(c(8,8,8,8)),
        legend.background=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))

# Combine the 2 panels
FIG4 <- grid.arrange(LG, HG, 
                     ncol=2, widths = c(6,5.2))

# Save the figure
ggsave(FIG4,
       width = 10, height = 6, units = "in",
       dpi = 600,
       bg = "white",
       file="figures/figure4.png")
