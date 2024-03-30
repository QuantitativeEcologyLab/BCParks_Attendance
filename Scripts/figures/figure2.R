# Note: this script relies on previously fitted data and model objects.
# It can take several hours to run 

library(mgcv)
library(marginaleffects)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(khroma)

# Import historical attendance
bcparks <- readRDS("Data/Attendance/Park Data/bcparks.rds")
regions <- aggregate(region ~ park, data = bcparks, FUN = "unique")
# Import the model
model <- readRDS("Scripts/Attendance/models/attendance_model.RDS")



# Number of parks to loop over
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

#.......................................................
## Multi-panel ----
#.......................................................

# Combine the 2 panels
FIG2 <- grid.arrange(temp, precip, 
                     ncol=2, widths = c(6,6))

# Save the figure
ggsave(FIG2,
       width = 10, height = 5, units = "in",
       dpi = 600,
       bg = "white",
       file="Figures/figure2.png")