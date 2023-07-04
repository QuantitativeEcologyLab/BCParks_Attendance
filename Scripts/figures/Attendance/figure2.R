# Note: this script relies on previously fitted data and model objects.
# It can take several hours to run 

library(mgcv)
library(marginaleffects)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(khroma)

#Load in the data and model
bcparks <- readRDS("~/Downloads/bcparks.rds")
regions <- aggregate(region ~ park, data = bcparks, FUN = "unique")
model <- readRDS("~/Downloads/model.rds") #note: this needed to be manually changed from rda to rds

# Number of parks to loop over
PARKS <- levels(bcparks$park)
N <- length(PARKS)

#-------------------------------------------------------
# Generate the temperature panel (A)
#-------------------------------------------------------

#Loop over the number of parks
for(i in 1:N){
  preds <- try(predictions(model,
                       datagrid(park = levels(bcparks$park)[i],
                                avgtemp = seq(min(bcparks$avgtemp),
                                              max(bcparks$avgtemp),
                                              by = 0.1))))
  
  #Bind the results
  if(i == 1){res <- preds}
  if(i>1 && class(preds)[1] == "predictions"){res <- rbind(res, preds)}
  
  #Clear the intermediate variable
  rm(preds)
}

#Save predictions
save(res, file = "~/Dropbox/UBC/Students/Dayna_Weststrate/Manuscript/temp_preds.Rda")
load("~/Dropbox/UBC/Students/Dayna_Weststrate/Manuscript/temp_preds.Rda")

# Merge and aggregate by region
res2 <- merge(res, regions, by.x = "park", by.y = "park", all.x = T)
temp_preds <- aggregate(estimate ~ avgtemp + region, data = res2, FUN = "mean")

# Average, population level effect
temp_pop_level <- aggregate(estimate ~ avgtemp, data = res2, FUN = "mean")


# Plot attendance in response to temperature
temp <-
  ggplot() +
  geom_point(data = bcparks[which(bcparks$attendancetype == "dayuse"),], 
             aes(y = attendance, x = avgtemp, col = region),
             alpha = 0.05, shape = 16) + # points for each observation
  # geom_line(data = temp_pop_level,
  #           aes(x = avgtemp, estimate),
  #           alpha = 1, col = "black", size = 1.2) + # line for overall trend
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
        axis.title.y = element_text(size=12, family = "sans", face = "bold"),
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = 0.04, vjust = -5, size = 20, 
                                  family = "sans", face = "bold"),
        # plot.subtitle = element_text(size = 14, family = "sans", face = "bold"),
        legend.position=c(0.21,0.66),
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



#-------------------------------------------------------
# Generate the precipitation panel (B)
#-------------------------------------------------------

#Loop over the number of parks
for(i in 1:N){
  
  preds <- try(predictions(model,
                           datagrid(park = levels(bcparks$park)[i],
                                    avgprecip = seq(min(bcparks$avgprecip),
                                                  max(bcparks$avgprecip),
                                                  by = 0.1))))
  
  #Bind the results
  if(i == 1){res <- preds}
  if(i>1 && class(preds)[1] == "predictions"){res <- rbind(res, preds)}
  
  #Clear the intermediate variable
  rm(preds)
}

#Save predictions
save(res, file = "~/Dropbox/UBC/Students/Dayna_Weststrate/Manuscript/precip_preds.Rda")

# Merge and aggregate by region
res3 <- merge(res, regions, by.x = "park", by.y = "park", all.x = T)
precip_preds <- aggregate(estimate ~ avgprecip + region, data = res3, FUN = "mean")

# Average, population level effect
precip_pop_level <- aggregate(estimate ~ avgprecip, data = res3, FUN = "mean")



# Plot attendance in response to precipitation
precip <-
  ggplot() +
  geom_point(data = bcparks[which(bcparks$attendancetype == "dayuse"),], 
             aes(y = attendance, x = avgprecip, col = region),
             alpha = 0.05, shape = 16) + # points for each observation
  geom_line(data = precip_preds,
            aes(x = avgprecip, estimate, col = region),
            alpha = 0.9, size = 0.7) + # line for each region
  # geom_line(data = precip_pop_level,
  #           aes(x = avgprecip, estimate),
  #           alpha = 1, col = "black", size = 1.2) + # line for overall trend
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
        axis.title.x = element_text(size=12, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        plot.title = element_text(hjust = 0.04, vjust = -5, size = 20, 
                                  family = "sans", face = "bold"),
        legend.position = "none",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


# Combine the 2 panels
FIG2 <- grid.arrange(temp, precip, 
                     ncol=2, widths = c(6,5.2))

# Save the figure
ggsave(FIG2,
       width = 10, height = 5, units = "in",
       dpi = 600,
       bg = "white",
       file="~/Dropbox/UBC/Students/Dayna_Weststrate/Manuscript/figure2.png")
