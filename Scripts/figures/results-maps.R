library('dplyr')        # for data wrangling
library('sf')           # for working with spatial data
library('sp')           # for working with spatial data
library('canadianmaps') # to download a shapefile of BC
library('ggplot2')      # for fancy plots
theme_set(theme_map())

# import a shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

# Import the attendance projections for relevant pop growth scenarios
LGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/LG-attendance-projections.rds")
HGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/HG-attendance-projections.rds")

# Make datasets of only predictions for 2100
LG2100 <- LGattendance[which(LGattendance$year == "2100"),]
HG2100 <- HGattendance[which(HGattendance$year == "2100"),]

# Take average of 2100 predictions
LG2100 <- LG2100 %>%
  group_by(ssp, park) %>% # don't compare different SSPs or parks to each other
  mutate(avg_rel_visitors = mean(relative_visitors))
HG2100 <- HG2100 %>%
  group_by(ssp, park) %>% # don't compare different SSPs or parks to each other
  mutate(avg_rel_visitors = mean(relative_visitors))

# Remove outlier park (?)
LG2100 <- LG2100[-which(LG2100$park == "Mount Assiniboine Park"),]
HG2100 <- HG2100[-which(HG2100$park == "Mount Assiniboine Park"),]

# Make maps of change in visitors under LG population scenario
LGmap <-
  ggplot() +
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_sf(data = bc_shp) +
  geom_point(aes(longitude, latitude, col = region, size = avg_rel_visitors), 
             LG2100) +
  ggtitle("Visitors in 2100 under Low Population Growth") +
  scale_color_manual(name="Region", 
                     values=alpha(c('ok'="#C5ABE7",
                                    'north'="#47007A",
                                    'tc'="#85D4FF",
                                    'west'="#34A02C",
                                    'south'="#1f78b4"), 0.03)) +
  labs(size = "Change in Visitors\nRelative to 2020") +
  guides(col = FALSE, size = guide_legend(override.aes = list(alpha=0.4))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans", face = "bold"),
        legend.box.background = element_rect(color = "black"),
        legend.margin=margin(c(8,8,8,8)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
# Make maps of change in visitors under HG population scenario
HGmap <-
ggplot() +
  facet_wrap(~ ssp, labeller = as_labeller(c(
    `1-2.6` = "SSP 1-2.6",
    `2-4.5` = "SSP 2-4.5",
    `3-7.0` = "SSP 3-7.0",
    `5-8.5` = "SSP 5-8.5"))) +
  geom_sf(data = bc_shp) +
  geom_point(aes(longitude, latitude, col = region, size = avg_rel_visitors), 
             HG2100) +
  ggtitle("Visitors in 2100 under High Population Growth") +
  scale_color_manual(name="Region", 
                     values=alpha(c('ok'="#C5ABE7",
                                    'north'="#47007A",
                                    'tc'="#85D4FF",
                                    'west'="#34A02C",
                                    'south'="#1f78b4"), 0.03)) +
  labs(size = "Change in Visitors\nRelative to 2020") +
  guides(col = FALSE, size = guide_legend(override.aes = list(alpha=0.4))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = -0.05, size = 15, family = "sans", face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans", face = "bold"),
        legend.box.background = element_rect(color = "black"),
        legend.margin=margin(c(8,8,8,8)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


# save the figures
ggsave(LGmap,
       width = 8.21, height = 5.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/projections/LG-map.png")
ggsave(HGmap,
       width = 8.21, height = 5.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/projections/HG-map.png")
