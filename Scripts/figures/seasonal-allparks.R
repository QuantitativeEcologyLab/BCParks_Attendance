library(ggplot2)
library(gridExtra)
library(ggh4x) # to fill in facet wrap title boxes
library(dplyr) # for data wrangling

# Import the historical data
bcparks = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/bcparks/bcparks.rds")
bcparks$year <- as.numeric(bcparks$year) # fix year format
bcparks <- bcparks[!bcparks$attendancetype == "camping",] # remove historical camping data (not relevant)

# Import the attendance projections for relevant pop growth scenarios
LGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/LG-attendance-projections.rds")
HGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/HG-attendance-projections.rds")

# Remove NA values
LGattendance <- na.omit(LGattendance)
HGattendance <- na.omit(HGattendance)
bcparks <- na.omit(bcparks)

# Disable scientific notation (for y-axis)
options(scipen = 999)

# Show seasonal trends separated by SSP
# make colour strips in x-direction for panel title boxes
strip <- strip_themed(background_x = 
                        elem_list_rect(fill = c("#0571b0", "#92c5de", "#f4a582", "#ca0020")))

# check that all park names are the same
length(intersect(unique(LGattendance$park), unique(HGattendance$park))) ==
  length(unique(LGattendance$park))
length(intersect(unique(bcparks$park), unique(HGattendance$park))) ==
  length(unique(bcparks$park))

for(PARK in unique(bcparks$park)[1:249]) { # repeat for all 249 parks with data
  MAX_VISITORS <- filter(HGattendance, park == PARK) %>%
    pull(predicted_visitors) %>%
    max()
  # round to the closest number above (1 sf)
  POW <- floor(log10(MAX_VISITORS))
  MAX_VISITORS <- ceiling(MAX_VISITORS / 10^POW) * 10^POW
  
  LG <-
    ggplot() +
    facet_wrap2(~ ssp, strip = strip, labeller = as_labeller(c(
      `1-2.6` = "SSP 1-2.6",
      `2-4.5` = "SSP 2-4.5",
      `3-7.0` = "SSP 3-7.0",
      `5-8.5` = "SSP 5-8.5"))) +
    geom_line(data = LGattendance[which(LGattendance$park == PARK),], 
              aes(month, predicted_visitors, group = year, col = year), 
              size=0.1) + # lines for each year
    geom_point(data = bcparks[which(bcparks$park == PARK),] %>%
                 group_by(month) %>%
                 summarise(visitortotal = mean(visitortotal)), 
               aes(month, visitortotal), 
               col = "black", size = 1, na.rm = TRUE) + # plot historical monthly averages as points
    xlab("Month") +
    ylab("Visitors") +
    ggtitle("A") +
    labs(subtitle = paste(PARK, "Attendance under Low Population Growth")) +
    scale_y_continuous(labels = scales::comma, limits = c(0,MAX_VISITORS)) +
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
          legend.title = element_text(size = 8, family = "sans", face = "bold"),
          legend.text=element_text(size=7),
          legend.box.background = element_rect(color = "black"),
          legend.margin=margin(c(8,8,8,8)),
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
    geom_line(data = HGattendance[which(HGattendance$park == PARK),], 
              aes(month, predicted_visitors, group = year, col = year), 
              size=0.1) + # lines for each year
    geom_point(data = bcparks[which(bcparks$park == PARK),] %>%
                 group_by(month) %>%
                 summarise(visitortotal = mean(visitortotal)), 
               aes(month, visitortotal), 
               col = "black", size = 1, na.rm = TRUE) + # plot historical monthly averages as points
    xlab("Month") +
    ylab("Visitors") +
    ggtitle("B") +
    labs(subtitle = paste(PARK, "Attendance under High Population Growth")) +
    scale_y_continuous(labels = scales::comma, limits = c(0,MAX_VISITORS)) +
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
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
  
  # Combine the 2 panels
  FIG3 <- grid.arrange(LG, HG, 
                       ncol=2, widths = c(6,5.2))
  
  # remove forward slashes in the name
  PARK <- stringr::str_replace_all(PARK, pattern = '/', replacement = ' ')
  
  # Save the figure
  ggsave(FIG3,
         width = 10, height = 6, units = "in",
         dpi = 600,
         bg = "white",
         file=paste0("~/Desktop/bio/440/BCParks_Attendance/Figures/seasonal projections/",
                     PARK, ".png"))
}

