library(dplyr)
library(ggplot2)
library(khroma)
library(ggh4x) # to fill in facet wrap title boxes

# Import the attendance projections for relevant pop growth scenarios
LGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/LG-attendance-projections.rds")
HGattendance = readRDS("~/Desktop/bio/440/BCParks_Attendance/Data/projections/HG-attendance-projections.rds")

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
                        elem_list_rect(fill = c("#0571b0", "#92c5de", "#f4a582", "#ca0020")))

# assign a letter for each SSP/panel
data_text <- data.frame(label = c("A", "B", "C", "D"),
                        ssp = names(table(LGattendance$ssp)),
                        x = c(2023, 2023, 2023, 2023), 
                        y = c(1.9, 1.9, 1.9, 1.9))

# Compare all regions by relative growth in visitors (same values for 
# both LG and HG, so just pick one dataset)
projections <-
  ggplot(LGattendance, aes(x = year, y = relative_visitors)) + 
  geom_hline(yintercept = 1, size = 0.5, color = "grey70") + # line at 1
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
  scale_y_continuous(limits = c(0.75,2)) +
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
        axis.title.y = element_text(size=14, family = "sans", face = "bold"),
        axis.title.x = element_text(size=14, family = "sans", face = "bold"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans"),
        plot.title = element_text(size = 14, family = "sans", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position=c(0.4,0.86),
        legend.key.size = unit(0.4, 'cm'),
        legend.title = element_text(size = 8, face = "bold"),
        legend.text=element_text(size=7),
        legend.box.background = element_rect(color = "black"),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))
projections

# save the figure
ggsave(projections,
       width = 8.21, height = 5.53, units = "in",
       dpi = 600,
       bg = "transparent",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/figure2.png")
