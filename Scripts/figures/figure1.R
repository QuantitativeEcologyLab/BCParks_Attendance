library(gridExtra)

# source in map of parks 
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/figures/map-bcparks.R")
# source in historical trend plots 
source("~/Desktop/bio/440/BCParks_Attendance/Scripts/figures/historical-trends.R")
# make function to grab legend from figures
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
# save region legend from map
regionlegend <- get_legend(map)
# save size legend from temp vs precip plot
sizelegend <- get_legend(plot)
# remove legends from map plot
map <- map + theme(legend.position="none")
august <- august + theme(legend.position="none")
december <- december + theme(legend.position="none")
seasonal <- seasonal + theme(legend.position="none")

# combine all 4 panels
fig <- grid.arrange(arrangeGrob(map, seasonal,
                                ncol = 1,
                                heights = c(8,6)), # left half
                    arrangeGrob(august, december, sizelegend,
                                ncol = 1,
                                heights = c(8,8,3)), # right half
                    ncol=2, widths = c(8, 6))
FIG1 <- grid.arrange(regionlegend, fig,
                     nrow = 2, heights = c(1,20))

#save all plots
ggsave(FIG1,
       width = 10, height = 8, units = "in",
       dpi = 600,
       bg = "white",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/figure1.png")
