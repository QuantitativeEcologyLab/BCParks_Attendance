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
# combine all 4 panels
FIG1 <- grid.arrange(arrangeGrob(arrangeGrob(map, arrangeGrob(regionlegend, sizelegend, 
                                             ncol = 2), heights = c(10,1)), # left half 
                                 arrangeGrob(seasonal, august, december, 
                                             ncol = 1, heights = c(10,10,10)), # right half
                                 ncol=2, widths=c(2,1.3)),
                     nrow=2,heights=c(10, 1))

#save all plots
ggsave(FIG1,
       width = 8, height = 6, units = "in",
       dpi = 600,
       bg = "white",
       file="~/Desktop/bio/440/BCParks_Attendance/Figures/figure1.png")