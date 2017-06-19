# adapted from https://rpubs.com/Koundy/71792
theme_eupath <- function(base_size=14, base_family="Helvetica", legend.position="bottom", legend.direction = "horizontal") {
  # library(ggthemes)
  # theme_foundation(base_size=base_size, base_family=base_family)+
    theme(
            text = element_text(size=base_size, family=base_family),
            panel.background = element_rect(fill = "white", colour = "white"),
            # plot.background = element_rect(colour = "white"),
            # panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold", size = rel(1.1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(size=rel(0.9), face="bold"),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA, fill="white"),
            legend.position = legend.position,
            legend.direction = legend.direction,
            legend.title = element_text(face="italic"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
            # plot.margin = unit(c(10,0,0,0), "cm")
    )
}

# 
# scale_fill_Publication <- function(...){
#   library(scales)
#   discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
#   
# }
# 
# scale_colour_Publication <- function(...){
#   library(scales)
#   discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
#   
# }