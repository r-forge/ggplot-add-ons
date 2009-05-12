theme_bw2 <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
    axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =        theme_segment(colour = "black", size = 0.2),
    axis.title.x =      theme_text(size = base_size, vjust = 1),
    axis.title.y =      theme_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),

    legend.background = theme_rect(colour=NA), 
    legend.key =        theme_rect(colour = "grey80"),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(size = base_size * 0.8),
    legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",

    panel.background =  theme_rect(fill = "white", colour = NA), 
    panel.border =      theme_rect(fill = NA, colour="grey50"), 
    panel.grid.major =  theme_line(colour = "grey90", size = 0.2),
    panel.grid.minor =  theme_line(colour = "grey98", size = 0.5),
    panel.margin =      unit(0.25, "lines"),

    strip.background =  theme_rect(fill = "grey80", colour = "grey50"), 
    strip.label =       function(variable, value) value, 
    strip.text.x =      theme_text(size = base_size * 0.8),
    strip.text.y =      theme_text(size = base_size * 0.8, angle = -90),

    plot.background =   theme_rect(colour = NA),
    plot.title =        theme_text(size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

theme_minimal <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
    axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =        theme_segment(colour = "black", size = 0.2),
    axis.title.x =      theme_text(size = base_size, vjust = 1),
    axis.title.y =      theme_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),

    legend.background = theme_rect(colour=NA), 
    legend.key =        theme_rect(colour = NA),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(size = base_size * 0.8),
    legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",

    panel.background =  theme_rect(fill = "white", colour = NA), 
    panel.border =      theme_rect(fill = NA, colour=NA), 
    panel.grid.major =  theme_line(colour = "grey90", size = 0.2),
    panel.grid.minor =  theme_line(colour = "grey98", size = 0.5),
    panel.margin =      unit(0.25, "lines"),

    strip.background =  theme_rect(fill = NA, colour = "grey90"), 
    strip.label =       function(variable, value) value, 
    strip.text.x =      theme_text(size = base_size * 0.8),
    strip.text.y =      theme_text(size = base_size * 0.8, angle = -90),

    plot.background =   theme_rect(colour = NA),
    plot.title =        theme_text(size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

theme_gray2 <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "grey50", vjust = 1),
    axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "grey50", hjust = 1),
    axis.ticks =        theme_segment(colour = "grey50"),
    axis.title.x =      theme_text(size = base_size, vjust = 0.5),
    axis.title.y =      theme_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),

    legend.background = theme_rect(colour="white"), 
    legend.key =        theme_rect(fill = "grey95", colour = "white"),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(size = base_size * 0.8),
    legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",

    panel.background =  theme_rect(fill = "grey90", colour = NA), 
    panel.border =      theme_blank(), 
    panel.grid.major =  theme_line(colour = "white"),
    panel.grid.minor =  theme_line(colour = "grey95", size = 0.25),
    panel.margin =      unit(0.25, "lines"),

    strip.background =  theme_rect(fill = "grey80", colour = NA), 
    strip.label =       function(variable, value) value, 
    strip.text.x =      theme_text(size = base_size * 0.8),
    strip.text.y =      theme_text(size = base_size * 0.8, angle = -90),

    plot.background =   theme_rect(colour = NA, fill = "white"),
    plot.title =        theme_text(size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

theme_talk <- function(base_size = 14) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "grey50", vjust = 1),
    axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "grey50", hjust = 1),
    axis.ticks =        theme_segment(colour = "grey50"),
    axis.title.x =      theme_text(size = base_size, vjust = 0.5),
    axis.title.y =      theme_text(size = base_size, angle = 0, vjust = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),

    legend.background = theme_rect(colour="white"), 
    legend.key =        theme_rect(fill = "grey95", colour = "white"),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(size = base_size * 0.8),
    legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",

    panel.background =  theme_rect(fill = "grey90", colour = NA), 
    panel.border =      theme_blank(), 
    panel.grid.major =  theme_line(colour = "white"),
    panel.grid.minor =  theme_line(colour = "grey95", size = 0.25),
    panel.margin =      unit(0.25, "lines"),

    strip.background =  theme_rect(fill = "grey80", colour = NA), 
    strip.label =       function(variable, value) value, 
    strip.text.x =      theme_text(size = base_size * 0.8),
    strip.text.y =      theme_text(size = base_size * 0.8, angle = -90),

    plot.background =   theme_rect(colour = NA, fill = "white"),
    plot.title =        theme_text(size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}
theme_flashy <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x = theme_text(colour = "#CCFF33"),
    axis.text.y = theme_text(colour = "#CCFF33", hjust = 1),
    axis.title.x = theme_text(colour = "#CCFF33", face = "bold"),
    axis.title.y = theme_text(colour = "#CCFF33", face = "bold", angle = 90) , 
    axis.ticks =        theme_segment(colour = "grey50"),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),

    legend.background = theme_rect(colour="white"), 
    legend.key =        theme_rect(fill = "grey95", colour = "white"),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(size = base_size * 0.8),
    legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",

    panel.background = theme_rect(fill = "#003DF5"),
    panel.border =      theme_blank(), 
    panel.grid.major =  theme_line(colour = "white"),
    panel.grid.minor =  theme_line(colour = "grey95", size = 0.25),
    panel.margin =      unit(0.25, "lines"),

    strip.background =  theme_rect(fill = "grey80", colour = NA), 
    strip.label =       function(variable, value) value, 
    strip.text.x =      theme_text(size = base_size * 0.8),
    strip.text.y =      theme_text(size = base_size * 0.8, angle = -90),

    plot.background = theme_rect(fill = "#3366FF"),
    plot.title =        theme_text(size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

# a+theme_flashy()

theme_dark <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "black", vjust = 1),
    axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1),
    axis.ticks =        theme_segment(colour = "black"),
    axis.title.x =      theme_text(size = base_size, vjust = 0.5),
    axis.title.y =      theme_text(size = base_size, angle = 0, vjust = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),

    legend.background = theme_rect(colour="black"), 
    legend.key =        theme_rect(fill = "black", colour = "black"),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(size = base_size * 0.8),
    legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",

    panel.background =  theme_blank(), 
    panel.border =      theme_blank(), 
    panel.grid.major =  theme_line(colour = "black"),
    panel.grid.minor =  theme_line(colour = "black", size = 0.25),
    panel.margin =      unit(0.25, "lines"),

    strip.background =  theme_rect(fill = "grey80", colour = NA), 
    strip.label =       function(variable, value) value, 
    strip.text.x =      theme_text(size = base_size * 0.8),
    strip.text.y =      theme_text(size = base_size * 0.8, angle = -90),

    plot.background =   theme_rect(colour = NA, fill = "grey80"),
    plot.title =        theme_text(size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}



theme_bb <- function(base_size = 14) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "white", vjust = 1),
    axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "white", hjust = 1),
    axis.ticks =        theme_segment(colour = "white"),
    axis.title.x =      theme_text(size = base_size, vjust = 0.5, colour = "white"),
    axis.title.y =      theme_text(size = base_size, angle = 0, vjust = 0.5, colour = "white"),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),

    legend.background = theme_rect(fill="NA", colour="NA"), 
    legend.key =        theme_rect(fill = "white", colour = "white"),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(size = base_size * 0.8, colour = "white"),
    legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0, colour = "white"),
    legend.position =   "right",

    panel.background =  theme_rect(fill = "grey40", colour = NA), 
    panel.border =      theme_blank(), 
    panel.grid.major =  theme_line(colour = "white"),
    panel.grid.minor =  theme_line(colour = "grey10", size = 0.1),
    panel.margin =      unit(0.25, "lines"),

    strip.background =  theme_rect(fill = "grey40", colour = "white"), 
    strip.label =       function(variable, value) value, 
    strip.text.x =      theme_text(size = base_size * 0.8, colour = "white"),
    strip.text.y =      theme_text(size = base_size * 0.8, angle = -90, colour = "white"),

    plot.background =   theme_rect(colour = NA, fill = "grey10"),
    plot.title =        theme_text(size = base_size * 1.2, colour = "white" ),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}




