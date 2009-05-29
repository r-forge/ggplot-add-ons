colorStrip <- # draw a palette or return a strip of colors
function (colors=1:3, draw = T) 
{
    x <- seq(0, 1 - 1/length(colors), length = length(colors))
    y <- rep(0.5, length(colors))
    my.grob <- grid.rect(x = unit(x, "npc"), y = unit(y, "npc"), 
        width = unit(1/length(colors), "npc"), 
		height = unit(1, "npc"), 
		just = "left", hjust = NULL, vjust = NULL, 
        default.units = "npc", name = NULL, 
		gp = gpar(fill = colors, col = colors, draw = draw, vp = NULL))
		
    my.grob
}

# colorStrip()
# colorStrip(brewer.pal(3, "Set1"))