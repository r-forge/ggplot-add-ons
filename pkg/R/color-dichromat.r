require(dichromat)
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

dichromat.types <- list(
    div= c("BrowntoBlue", "BluetoDarkOrange", 
    "DarkRedtoBlue", "BluetoGreen", "BluetoGray", "BluetoOrangeRed", 
    "BluetoOrange", "GreentoMagenta"), 
    seq= c("LightBluetoDarkBlue"), 
    cat= c("Categorical"), 
    step= c("SteppedSequential")
    )


createCol <- function(n=3, name="BrowntoBlue.10"){
	pal <- dichromat::colorschemes[[name]]
	if (n == 1) return(pal[1])
	pal[floor(seq(1, length(pal), length=n))]
}
# createCol()

# colorStrip(createCol())

chooseCol <- function(n=1, breaks = c(10, 12), 
						palettes=c("BrowntoBlue.10", "BrowntoBlue.12")){
	
	breaks <- c(1, breaks)
	f <- cut(seq(1, max(breaks)), breaks = breaks)
	
	createCol(n, palettes[f[n]])
}

# chooseCol(n=3)
# chooseCol(n=11)

dichromat.pal <- function(n=3, name="BrowntoBlue"){
	switch(name, 
		BrowntoBlue = chooseCol(n, breaks = c(10, 12), 
			palettes=c("BrowntoBlue.10", "BrowntoBlue.12")), 	
		BluetoDarkOrange = chooseCol(n, breaks = c(12, 18), 
			palettes=c("BluetoDarkOrange.12", "BluetoDarkOrange.18")), 	
		DarkRedtoBlue = chooseCol(n, breaks = c(12, 18), 
			palettes=c("DarkRedtoBlue.12", "DarkRedtoBlue.18")), 	
		BluetoGreen = chooseCol(n, breaks = c(14), 
			palettes=c("BluetoGreen.14")), 	
		BluetoGray = chooseCol(n, breaks = c(8), 
			palettes=c("BluetoGray.8")), 	
		BluetoOrangeRed = chooseCol(n, breaks = c(14), 
			palettes=c("BluetoOrangeRed.14")), 	
		BluetoOrange = chooseCol(n, breaks = c(8, 10, 12), 
			palettes=c("BluetoOrange.8", "BluetoOrange.10", "BluetoOrange.12")), 	
		GreentoMagenta = chooseCol(n, breaks = c(16), 
			palettes=c("GreentoMagenta.16")), 
		LightBluetoDarkBlue = chooseCol(n, breaks = c(7, 10), 
			palettes=c("LightBluetoDarkBlue.7", "LightBluetoDarkBlue.10")), 	
		Categorical = chooseCol(n, breaks = c(12), 
			palettes=c("Categorical.12")), 	
		SteppedSequential = chooseCol(n, breaks = c(5), 
			palettes=c("SteppedSequential.5"))
		)
}


# dichromat.pal()
# colorStrip(dichromat.pal())
dichromat.names <- do.call(c, dichromat.types)
dichromat.maxcolors <- c(12, 18, 18, 14, 8, 14, 12, 16, 10, 12, 5)
	

ScaleDichromat <- proto(ScaleColour, expr={
  doc <- TRUE

  new <- function(., name=NULL, palette=1, type="div", alpha=1, limits=NULL, breaks = NULL, labels=NULL, formatter = identity, variable) {
    .$proto(name=name, palette=palette, type=type, .input=variable, .output=variable, .alpha=alpha, .labels = labels, breaks = breaks, limits= limits, formatter = formatter)
  }

  output_set <- function(.) {
    n <- length(.$input_set())
    pal <- dichromat.pal(n, .$pal_name())[1:n]
    alpha(pal, .$.alpha)
  }

  pal_name <- function(.) {
    if (is.character(.$palette)) {
      if (!.$palette %in% dichromat.names) {
        warning("Unknown palette ", .$palette)
        .$palette <- "BluetoOrangeRed"
      }
      return(.$palette)
    }
    
    switch(.$type, 
      div = dichromat.types$div, 
      seq = dichromat.types$seq, 
      cat = dichromat.types$cat, 
      step = dichromat.types$step
    )[.$palette]
  }
  
  max_levels <- function(.) { # unimplemented
    dichromat.maxcolors[dichromat.names == .$pal_name()]
  }

  # Documentation -----------------------------------------------

  objname <- "dichromat"
  desc <- "colour scales from the dichromat package"
  details <- "<p>See <a href='http://www.amstat-online.org/sections/graphics/newsletter/Volumes/v172.pdf'>http://www.amstat-online.org/sections/graphics/newsletter/Volumes/v172.pdf</a> for more info</p>"
  common <- c("colour", "fill")

  icon <- function(.) {
    rectGrob(c(0.1, 0.3, 0.5, 0.7, 0.9), width=0.21, 
      gp=gpar(fill=dichromat.pal(5, "BluetoOrangeRed"), col=NA)
    )
  }
  
  examples <- function(.) {
	 dsamp <- diamonds[sample(nrow(diamonds), 1000), ] 
	str(dsamp)
	d <- qplot(carat, price, data=dsamp, colour=cut)
	
	d
	
	d + scale_colour_brewer() 
	d + scale_colour_dichromat(type="div") 
	d + scale_colour_dichromat(type="div", palette=2)
	d + scale_colour_dichromat(type="div", palette=3)
	
	d + scale_colour_dichromat(type="seq")  
	d + scale_colour_dichromat(type="step") 
	d + scale_colour_dichromat(type="cat") 
	


  }
})


