
fieldGrob <- function(x, y, angle, length, size, 
	colour="black", linetype=1, arrow=NULL){
		 segmentsGrob(
		x0=x - 0.5*length*cos(angle), 
		y0=y - 0.5*length*sin(angle),
		x1=x+0.5*length*cos(angle), 
		y1=y+0.5*length*sin(angle), 
		default.units="native",
	    gp = gpar(col=colour, lwd=size*ggplot2:::.pt, lty=linetype, lineend = "butt"), 
	        arrow = arrow)
}


# fieldGrob(0.5, 0.5, pi/6, 0.2,1,  col="blue")->g
# fieldGrob(0.5, 0.5, pi/2, 0.5,2,  col="red")->g2
# pushViewport(vp=viewport(width=1, height=1))
# grid.draw(g)
# grid.draw(g2)

GeomField <- proto(Geom, {
  draw <- function(., data, scales, coordinates, arrow=NULL, ...) {
    if (!coordinates$muncher()) {
      return(with(coordinates$transform(data, scales), 
        fieldGrob(x, y, angle, length, size, 
        col=colour, linetype, arrow)
      ))
    }
    
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data, ggname(.$my_name(),
		fieldGrob(0.5, 0.5, angle, length, size,  col=colour, linetype) )
	)
  }
 
  objname <- "field"
  desc <- "Single line segments"
  icon <- function(.) 
segmentsGrob(c(0.1, 0.3, 0.5, 0.7), c(0.3, 0.5, 0.1, 0.9), 
	c(0.2, 0.5, 0.7, 0.9), c(0.8, 0.7, 0.4, 0.3))

  desc_params <- list(
    arrow = "specification for arrow heads, as created by arrow()"
  )

  seealso <- list(
    geom_path = GeomPath$desc,
    geom_segment = GeomPath$desc,
    geom_line = GeomLine$desc
  )

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "angle", "length")
  default_aes <- function(.) aes(colour="black", angle=pi/4, length=1, size=0.5, linetype=1)
  guide_geom <- function(.) "field"
  
  examples <- function(.) {
    
	library(ggextra)
	xy <- expand.grid(x=-10:10, y=-10:10)
	d1 <- data.frame(x=xy$x, y=xy$y)
	d1$colour <- sample(1:5, length(xy$x), repl=T) 
	
	d1$angle <- 2*pi*rnorm(d1$x) 
	d1$whatever <- rnorm(d1$x) 
	d1$length <- abs(rnorm(d1$y))/10 
	
	
	p <- ggplot(data=d1, map=aes(x=x, y=y, angle=angle, length=length, colour=colour))
	
	p2 <- 
	p + geom_field() + geom_point()
	p + geom_field(aes(size=whatever)) 
	p + geom_field(arrow=arrow(angle = 30, length = unit(2, "mm"),
	      ends = "both", type = "open"))
	
	
  }
  
})
# 
# geom_field <- GeomField$build_accessor()
# 

# ls(package:ggextra)
# # themes
#  "theme_dark"	"theme_flashy"	"theme_gray2"	"theme_minimal"	"theme_talk"	"theme_bb"	"theme_bw2"      
# # utility functions
#  "arrange"	"vp.layout"	"polygon.regular"	"star"   
#  "chooseCol"	"colorStrip"	"createCol"          
# # geoms    
#  "geom_ellipse"	"geom_field"	"geom_fielduv"	"geom_ngon"	"geom_star"        
# # scales                    
#  "scale_abscissa"      "scale_alpha_manual" "scale_angle"     "scale_angle_discrete"   "scale_angle_manual"    
#  "scale_ar"            "scale_ar_discrete"  "scale_ar_manual" "scale_colour_dichromat" "scale_fill_dichromat"   "scale_length"          
#  "scale_length_manual" "scale_ordinate"     "scale_sides"     "scale_sides_manual"       
        
