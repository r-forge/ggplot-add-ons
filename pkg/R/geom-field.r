
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
    
	library(ggplotpp)
	xy <- expand.grid(x=-10:10, y=-10:10)
	d1 <- data.frame(x=xy$x, y=xy$y)
	d1$colour <- sample(1:5, length(xy$x), repl=T) 

	d1$angle <- 2*pi*rnorm(d1$x) 
	d1$length <- rnorm(d1$y) 


	p <- ggplot(data=d1, map=aes(x=x, y=y, angle=angle, length=length/10, colour=colour))

	p + geom_field()
	
  }
  
})

geom_field <- GeomField$build_accessor()


library(ggplotpp)
xy <- expand.grid(x=-10:10, y=-10:10)
d1 <- data.frame(x=xy$x, y=xy$y)
d1$colour <- sample(1:5, length(xy$x), repl=T) # check that other aesth. are handled well

d1$angle <- 2*pi*rnorm(d1$x) 
d1$length <- rnorm(d1$y) 


p <- ggplot(data=d1, map=aes(x=x, y=y, angle=angle, length=length/10, colour=colour))

p + geom_field() 



field2segment <- function(d=NULL){
	transform(d, 
		x=x - 0.5*length*cos(angle), y=y - 0.5*length*sin(angle), 
		# x and y shifted from the middle to the extremity of the segment
		xend=x+0.5*length*cos(angle), yend=y+0.5*length*sin(angle) )
}

xy <- expand.grid(x=-10:10, y=-10:10)
d1 <- data.frame(x=xy$x, y=xy$y)
d1$colour <- sample(1:5, length(xy$x), repl=T) # check that other aesth. are handled well

d1$angle <- 2*pi*rnorm(d1$x) 
d1$length <- rnorm(d1$y) 

d2 <- field2segment(d1)

p <- ggplot(map=aes(x=x, y=y, colour=colour))

p + geom_segment(data=d1, map=aes(xend=xend, yend=yend)) # fails as xend and yend are missing
p + geom_segment(data=d2, map=aes(xend=xend, yend=yend))+
 geom_point(data=d1) 


?seq

seq.weave <- function(froms, by, length,  ...){
	c(
		matrix(c(sapply(froms, seq, by=by, length = length/2,  ...)), 
		nrow=length(froms), byrow=T)
		)
}
seq.weave(c(2, 3), by=3, length=8)
seq.weave(c(2, 3, 4), by=2, length=8)


# [1]  2  3  5  6  8  9 11 12



