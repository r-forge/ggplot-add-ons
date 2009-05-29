
fielduvGrob <- function(x, y, abscissa, ordinate, size, 
	colour="black", linetype=1, arrow=NULL, center=FALSE){
		if(center) {
			return(
		 segmentsGrob(
		x0=x - 0.5*abscissa, 
		y0=y - 0.5*ordinate,
		x1=x + 0.5*abscissa, 
		y1=y + 0.5*ordinate, 
		default.units="native",
	    gp = gpar(col=colour, lwd=size*ggplot2:::.pt, lty=linetype, lineend = "butt"), 
	        arrow = arrow))} else {
				return(
			 segmentsGrob(
			x0=x , 
			y0=y ,
			x1=x + abscissa, 
			y1=y + ordinate, 
			default.units="native",
		    gp = gpar(col=colour, lwd=size*ggplot2:::.pt, lty=linetype, lineend = "butt"), 
		        arrow = arrow))}
	
}

# 
# fielduvGrob(0.5, 0.5, 0.1, 0.2, 1,  col="blue")->g
# fielduvGrob(0.5, 0.5, 0.1, 0.05,  1, col="red")->g2
# pushViewport(vp=viewport(width=1, height=1))
# grid.draw(g)
# grid.draw(g2)

GeomFielduv <- proto(Geom, {
  draw <- function(., data, scales, coordinates, arrow=NULL, center=FALSE, ...) {
    if (!coordinates$muncher()) {
      return(with(coordinates$transform(data, scales), 
        fielduvGrob(x, y, abscissa, ordinate, size, 
        col=colour, linetype, arrow, center)
      ))
    }
    
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data, ggname(.$my_name(),
		fielduvGrob(x=0.5, y=0.5, abscissa, ordinate, size,  col=colour, linetype, center=TRUE) )
	)
  }
 
  objname <- "fielduv"
  desc <- "Single line segments parametrised by u and v"
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
  required_aes <- c("x", "y", "abscissa", "ordinate")
  default_aes <- function(.) aes(abscissa=0.5, ordinate=0.5, size=1,colour="black",  linetype=1)
  guide_geom <- function(.) "fielduv"
  
  examples <- function(.) {
    
	library(ggextra)
	xy <- expand.grid(x=-10:10, y=-10:10)
	d1 <- data.frame(x=xy$x, y=xy$y)

	d1$u <- rnorm(d1$x) / 10
	d1$whatever <- rnorm(d1$x) 
	d1$v <- abs(rnorm(d1$y))/10
	
	
	p <- ggplot(data=d1, map=aes(x=x, y=y, abscissa=u, ordinate=v))
	
	p + geom_fielduv(aes(colour=whatever)) + geom_point()
	
  }
  
})
# 
# geom_fielduv <- GeomFielduv$build_accessor()
# 