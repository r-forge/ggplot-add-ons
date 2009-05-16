GeomEllipse <- proto(Geom, {
  objname <- "ellipse"
  desc <- "Ellipses"
 
  draw <- function(., data, scales, coordinates, ...) {
    with(coordinates$munch(data, scales),
      ggname(.$my_name(), 
	ellipseGrob(x, y, size, angle, ar, col=alpha(colour, alpha), fill = alpha(fill, alpha)))
    )
  }


  required_aes <- c("x", "y")
  default_aes <- function(.) 
	aes(size=1, angle=0, ar=1, colour="black", fill = "grey50", alpha = 1)
  default_stat <- function(.) StatIdentity
   guide_geom <- function(.) "ellipse"
  
  # 

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data,
	{
		if(angle != 0 && ar == 1)
		grob.angle <- segmentsGrob(0.5, 0.5, 0.5 + cos(angle)/2, 0.5 + sin(angle)/2, 
			gp=gpar(colour="grey50", linewidth=1))
		else grob.angle <- NULL
      ggname(.$my_name(),gTree(children = gList( grob.angle, 
              ellipseGrob(0.5, 0.5,
                      ar=ar,
                      size=size,
                      angle = angle ,
                      fill=fill, units.def="npc"))))
	}
		)
  }

  icon <- function(.) {
	ellipseGrob(c(1/4, 1/2, 3/4), c(1/4, 1/2, 3/4), 
		ar=c(1, 1.5, 2), 
		size=c(10, 3, 5), 
		angle = c(0, pi/4, pi/3) , 
		fill=c("#E41A1C",  "#377EB8",  "#4DAF4A"))
  }

  examples <- function() {# 
library(ggplotpp)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
str(dsmall)
d <- ggplot(dsmall, aes(carat, price)) + theme_minimal()

d + geom_ellipse(aes(colour = carat, angle = cut, ar=y, fill=carat), size=2)

d + geom_ellipse(aes(fill = carat, angle = price, ar=cut), color=NA,  size=2)

  }
})



ellipseGrob <- function(x, y, size = 1, 
						angle=rep(pi/2, length(x)), ar = rep(1, length(x)), 
						colour = "grey50", fill = "grey90", units.def="native") {
							
  stopifnot(length(y) == length(x))
  
n <- length(x)
size <- size / 2 # polygon.regular has radius unity

if(length(size)  < n ) size  <- rep(size,  length.out=n) 

ngonC <- polygon.regular(50) # seems enough for a smooth curve
ngonC <- llply(seq_along(x), function(ii) ngonC)

# stretch the polygons, then rotate them
# aspect ratio factor for constant area
ngonC.list <- 
llply(seq_along(ngonC), function(ii) 
		size[ii] * ngonC[[ii]] %*% matrix(c(sqrt(ar[ii]), 0, 0, 1/sqrt(ar[ii])), ncol=2) %*%
		matrix(c(cos(angle[ii]), -sin(angle[ii]), sin(angle[ii]), cos(angle[ii])), nrow = 2)
		)

vertices <- laply(ngonC.list, nrow)
reps.x <- do.call(c, llply(seq_along(x), function(ii) rep(x[ii], vertices[ii])))
reps.y <- do.call(c, llply(seq_along(y), function(ii) rep(y[ii], vertices[ii])))

ngonXY <- do.call(rbind, ngonC.list)

 polygonGrob(
    x = unit(ngonXY[, 1], "mm") + unit(reps.x, units.def),
    y = unit(ngonXY[, 2], "mm") + unit(reps.y, units.def),
    default.units = units.def,
    id.lengths = unlist(vertices), gp = gpar(col = colour, fill = fill)
  )
}

# now lives in zzz.r# # 
# 
# 
# 
