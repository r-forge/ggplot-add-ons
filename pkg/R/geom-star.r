GeomStar <- proto(Geom, {
  objname <- "star"
  desc <- "Regular starred polygons"
 
  draw <- function(., data, scales, coordinates, ...) {
    with(coordinates$transform(data, scales),
      ggname(.$my_name(), 
	starGrob(x, y, sides, size, angle, col=alpha(colour, alpha)))
    )
  }


  required_aes <- c("x", "y")
  default_aes <- function(.) 
	aes(sides=5, size=1, angle=0, colour="black", alpha = 1)
  default_stat <- function(.) StatIdentity
   guide_geom <- function(.) "star"
  
  # 

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data,
	{
		if(angle != 0 && sides == 50 )
		grob.angle <- segmentsGrob(0.5, 0.5, 0.5 + cos(angle)/2, 0.5 + sin(angle)/2, 
			gp=gpar(colour="grey50", linewidth=1))
		else grob.angle <- NULL
      ggname(.$my_name(),gTree(children = gList(grob.angle, 
              starGrob(0.5, 0.5,
                      size=size,
                      sides=sides,
                      angle = angle , colour = colour, 
                      units.def="npc"))))
	}
		)
  }

  icon <- function(.) {
	starGrob(c(1/4, 1/2, 3/4), c(1/4, 1/2, 3/4), 	
		sides=c(5, 6, 50),
		size=c(10, 3, 5), 
		angle = c(0, pi/4, pi/3) , 
		col=c("#E41A1C",  "#377EB8",  "#4DAF4A"))
  }

  examples <- function() {# 
library(ggplotpp)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
str(dsmall)
d <- ggplot(dsmall, aes(carat, price))+theme_minimal()

d + geom_star(aes(colour = carat, angle = x), size=2,  sides=5)

d1 <- d + geom_star(aes(size=x, sides=cut))
d1 + geom_point(aes(size=x), col="white")

d + geom_star(aes(col = carat, sides=color), size=2, angle=pi/3)

  }
})



starGrob <- function(x, y, sides=5, size = 1, angle=pi/2, 
						colour = "grey50",  units.def="npc") {
							
  stopifnot(length(y) == length(x))
  
n <- length(x)
size <- size #/ 2 # polygon.regular has radius unity

if(length(size)  < n ) size  <- rep(size,  length.out=n) 
if(length(sides) < n ) sides <- rep(sides, length.out=n) 
if(length(angle) < n ) angle <- rep(angle, length.out=n) 

# create a list of n polygons of identical size and orientation

starC <- llply(sides, star)


#  rotate the stars

starC.list <- 
llply(seq_along(starC), function(ii) 
		size[ii] * starC[[ii]] %*% 
		matrix(c(cos(angle[ii]), -sin(angle[ii]), sin(angle[ii]), cos(angle[ii])), nrow = 2)
		)

vertices <- laply(starC.list, nrow)
reps.x <- do.call(c, llply(seq_along(x), function(ii) rep(x[ii], vertices[ii])))
reps.y <- do.call(c, llply(seq_along(y), function(ii) rep(y[ii], vertices[ii])))

starXY <- do.call(rbind, starC.list)

 polygonGrob(
    x = unit(starXY[, 1], "mm") + unit(reps.x, units.def),
    y = unit(starXY[, 2], "mm") + unit(reps.y, units.def),
    default.units = units.def,
    id.lengths = unlist(vertices), gp = gpar(col = colour, fill = colour)
  )
}

# now lives in zzz.r# # 
# geom_star <- GeomStar$build_accessor()
# 

# grid.draw(
# icon()
# )

 
# 
# dsmall <- diamonds[sample(nrow(diamonds), 100), ]
# str(dsmall)
# d <- ggplot(dsmall, aes(carat, price))
# 
# d + geom_ngon(aes(fill=color, sides=cut, size=x))
# 
# 
