GeomNgon <- proto(Geom, {
  objname <- "ngon"
  desc <- "Regular polygons"
 
  draw <- function(., data, scales, coordinates, ...) {
    with(coordinates$transform(data, scales),
      ggname(.$my_name(), 
	ngonGrob(x, y, sides, size, angle, ar, col=alpha(colour, alpha), fill = alpha(fill, alpha)))
    )
  }


  required_aes <- c("x", "y")
  default_aes <- function(.) 
	aes(sides=5, size=1, angle=0, ar=1, colour=NA, fill = "grey50", alpha = 1)
  default_stat <- function(.) StatIdentity
   guide_geom <- function(.) "ngon"
  
  # 

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data,
	{
		if(angle != 0 && sides == 50 && ar == 1)
		grob.angle <- segmentsGrob(0.5, 0.5, 0.5 + cos(angle)/2, 0.5 + sin(angle)/2, 
			gp=gpar(colour="grey", linewidth=1))
		else grob.angle <- NULL
      ggname(.$my_name(),gTree(children = gList(
              ngonGrob(0.5, 0.5,
                      ar=ar,
                      size=1.5*size,
                      sides=sides,
                      angle = angle ,
                      fill=fill, units.def="npc"), grob.angle)))
	}
		)
  }

  icon <- function(.) {
	ngonGrob(c(1/4, 1/2, 3/4), c(1/4, 1/2, 3/4), 
		ar=c(1, 1.5, 2), 
		size=c(10, 3, 5), 
		sides=c(5, 6, 50),
		angle = c(0, pi/4, pi/3) , 
		fill=c("#E41A1C",  "#377EB8",  "#4DAF4A"))
  }

  examples <- function() {# 
	library(ggplotpp)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
str(dsmall)
d <- ggplot(dsmall, aes(carat, price))+theme_minimal()

d + geom_ngon(aes(colour = carat, angle = x, ar=y, fill=carat), size=2,  sides=50)

d + geom_ngon(aes(fill=color, sides=cut, size=x))

d + geom_ngon(aes(fill = carat, sides=color), colour="orange",ar=1,  size=5, angle=pi/3)

  }
})



ngonGrob <- function(x, y, sides=5, size = 1, 
						angle=rep(pi/2, length(x)), ar = rep(1.5, length(x)), 
						colour = "grey50", fill = "grey90", units.def="npc") {
							
  stopifnot(length(y) == length(x))
  
n <- length(x)
size <- size / 2 # polygon.regular has radius unity

if(length(size)  < n ) size  <- rep(size,  length.out=n) 
# if(length(star)  < n ) star  <- rep(star,  length.out=n) 
if(length(sides) < n ) sides <- rep(sides, length.out=n) 

# create a list of n polygons of identical size and orientation

# possible optimization here when sides and star are constant
# if(length(sides) == 1 ) {
# 	ngonC <- polygon.regular(sides) #x, y, d=1, n=4
# 	ngonC <- llply(seq_along(x), function(ii) ngonC)
# 	 } else {
	# sides <- rep(sides, length.out=length(x))
	
# general case

# ngonC <- mlply(cbind(sides=sides, star=star), polygon.regular)
ngonC <- llply(sides, polygon.regular)


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
# geom_ngon <- GeomNgon$build_accessor()
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
