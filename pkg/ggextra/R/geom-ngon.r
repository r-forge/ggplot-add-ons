GeomNgon <- proto(Geom, {
  objname <- "ngon"
  desc <- "Regular polygons"
 
  draw <- function(., data, scales, coordinates, ...) {
    with(coordinates$munch(data, scales),
      ggname(.$my_name(), 
	ngonGrob(x, y, sides, size, angle, ar, col=alpha(colour, alpha), fill = alpha(fill, alpha)))
    )
  }


  required_aes <- c("x", "y")
  default_aes <- function(.) 
	aes(sides=5, size=1, angle=0, ar=1, colour="grey50", fill = NA, alpha = 1)
  default_stat <- function(.) StatIdentity
   guide_geom <- function(.) "ngon"
  
  # 

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data,
	{
		if(angle != 0 && is.na(fill) && ar == 1)
		grob.angle <- segmentsGrob(0.5, 0.5, 0.5 + cos(angle)/2, 0.5 + sin(angle)/2, 
			gp=gpar(colour="grey50", linewidth=1))
		else grob.angle <- NULL
      ggname(.$my_name(),gTree(children = gList(grob.angle, 
              ngonGrob(0.5, 0.5,
                      ar=ar,
                      size=size,
                      sides=sides,
                      angle = angle ,
                      fill=fill, units.def="npc"))))
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
	library(ggextra)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
str(dsmall)
d <- ggplot(dsmall, aes(carat, price))+theme_minimal()

d + geom_ngon(aes(colour = carat, angle = x, ar=y, fill=carat),  sides=50)

d + geom_ngon(aes(fill = depth, sides=cut, size=color),ar=1)


library(ggextra)
ggplot(data.frame(x=0, y=0), mapping = aes(x, y ))+ 
geom_point(size=100, col="red", pch=1)+
geom_point(size=100, col="red", pch=0)+
geom_point(size=100, col="red", pch=3)+
geom_ngon(size=100, fill="yellow", alpha=0.2, sides=50, col="green")+
geom_ngon(size=100, fill="yellow", alpha=0.2, sides=3, col="blue")+
geom_ngon(size=100, fill="yellow", alpha=0.2, sides=4, col="blue")+
geom_ngon(size=100, fill="yellow", alpha=0.2, sides=5, col="blue")+
geom_ngon(size=100, fill="yellow", alpha=0.2, sides=6, col="blue")+
geom_ngon(size=100, fill="yellow", alpha=0.2, sides=7, col="blue")+
geom_ngon(size=100, fill="yellow", alpha=0.2, sides=8, col="blue")

  }
})



ngonGrob <- function(x, y, sides=5, size = 1, 
						angle=rep(pi/2, length(x)), ar = rep(1, length(x)), 
						colour = "grey50", fill = "grey90", units.def="native") {
							
  stopifnot(length(y) == length(x))
  
n <- length(x)
  

if(length(size)  < n ) size  <- rep(size,  length.out=n) 
# if(length(star)  < n ) star  <- rep(star,  length.out=n) 
if(length(sides) < n ) sides <- rep(sides, length.out=n) 

# create a list of n polygons of identical size and orientation
# possible optimization here when sides and star are constant

# general case
# sides <- c(3:8, 50)
# ngonC <- mlply(cbind(sides=sides, star=star), polygon.regular)

ngonC <- llply(sides, polygon.regular)
# polygon.regular has area unity


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
