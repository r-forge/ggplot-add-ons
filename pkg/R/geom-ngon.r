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
      ggname(.$my_name(),
              ngonGrob(0.5, 0.5,
                      ar=ar,
                      size=size*0.1,
                      sides=sides,
                      angle = angle ,
                      fill=fill, units.def="npc")))
  }

  icon <- function(.) {
	ngonGrob(c(1/4, 1/2, 3/4), c(1/4, 1/2, 3/4), 
		ar=c(1, 1.5, 2), 
		size=1.2*c(0.1, 0.3, 0.1), 
		sides=c(5, 6, 50),
		angle = c(0, pi/4, pi/3) , 
		fill=c("#E41A1C",  "#377EB8",  "#4DAF4A"))
  }

  examples <- function() {# 
	library(ggplotpp)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
str(dsmall)
d <- ggplot(dsmall, aes(carat, price))
d + geom_ngon(aes(fill=color, sides=cut, size=))
  }
})



ngonGrob <- function(x, y, sides=5, size = rep(1, length(x)), 
						angle=rep(pi/2, length(x)), ar = rep(1.5, length(x)), 
						colour = "grey50", fill = "grey90", units.def="native") {
							
  stopifnot(length(y) == length(x))
  
res <- max(resolution(x, FALSE), resolution(y, FALSE))
  # dx <- res/diff(range(x))
  # dy <- res/diff(range(y))
  
  n <- length(x)

# create a list of n polygons of identical size and orientation
if(length(sides) == 1 ) {
	ngonC <- polygon.regular(sides) #x, y, d=1, n=4
	ngonC <- llply(seq_along(x), function(ii) ngonC)
	 } else {
	sides <- rep(sides, length.out=length(x))
	ngonC <- llply(sides, polygon.regular)
}

# stretch the polygons, then rotate them
# aspect ratio factor for constant area
ngonC.list <- 
llply(seq_along(ngonC), function(ii) 
		size[ii]* res * ngonC[[ii]] %*% matrix(c(sqrt(ar[ii]), 0, 0, 1/sqrt(ar[ii])), ncol=2) %*%
		matrix(c(cos(angle[ii]), -sin(angle[ii]), sin(angle[ii]), cos(angle[ii])), nrow = 2)
		)

vertices <- laply(ngonC.list, nrow)
reps.x <- do.call(c, llply(seq_along(x), function(ii) rep(x[ii], vertices[ii])))
reps.y <- do.call(c, llply(seq_along(y), function(ii) rep(y[ii], vertices[ii])))

ngonXY <- do.call(rbind, ngonC.list)

 polygonGrob(
    x = ngonXY[, 1] + reps.x,
    y = ngonXY[, 2] + reps.y,
    default.units = units.def,
    id.lengths = unlist(vertices), gp = gpar(col = colour, fill = fill)
  )
}

# now lives in zzz.r# 
# geom_ngon <- GeomNgon$build_accessor()
# 