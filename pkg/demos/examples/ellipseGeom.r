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
  guide_geom <- function(.) "polygon"
  
  examples <- function() {
	dsmall <- sample.df(diamonds)
	d <- ggplot(dsmall, aes(carat, price))
	d + geom_ngon(aes(fill=carat), sides=4, size=2)
  }
})



polygon.regular <- function (n=4) { 

if (n<3) n <- 3 
if (n > 8) n <- 50
th <- pi*2/n 
costh <- cos (th) 
sinth <- sin (th) 
xx <- yy <- rep (0,n+1) 
if (n %% 2) { 
	xx[1] <- 0 
	yy[1] <- 1 } else { 
	xx[1] <- -sin(th/2) 
	yy[1] <-  cos(th/2) 
} 

for (i in 2:(n+1)) { 
xl <- xx[i-1] 
yl <- yy[i-1] 
xx[i] <- xl*costh - yl*sinth 
yy[i] <- xl*sinth + yl*costh } 

matrix(c(xx,yy), ncol=2)
} 

# plot(-1:1, -1:1, t="n")
# el <- polygon.regular(0, 0, 3)
# lines(el[, 1], el[, 2])
# rotM(el, pi/2) -> el2
# lines(el2[, 1], el2[, 2], col=2)


# Draw ngon grob
#
# @arguments x positions of centres
# @arguments y positions
# @argument vector of sizes
# @argument border colour
# @argument fill colour
# @keywords internal

ngonGrob <- function(x, y, sides=5, size = rep(1, length(x)), 
						angle=rep(pi/2, length(x)), ar = rep(1.5, length(x)), 
						colour = "grey50", fill = "grey90") {
							
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
    default.units = "native",
    id.lengths = unlist(vertices), gp = gpar(col = colour, fill = fill)
  )
}


geom_ngon <- GeomNgon$build_accessor()


ScaleSides <- proto(ScaleDiscrete, expr={
  doc <- TRUE
  common <- NULL
  .input <- .output <- "sides"
 
  output_set <- function(.) c(3, 4, 5, 6, 7, 8, 50)[seq_along(.$input_set())]
  max_levels <- function(.) 9
  
  detail <- "<p></p>"
  
  # Documentation -----------------------------------------------
 
  objname <- "sides"
  desc <- "Scale for polygon sides"
  
  icon <- function(.) {
  # 
  }
  
  examples <- function() {
    #  
    # See scale_manual for more flexibility
  }
  
})


scale_sides <- ScaleSides$build_accessor()



dsmall <- diamonds[sample(nrow(diamonds), 100), ]
d <- ggplot(dsmall, aes(carat, price))
str(dsmall)
library(ggplotpp)


d + geom_ngon(aes(fill = carat, sides=color), colour="orange",ar=1,  size=5, angle=pi/3)

d + geom_ngon(aes(colour = carat, angle = x, ar=y),fill=NA,  sides=50)



# 
# 
# library(spectral)
# colorStrip <- 
# function (colors, draw = T) 
# {
#     x <- seq(0, 1 - 1/ncol(colors), length = ncol(colors))
#     y <- rep(0.5, length(colors))
#     my.grob <- grid.rect(x = unit(x, "npc"), y = unit(y, "npc"), 
#         width = unit(1/ncol(colors), "npc"), height = unit(1, 
#             "npc"), just = "left", hjust = NULL, vjust = NULL, 
#         default.units = "npc", name = NULL, gp = gpar(fill = rgb(colors[1, 
#             ], colors[2, ], colors[3, ]), col = rgb(colors[1, 
#             ], colors[2, ], colors[3, ])), draw = draw, vp = NULL)
#     my.grob
# }
# 
# 
# colors <- rbind(c(1, 0, 1), c(0, 1, 0), c(0, 0, 0))
# 
# pdf("testRGB.pdf")
# colorStrip(colors)
# dev.off()
# 
# 
# 
# PostScriptTrace("testRGB.pdf") 
# 
# test <- readLines("testRGB.pdf.xml") 
# 
# testRead <- readPicture("testRGB.pdf.xml") 
# 
# str(testRead)
# grid.picture(testRead) 
# 
# grep("<rgb.+", test, value=T)
# 




