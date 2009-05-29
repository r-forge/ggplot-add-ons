
polygon.regular <- function (sides=4, scale.area=TRUE, star=FALSE) { 

n <- sides # lazy to replace
if (n<3) n <- 3 
if (n > 8) n <- 50
# if (n > 8 & star) n <- 8

if(!star){ # convex polygon
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

xy <- matrix(c(xx,yy), ncol=2)
 if(scale.area) {
	Area = n/2 * 1^2 * sin(2*pi / n)
	xy <- xy / sqrt(Area)
 }

} else { # starred version
xy <- polygon.star(n=sides)
if(scale.area) {
	# unimplemented
}

}
return(xy)
} 

