
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
