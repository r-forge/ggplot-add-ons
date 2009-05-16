area <- function(n=5, R=1){
	
	Area = n/2 * R^2 * sin(2*pi / n)
 return(Area)
}

# sapply(c(3:12, 50, 500), area)
