star <- function(n=4, adj=0, r=1){

	    if (n == 1) {
	        return(rbind(c(0, 0), c(cos(adj), sin(adj)) * r))
	    }
	    if (n == 2) {
	        return(rbind(c(cos(adj), sin(adj)), c(cos(adj + pi), 
	            sin(adj + pi))) * r)
	    }
	    if (n == 3) {
	        return(rbind(c(0, 0), c(cos(adj), sin(adj)), NA, c(0, 
	            0), c(cos(adj + 2 * pi/3), sin(adj + 2 * pi/3)), 
	            NA, c(0, 0), c(cos(adj + 4 * pi/3), sin(adj + 4 * 
	                pi/3))) * r)
	    }
	    if (n == 4) {
	        return(rbind(c(cos(adj), sin(adj)), c(cos(adj + pi), 
	            sin(adj + pi)), NA, c(cos(adj + pi/2), sin(adj + 
	            pi/2)), c(cos(adj + 3 * pi/2), sin(adj + 3 * pi/2))) * 
	            r)
	    }
	    if (n == 6) {
	        tmp <- c(0, 2 * pi/3, 4 * pi/3, 2 * pi)
	        tmp <- c(tmp, NA, tmp + pi/3) + adj
	        return(cbind(cos(tmp), sin(tmp)) * r)
	    }
	    skp <- floor(n/2 - 0.1)
	    tmp <- seq(0, skp * 2 * pi, length.out = n + 1) + adj
	    tmp2 <- cbind(cos(tmp), sin(tmp)) * r
	    while (any(duplicated(round(tmp2[-1, ], 5)))) {
	        skp <- skp - 1
	        tmp <- seq(0, skp * 2 * pi, length.out = n + 1) + adj
	        tmp2 <- cbind(cos(tmp), sin(tmp)) * r
	    }
	    return(tmp2)

}

# 
# polygon.star(3)
# polygon.star(5)
# polygon.star(50)
