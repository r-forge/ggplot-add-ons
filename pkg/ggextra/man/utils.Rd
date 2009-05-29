\name{utils}
\Rdversion{1.1}
\alias{arrange}
\alias{area}
\alias{star}
\alias{polygon.regular}
\alias{vp.layout}

\title{
Convenient utilities for plotting
}
\description{
	Wrapper functions for grid and ggplot2
}
\usage{
arrange(..., nrow = NULL, ncol = NULL, as.table = F)
area(n=5, R=1)
star(n = 4, adj = 0, r = 1) 
polygon.regular(sides = 4, star = FALSE) 
vp.layout(x, y) 
}

\arguments{
}
\details{
	see examples
}
\value{
	
}
\references{
	
}
\author{baptiste Auguie (most things originally borrowed from ggplot2, r-help, etc.)
}
\note{
	
}

\seealso{
	
}
\examples{
	
sapply(c(3:12, 50, 500), area)
polygon.regular(5)
star(5)

	d <- diamonds[sample(nrow(diamonds), 100), ]
	p <- ggplot(d, aes(carat, price)) 
	str(d)

	p1 <-
	p + geom_ngon()+opts(title="plot1")
	p2 <- 
	p + geom_point()+opts(title="plot2")
	p3 <- 
	p + geom_ellipse(ar=2)+opts(title="plot3")
	p4 <- 
	p + geom_ellipse(ar=2, angle=pi/4)+opts(title="plot4")
	p5 <- 
	p + geom_ellipse(ar=2, colour="red")+opts(title="plot5")

\dontrun{
	arrange(p1, p2, p3, p4, p5)

	arrange(p1, p2, p3, p4, p5, as.table=T)
}

}

