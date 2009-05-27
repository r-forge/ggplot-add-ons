\name{ggplotpp}
\alias{ggplotpp}
\title{Themes and functions for ggplot2}
\author{baptiste Auguie}

\description{

}
\usage{}
\arguments{
}

\details{

}

\examples{


	library(ggplotpp)

# themes
	mdf <- data.frame(x <- seq(0, 10), y=rnorm(x), 
		f=factor(rep(letters[1:2], each=3, length=length(x))))

	a <- qplot(x, y, data=mdf, colour=f, geom=c("line", "point"), facets=f~.) +
	 theme_grey() +  opts(title="theme_grey")

	b <- a + theme_talk() + opts(title="theme_talk")
	c <- a + theme_minimal() + opts(title="theme_minimal")
	d <- a + theme_bw() + opts(title="theme_bw")
	e <- a + theme_flashy() + opts(title="theme_flashy")
	f <- a + theme_bb() + opts(title="theme_dark")

	arrange(a,b,c,d, e, f)
	
# geoms and scales
	
	d <- diamonds[sample(nrow(diamonds), 100), ]
	p <- ggplot(d, aes(carat, price, colour=cut)) 
	str(d)

	p1 <-
	p + geom_ngon(map=aes(sides=cut, fill=cut), size=2)+opts(title="plot1")
	p2 <- 
	p + geom_point()+ geom_field(map=aes(length=carat/10, angle=cut))+opts(title="plot2")
	p3 <- 
	p + geom_ellipse(map=aes(ar=depth, angle=table, fill=cut))+opts(title="plot3")
	p4 <- 
	p + geom_star(map=aes(edges=cut, size=carat))+opts(title="plot5")+
	scale_colour_dichromat(type="div", palette=8)

	arrange(p1, p2, p3, p4)

}

