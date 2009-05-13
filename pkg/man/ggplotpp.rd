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

	dsmall <- diamonds[sample(nrow(diamonds), 100), ]
	d <- ggplot(dsmall, aes(carat, price))
	str(dsmall)
	library(ggplotpp)


	d + geom_ngon(aes(fill = carat, sides=color), colour="orange",ar=1,  size=5, angle=pi/3)

	d + geom_ngon(aes(colour = carat, angle = x, ar=y),fill=NA,  sides=50)
	
	d + geom_ngon(aes(colour = carat, fill = x, size=y), linewidth=2,  sides=6)

	vplayout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

	drawAll <- function(a,b,c,d, e, f) {
	   grid.newpage()
	   pushViewport(viewport(layout=grid.layout(3,2) ) )
	       print(a, vp=vplayout(1,1))
	       print(b, vp=vplayout(1,2))
	       print(c, vp=vplayout(2,1))
	       print(d, vp=vplayout(2,2))
	       print(e, vp=vplayout(3,1))
	       print(f, vp=vplayout(3,2))
	}

	mdf <- data.frame(x <- seq(0, 10), y=rnorm(x), 
		f=factor(rep(letters[1:2], each=3, length=length(x))))

	a <- qplot(x, y, data=mdf, colour=f, geom=c("line", "point"), facets=f~.) +
	 theme_grey() +  opts(title="theme_grey")

	b <- a + theme_talk() + opts(title="theme_talk")
	c <- a + theme_minimal() + opts(title="theme_minimal")
	d <- a + theme_bw() + opts(title="theme_bw")
	e <- a + theme_flashy() + opts(title="theme_flashy")
	f <- a + theme_bb() + opts(title="theme_dark")

	drawAll(a,b,c,d, e, f)

}

