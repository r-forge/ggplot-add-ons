\name{themes}
\alias{theme_flashy}
\alias{theme_talk}
\alias{theme_minimal}
\title{Themes for ggplot}
\author{Hadley Wickham <h.wickham@gmail.com>, modified by baptiste Auguie}

\description{
Produce a theme with white background and black gridlines.
}
\usage{theme_bw(base_size = 12)}
\arguments{
\item{base_size}{}
}

\details{Apart from the original theme_bw() and theme_grey(), the themes don't have carefully chosen aesthetic values, and are only intended as placeholders for customization.}

\examples{

	mdf <- data.frame(x <- seq(0, 10), y=rnorm(x), 
		f=factor(rep(letters[1:2], each=3, length=length(x))))

	a <- qplot(x, y, data=mdf, colour=f, geom=c("line", "point"), facets=f~.) +
	 theme_grey() +  opts(title="theme_grey")

	b <- a + theme_talk() + opts(title="theme_talk")
	c <- a + theme_minimal() + opts(title="theme_minimal")
	d <- a + theme_bw() + opts(title="theme_bw")
	e <- a + theme_flashy() + opts(title="theme_flashy")
	f <- a + theme_bb() + opts(title="theme_dark")
	
\dontrun{
	arrange(a,b,c,d,e,f)
}
}

