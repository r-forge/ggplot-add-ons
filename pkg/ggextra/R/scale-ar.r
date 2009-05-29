ScaleAr <- proto(ScaleContinuous, expr={
  doc <- TRUE
  common <- NULL
  
  new <- function(., name=NULL, limits=NULL, breaks=NULL, labels=NULL, trans = NULL, to = c(1, 5)) {
    .super$new(., name=name, limits=limits, breaks=breaks, labels=labels, trans=trans, variable = "ar", to = to)
  }
  
  map <- function(., values) {
    rescale(values, .$to, .$input_set())
  }
  output_breaks <- function(.) .$map(.$input_breaks())
  
  objname <- "ar"
  desc <- "Aspect ratio scale for continuous variable"
  
  icon <- function(.) {
    x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    rectGrob(x, width=0.25,
      gp=gpar(fill=alpha("black", x), col=NA)
    )
    
  }
  
  examples <- function(.) {

	library(ggextra)
	dsmall <- diamonds[sample(nrow(diamonds), 100), ]
	str(dsmall)
	d <- ggplot(dsmall, aes(carat, price))+theme_minimal()

	d + geom_ngon(aes(ar=price), size=2,  sides=50) # int
	d + geom_ngon(aes(ar=table), size=2,  sides=50) # num
	d + geom_ngon(aes(ar=cut), size=2,  sides=50) # factor
	
	

  }
})
 

ScaleArDiscrete <- proto(ScaleDiscrete, expr={
  common <- NULL
  objname <- "ar_discrete"
  .input <- .output <- "ar"
  desc <- "AR scale for discrete variables"
  doc <- FALSE

  max_levels <- function(.) 11
  output_set <- function(.) seq(1, 5, length=11)

})

# scale_ar <- ScaleAr$build_accessor()
# scale_ar_discrete <- ScaleArDiscrete$build_accessor()