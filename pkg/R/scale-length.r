ScaleLength <- proto(ScaleContinuous, expr={
  doc <- TRUE
  common <- NULL
  
  new <- function(., name=NULL, limits=NULL, breaks=NULL, labels=NULL, trans = NULL, to = NULL) {
    .super$new(., name=name, limits=limits, breaks=breaks, labels=labels, trans=trans, variable = "length", to = to)
  }
  
  map <- function(., values) {
    # rescale(values, .$to, .$input_set())
values
  }
  output_breaks <- function(.) .$map(.$input_breaks())
  
  objname <- "length"
  desc <- "Length scale for continuous variable"
  
  icon <- function(.) {
    x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    rectGrob(x, width=0.25,
      gp=gpar(fill=alpha("black", x), col=NA)
    )
    
  }
  
  examples <- function(.) {

	library(ggplotpp)
	dsmall <- diamonds[sample(nrow(diamonds), 100), ]
	str(dsmall)
	d <- ggplot(dsmall, aes(carat, price))+theme_minimal()

	d + geom_ngon(aes(ar=price), size=2,  sides=50) # int
	d + geom_ngon(aes(ar=table), size=2,  sides=50) # num
	d + geom_ngon(aes(ar=cut), size=2,  sides=50) # factor
	
	

  }
})
 

ScaleLengthDiscrete <- proto(ScaleDiscrete, expr={
  common <- NULL
  objname <- "length_discrete"
  .input <- .output <- "length"
  desc <- "Length scale for discrete variables"
  doc <- FALSE

  max_levels <- function(.) 11
  output_set <- function(.) seq(1, 5, length=11)

})

# scale_length <- ScaleLength$build_accessor()
# scale_ar_discrete <- ScaleArDiscrete$build_accessor()