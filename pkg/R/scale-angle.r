	ScaleAngle <- proto(ScaleContinuous, expr={
  doc <- TRUE
  common <- NULL
  
  new <- function(., name=NULL, limits=NULL, breaks=NULL, labels=NULL, trans = NULL, to = c(0, pi)) {
    .super$new(., name=name, limits=limits, breaks=breaks, labels=labels, trans=trans, variable = "angle", to = to)
  }
  
  map <- function(., values) {
    rescale(values, .$to, .$input_set())
  }
  output_breaks <- function(.) .$map(.$input_breaks())
  
  objname <- "angle"
  desc <- "Alpha scale for continuous variable"
  
  icon <- function(.) {
    x <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    rectGrob(x, width=0.25,
      gp=gpar(fill=alpha("black", x), col=NA)
    )
    
  }
  
  examples <- function(.) {
    (p <- qplot(mpg, cyl, data=mtcars, alpha=cyl))
    p + scale_alpha("cylinders")
    p + scale_alpha("number\nof\ncylinders")
    
    p + scale_alpha(to = c(0.4, 0.8))
  }
})
# 
# scale_angle <- ScaleAngle$build_accessor()
# scale_angle <- ScaleAngle$build_accessor()


ScaleAngleDiscrete <- proto(ScaleDiscrete, expr={
  common <- NULL
  objname <- "angle_discrete"
  .input <- .output <- "angle"
  desc <- "Angle scale for discrete variables"
  doc <- FALSE

  max_levels <- function(.) 7
  output_set <- function(.) seq(0, pi, by=pi/6)

})