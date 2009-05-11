ScaleAngle <- proto(ScaleContinuous, expr={
	
  new <- function(., name=NULL, limits=NULL, breaks=NULL, labels=NULL, trans = NULL, to = c(0, 2*pi)) {
    .super$new(., name=name, limits=limits, breaks=breaks, labels=labels, trans=trans, variable = "angle", to = to)
  }

  map <- function(., values) {
    rescale(values, .$to, .$input_set())
  }
  output_breaks <- function(.) .$map(.$input_breaks())
  
  

  # Documentation -----------------------------------------------
  
  objname <- "angle"
  desc <- "angle of rotation of a ngon"
  icon <- function(.) {
    # 
  }
 
 
  examples <- function(.) {
  # 
  }
  
  
})


scale_angle <- ScaleAngle$build_accessor()