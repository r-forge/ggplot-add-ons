ScaleSides <- proto(ScaleDiscrete, expr={
  doc <- TRUE
  common <- NULL
  .input <- .output <- "sides"
 
  output_set <- function(.) c(3, 4, 5, 6, 7, 8, 50)[seq_along(.$input_set())]
  max_levels <- function(.) 7
  
  # Documentation -----------------------------------------------
  detail <- "<p></p>"
  
 
  objname <- "sides"
  desc <- "Scale for polygon sides"
  
  icon <- function(.) {
  # 
  }
  
  examples <- function() {
    #  
    # See scale_manual for more flexibility
  }
  
})

# scale_sides <- ScaleSides$build_accessor()

