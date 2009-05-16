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


# 
ScaleSidesContinuous <- proto(ScaleContinuous, expr={
  common <- NULL
  objname <- "sides_continuous"
  .input <- .output <- "sides"
  desc <- "Sides scale for continuous variables"
  doc <- FALSE


new <- function(., name=NULL, limits=NULL, breaks=NULL, labels=NULL, trans = NULL, to = c(3:8, 50)) {
.super$new(., name=name, limits=limits, breaks=breaks, labels=labels, trans=trans, variable = "sides", to = to)
}

map <- function(., values) {
rescale(values, .$to, .$input_set())
}
output_breaks <- function(.) .$map(.$input_breaks())

objname <- "sides"
desc <- "Sides scale for continuous variable"

})

# scale_sides <- ScaleSides$build_accessor()

