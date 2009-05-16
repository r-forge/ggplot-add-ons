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
    library(ggplotpp)
	dsmall <- diamonds[sample(nrow(diamonds), 100), ]
	str(dsmall)
	d <- ggplot(dsmall, aes(carat, price)) + theme_minimal()

	d + geom_ngon(aes(colour = carat, sides=cut), size=2)
	d + geom_ngon(aes(colour = carat, sides=depth), size=2) # error
	d + geom_ngon(aes(colour = carat, sides=cut(price, 3)), size=2) # OK

	
  }
  
})


# # 
# ScaleSidesContinuous <- proto(ScaleContinuous, expr={
#   common <- NULL
#   objname <- "sides_continuous"
#   .input <- .output <- "sides"
#   desc <- "Sides scale for continuous variables"
#   doc <- FALSE
# 
# 
# new <- function(., name=NULL, limits=NULL, breaks=NULL, labels=NULL, trans = NULL, to = c(3:8, 50)) {
# .super$new(., name=name, limits=limits, breaks=breaks, labels=labels, trans=trans, variable = "sides", to = to)
# }
# 
# map <- function(., values) {
# rescale(cut(values, 7), .$to, .$input_set())
# }
# output_breaks <- function(.) .$map(.$input_breaks())
# 
# objname <- "sides"
# desc <- "Sides scale for continuous variable"
# 
# })

# scale_sides <- ScaleSides$build_accessor()

