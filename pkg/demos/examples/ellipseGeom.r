

ScaleSidesManual <- proto(ScaleManual, expr={
  doc <- TRUE
  common <- NULL
  .input <- .output <- "sides"
 
  output_set <- function(.) c(3, 4, 5, 6, 7, 8, 50)[seq_along(.$input_set())]
  max_levels <- function(.) 9
  
  detail <- "<p></p>"
  
  # Documentation -----------------------------------------------
 
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


scale_sides <- ScaleSidesManual$build_accessor()



dsmall <- diamonds[sample(nrow(diamonds), 100), ]
d <- ggplot(dsmall, aes(carat, price))
str(dsmall)
library(ggplotpp)


d + geom_ngon(aes(fill = carat, sides=color), colour="orange",ar=1,  size=5, angle=pi/3)

d + geom_ngon(aes(colour = carat, angle = x), fill=NA,  sides=5)


d <- ggplot(dsmall, aes(carat, price)) 
d + stat_binhex(aes( fill=color,  ), geom="ngon", sides=5) 

# 

ScaleManual <- proto(ScaleDiscrete, {
  doc <- TRUE
  common <- c("colour","fill","size","shape","linetype")
  values <- c()
  
  new <- function(., name=NULL, values=NULL, variable="x", limits = NULL, breaks = NULL, labels = NULL, formatter = identity) {
    .$proto(name=name, values=values, .input=variable, .output=variable, limits = limits, breaks = breaks, .labels = labels, formatter = formatter)
  }
 
  map <- function(., values) {
    .$check_domain()
 
    values <- as.character(values)
    values[is.na(values)] <- "NA"
    input <- .$input_set()
    input[is.na(input)] <- "NA"
    
    if (.$has_names()) {
      values[!values %in% input] <- NA
      .$output_set()[values]
    } else {
      
      .$output_set()[match(values, input)]
    }
  }
 
  has_names <- function(.) !is.null(names(.$output_set()))
 
  input_breaks <- function(.) nulldefault(.$breaks, .$input_set())
  output_breaks <- function(.) .$map(.$input_breaks())
 
  output_set <- function(.) .$values
  labels <- function(.) {
    .$.labels %||% .$input_breaks()
  }
 
  # Documentation -----------------------------------------------
 
  objname <- "manual"
  desc <- "Create your own discrete scale"
  icon <- function(.) textGrob("DIY", gp=gpar(cex=1.2))
  
  examples <- function(.) {
    p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl))
 
    p + scale_colour_manual(values = c("red","blue", "green"))
    p + scale_colour_manual(
      values = c("8" = "red","4" = "blue","6" = "green"))
    
    # As with other scales you can use breaks to control the appearance
    # of the legend
    cols <- c("8" = "red","4" = "blue","6" = "darkgreen", "10" = "orange")
    p + scale_colour_manual(values = cols)
    p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"))
    p + scale_colour_manual(values = cols, breaks = c("8", "6", "4"))
    p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"),
      labels = c("four", "six", "eight"))
    
    # And limits to control the possible values of the scale
    p + scale_colour_manual(values = cols, limits = c("4", "8"))
    p + scale_colour_manual(values = cols, limits = c("4", "6", "8", "10"))
    
  }
  
})



