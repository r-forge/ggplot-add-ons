GeomCustom <- proto(Geom, {
  objname <- "custom"
  desc <- "Custom grob"
 
 draw <- function(., data, scales, coordinates, customGrob = ngonGrob, ...) {
  with(coordinates$transform(data, scales),
    ggname(.$my_name(),
	customGrob(x, y, col=alpha(colour, alpha), ...))
  )
}

  required_aes <- c("x", "y")
  default_aes <- function(.) 
	aes(customGrob=ngonGrob, colour="black", alpha = 1)
  default_stat <- function(.) StatIdentity
   guide_geom <- function(.) "custom"
  
  # 

	  draw_legend <- function(., data, ...) {
	    data <- aesdefaults(data, .$default_aes(), list(...))
	
	    with(data,
	{
	      ggname(.$my_name(),gTree(children = gList(
	              ngonGrob(0.5, 0.5,
	                      col=col, units.def="npc", ...))))
	}
		)
	  }

  icon <- function(.) {
	customGrob(c(1/4, 1/2, 3/4), c(1/4, 1/2, 3/4), 
		size=c(10, 3, 5),  
		col=c("#E41A1C",  "#377EB8",  "#4DAF4A"))
  }

  examples <- function() {# 
	library(ggplotpp)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
str(dsmall)
d <- ggplot(dsmall, aes(carat, price))+theme_minimal()

d + geom_custom(aes(colour = carat), size=2)
d + geom_custom(aes(colour = carat), size=2, customGrob=ngonGrob)

  }
})



# now lives in zzz.r# # 
# geom_custom <- GeomCustom$build_accessor()
# 

# grid.draw(
# icon()
# )

 
# 
# dsmall <- diamonds[sample(nrow(diamonds), 100), ]
# str(dsmall)
# d <- ggplot(dsmall, aes(carat, price))
# 
# d + geom_ngon(aes(fill=color, sides=cut, size=x))
# 
# 
