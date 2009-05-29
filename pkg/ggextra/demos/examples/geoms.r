library(ggextra)
library(dichromat)

d <- diamonds[sample(nrow(diamonds), 100), ]
p <- ggplot(d, aes(carat, price)) + theme_minimal()
str(d)


p1 <- 
p + geom_ngon(aes(fill = carat, sides=cut), colour=NA,ar=1,  size=2, angle=pi/2)

p2 <- 
p + geom_ellipse(aes(fill=clarity, ar=cut, angle=depth), colour="grey50", size=2)+
scale_fill_dichromat(type="seq", palette=1) + facet_grid(cut~color)


p3 <- 
p + geom_star(aes(colour = carat, sides=cut),  ar=1,  size=2, angle=pi/2)+ 
facet_grid(cut~.) 

pdf(width=20, height=12)
arrange(p1, p2, p3)
dev.off()


