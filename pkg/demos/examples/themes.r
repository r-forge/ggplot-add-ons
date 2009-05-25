library(ggplotpp)


d <- diamonds[sample(nrow(diamonds), 100), ]
p <- ggplot(d, aes(carat, price)) 
str(d)

d <- diamonds[sample(nrow(diamonds), 100), ]
p <- ggplot(d, aes(carat, price)) + theme_minimal()
str(d)


p1 <- 
p + geom_ngon(aes(fill = carat, sides=cut), colour=NA,ar=1,  size=2, angle=pi/2)



arrange(p1, p1, p1, ncol=1)