# 
# 
# install.packages("pgfSweave", repos="http://R-Forge.R-project.org")
# install.packages("cacheSweave")
# install.packages("stashR")
library(pgfSweave)
?pgfSweave

pgfSweave("pgfSweave-ggplot2.Rnw",pdf=TRUE)

