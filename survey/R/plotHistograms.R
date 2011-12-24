###############################################################################
## plotHistograms.R
##
## make histograms of every response
## 
## Author: Haaland
###############################################################################


library(ggplot2)

ggplot(data=allDf,aes(x=Q34_3)) + geom_histogram()

