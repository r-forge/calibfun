###############################################################################
## plotHistograms.R
##
## make histograms of every response
## 
## Author: Haaland
###############################################################################

load("rdata/curData.RData")
library(ggplot2)



ggplot(data=allDf,aes(x=Q34_3)) + geom_histogram() +
		xlab("Response") + ylab("Frequency") +
		opts(title=subset(descDf,varName=="Q34_3")[,2],size=2)

