###############################################################################
## plotHistograms.R
##
## make histograms of every response
## 
## Author: Haaland
###############################################################################

load("rdata/preData.RData")
library(ggplot2)

pdf("plots/preHistograms.pdf")
for(i in 1:nrow(descPreDf)){
	vname = descPreDf$varName[i]
	adf = preDf
	names(adf)[names(adf)==vname] = "x"
	if(vname %in% preNumVarNames){
		adf$x = factor(adf$x,levels=max(adf$x):min(adf$x))
	}
	bdf = subset(descPreDf,varName==vname)
	titleText = paste(bdf[,"varName"]," (",bdf[,"oldName"],"):",as.character(bdf[,"varDesc"]),
			sep="")
	g = ggplot(data=adf,aes(x=x)) + geom_histogram(size=.4) +
			xlab("Response") + ylab("Frequency") +
			opts(title=titleText,size=2) +
			coord_flip()
	print(g)
}
dev.off()

load("rdata/postData.RData")

pdf("plots/postHistograms.pdf")
for(i in 1:nrow(descPostDf)){
	vname = descPostDf$varName[i]
	adf = postDf
	names(adf)[names(adf)==vname] = "x"
	if(vname %in% postNumVarNames){
		adf$x = factor(adf$x,levels=max(adf$x):min(adf$x))
	}
	bdf = subset(descPostDf,varName==vname)
	titleText = paste(bdf[,"varName"]," (",bdf[,"oldName"],"):",as.character(bdf[,"varDesc"]),
			sep="")
	g = ggplot(data=adf,aes(x=x)) + geom_histogram(size=.4) +
			xlab("Response") + ylab("Frequency") +
			opts(title=titleText,size=2) +
			coord_flip()
	print(g)
}
dev.off()

