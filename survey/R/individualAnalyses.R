###############################################################################
## individualAnalyses.R
##
## Basic descriptive statistics and individual comparisons
##		 and analyses 
##
## Author: Haaland
###############################################################################
load("rdata/preData.RData")
load("rdata/postData.RData")
library(ggplot2)
## do descriptive statistics for all questions
# first, create a data.frame with only numeric responses
numPreDf <- preDf[,preNumVarNames]
#calculate mean and standard deviation and standard error
sumFun = function(x) {
	data.frame(Mean=round(mean(x),2),
			SD = round(sd(x),2), 
			SE = round(sd(x)/sqrt(length(x)),2),
			n = length(x))
}


sumPreDf = sapply(numPreDf,sumFun)
head(sumPreDf)
sumPreDf = t(sumPreDf)
head(sumPreDf)
# head(sumPreDf)
#      Mean SD   SE   n 
# N001 1.67 0.78 0.22 12
# N003 1    0    0    12
# N004 3    1.04 0.3  12
# N006 5    1.6  0.46 12
# N007 1    0    0    12
# N008 4.83 3.88 1.12 12

# first, create a data.frame with only numeric responses
numPostDf <- postDf[,postNumVarNames]
#calculate mean and standard deviation
sumPostDf = t(sapply(numPostDf,sumFun))
head(sumPostDf)
# head(sumPostDf)
#      Mean SD   SE   n 
# P001 2.09 1.38 0.29 23
# P003 1    0    0    23
# P004 2.48 0.9  0.19 23
# P006 4.7  1.33 0.28 23
# P007 1    0    0    23
# P008 5.61 3.63 0.76 23

## look at the data and determine the questions that have no 
## standard deviation
# P003, P007, P024

pdf("plots/scales.pdf")

#########################################################################
## create plots of actual-perceived scores & different themes
#########################################################################
ggplot(data=postDf,aes(x=tActualAct,y=tPerceivedAct))+geom_point()+
		geom_abline(intercept=0, slope=1)
ggplot(data=postDf,aes(x=tActualPush,y=tPerceivedPush))+geom_point()+
		geom_abline(intercept=0, slope=1)

## example of how to add color: ggplot(data=postDf,aes(x=laborLand,y=tDiffAct,color=physEnv))+geom_point()
ggplot(data=postDf,aes(x=laborLand,y=tDiffAct))+geom_point()
ggplot(data=postDf,aes(x=intuitMov,y=tDiffAct))+geom_point()
ggplot(data=postDf,aes(x=physEnv,y=tDiffAct))+geom_point()
ggplot(data=postDf,aes(x=emotEnv,y=tDiffAct))+geom_point()
ggplot(data=postDf,aes(x=fluidReal,y=tDiffAct))+geom_point()
ggplot(data=postDf,aes(x=intensePres,y=tDiffAct))+geom_point()
ggplot(data=postDf,aes(x=painExp,y=tDiffAct))+geom_point()
ggplot(data=postDf,aes(x=expectations,y=tDiffAct))+geom_point()
ggplot(data=postDf,aes(x=outcomeMeasures,y=tDiffAct))+geom_point()


ggplot(data=postDf,aes(x=laborLand,y=tDiffPush))+geom_point()
ggplot(data=postDf,aes(x=intuitMov,y=tDiffPush))+geom_point()
ggplot(data=postDf,aes(x=physEnv,y=tDiffPush))+geom_point()
ggplot(data=postDf,aes(x=emotEnv,y=tDiffPush))+geom_point()
ggplot(data=postDf,aes(x=fluidReal,y=tDiffPush))+geom_point()
ggplot(data=postDf,aes(x=intensePres,y=tDiffPush))+geom_point()
ggplot(data=postDf,aes(x=painExp,y=tDiffPush))+geom_point()
ggplot(data=postDf,aes(x=expectations,y=tDiffPush))+geom_point()
ggplot(data=postDf,aes(x=outcomeMeasures,y=tDiffPush))+geom_point()

dev.off()

#########################################################################
## create affect score from PANAS questions
#########################################################################
