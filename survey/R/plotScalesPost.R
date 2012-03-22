###############################################################################
## plotScalesPost.R
##
## plot scales (correlations)
## 
## Author: Haaland
###############################################################################
library(ggplot2)
library(psych)
library(GPArotation)
library(Hmisc)
load("rdata/postData.RData")

# I know that most of my themes are no longer correlated with laborLand,
# but I'm interested if they're correlated with outcomeMeasures
# BUT, still want to check painExp,expectations,panas,outcomeMeasures
# against laborLand
groups1Df = data.frame(intuitMov = postDf$intuitMov, physEnv = postDf$physEnv,
		emotEnv = postDf$emotEnv, fluidReal = postDf$fluidReal, intensePres = postDf$intensePres,
		painExp = postDf$painExp, expectations = postDf$expectations,
		vocals = postDf$vocals, panas = postDf$panas, laborLand = postDf$laborLand, 
		outcomeMeasures = postDf$outcomeMeasures)
grp1corrtest = corr.test(groups1Df)
# correlations, unadjusted p and adjusted p
grp1cres = data.frame(round(grp1corrtest$r[,11],3),
		round(grp1corrtest$p[11,],4),
		round(grp1corrtest$p[,11],4))
names(grp1cres) = c("cor.","p-raw","p-adjusted")
grp1cres
# grp1cres
#                  cor.  p-raw p-adjusted
# intuitMov       0.696 0.0000     0.0002
# physEnv         0.552 0.0006     0.0234
# emotEnv         0.733 0.0000     0.0000
# fluidReal       0.554 0.0006     0.0226
# intensePres     0.543 0.0007     0.0282
# painExp         0.760 0.0000     0.0000
# expectations    0.483 0.0033     0.0956
# vocals          0.028 0.8753     1.0000
# panas           0.423 0.0114     0.2947
# laborLand       0.627 0.0001     0.0027
# outcomeMeasures 1.000 0.0000     0.0000

## to get the whole correlation matrix
# grp1corrtest$r

groups2Df = data.frame(painExp = postDf$painExp, expectations = postDf$expectations,
		panas = postDf$panas, outcomeMeasures = postDf$outcomeMeasures,
		laborLand = postDf$laborLand)
grp2corrtest = corr.test(groups2Df)
# correlations, unadjusted p and adjusted p
grp2cres = data.frame(round(grp2corrtest$r[,5],3),
		round(grp2corrtest$p[5,],4),
		round(grp2corrtest$p[,5],4))
names(grp2cres) = c("cor.","p-raw","p-adjusted")
grp2cres
# grp2cres
#                  cor.  p-raw p-adjusted
# painExp         0.515 0.0015     0.0123
# expectations    0.333 0.0504     0.0923
# panas           0.384 0.0229     0.0917
# outcomeMeasures 0.627 0.0001     0.0005
# laborLand       1.000 0.0000     0.0000


library(ggplot2)
pdf("plots/scaleCorrelations.pdf")
#########################################################################
## First, compare themes to laborLand
#########################################################################
## painExp
adf = grp2cres[rownames(grp2cres)=="painExp",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groups2Df,aes(x=painExp, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## expectations
adf = grp2cres[rownames(grp2cres)=="expectations",]
adf$x = -4
adf$y = 12
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groups2Df,aes(x=expectations, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## outcomeMeasures
groups2Df$P016 = postDf$P016
adf = grp2cres[rownames(grp2cres)=="outcomeMeasures",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groups2Df,aes(x=outcomeMeasures, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## panas
adf = grpcres[rownames(grpcres)=="panas",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=panas, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)

#########################################################################
## Now compare each theme to outcome measures
#########################################################################
## intuitMov
adf = grp1cres[rownames(grp1cres)=="intuitMov",]
adf$x = -6
adf$y = 5
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groups1Df,aes(x=intuitMov, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## physEnv
adf = grp1cres[rownames(grp1cres)=="physEnv",]
adf$x = -6
adf$y = 5
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groups1Df,aes(x=physEnv, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## emotEnv
adf = grp1cres[rownames(grp1cres)=="emotEnv",]
adf$x = -10
adf$y = 6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groups1Df,aes(x=emotEnv, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## fluidReal
adf = grp1cres[rownames(grp1cres)=="fluidReal",]
adf$x = -10
adf$y = 6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groups1Df,aes(x=fluidReal, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## intensePres
adf = grp1cres[rownames(grp1cres)=="intensePres",]
adf$x = -10
adf$y = 6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groups1Df,aes(x=intensePres, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## painExp
adf = grp1cres[rownames(grp1cres)=="painExp",]
adf$x = -10
adf$y = 5
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groups1Df,aes(x=painExp, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## expectations
adf = grp1cres[rownames(grp1cres)=="expectations",]
adf$x = -3
adf$y = 6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groups1Df,aes(x=expectations, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## panas
adf = grp1cres[rownames(grp1cres)=="panas",]
adf$x = 10
adf$y = -6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groups1Df,aes(x=panas, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)

dev.off()
