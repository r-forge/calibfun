###############################################################################
## basicAnalyses.R
##
## Basic descriptive statistics and individual comparisons
##		 and analyses 
##
## Author: Haaland
###############################################################################
load("rdata/preData.RData")
load("rdata/postData.RData")
library(ggplot2)
library(psych)
library(GPArotation)
library(Hmisc)

#########################################################################
## descriptive stats
#########################################################################
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

#########################################################################
## compare themes to time difference scores via lm
#########################################################################

loopvars = c("laborLand","intuitMov","physEnv","emotEnv","fluidReal",
		"intensePres","painExp","expectations","outcomeMeasures")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"tDiffAct")]
	names(df)[1]="x"
	lm1=lm(tDiffAct~x,data=df)
	print(summary(lm1))
}
## how to read an "lm" summary?
## none show significant linear regression

loopvars = c("laborLand","intuitMov","physEnv","emotEnv","fluidReal",
		"intensePres","painExp","expectations","outcomeMeasures")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"tDiffPush")]
	names(df)[1]="x"
	lm1=lm(tDiffPush~x,data=df)
	print(summary(lm1))
}
## Nope, none

pdf("plots/tDiffComparisons.pdf")
#########################################################################
## create plots of actual-perceived scores & different themes
#########################################################################
ggplot(data=postDf,aes(x=tActualAct,y=tPerceivedAct))+geom_point()+
		geom_abline(intercept=0, slope=1)
ggplot(data=postDf,aes(x=tActualPush,y=tPerceivedPush))+geom_point()+
		geom_abline(intercept=0, slope=1)

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
## check to see if difference scores are different for low v. high laborLand
#########################################################################
aovdiff1 = aov(tDiffAct~mslaborLand,data=postDf)
summary(aovdiff1)
# summary(aovdiff1)
#             Df Sum Sq Mean Sq F value Pr(>F)
# mslaborLand  1   1.53   1.530   0.319  0.576
# Residuals   33 158.01   4.788               
ddply(postDf,.(mslaborLand),function(df){
			data.frame(mean = mean(df$tDiffAct),
					se=sqrt(var(df$tDiffAct)/nrow(df)),
					n=nrow(df))
		})
#mslaborLand     mean        se  n
#1         low 1.529412 0.6070520 17
#2        high 1.111111 0.4345299 18

t.test(tDiffAct~mslaborLand, var.equal = TRUE, data=postDf)
# t.test(tDiffAct~mslaborLand, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  tDiffAct by mslaborLand 
# t = 0.5652, df = 33, p-value = 0.5757
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -1.087349  1.923950 
# sample estimates:
#  mean in group low mean in group high 
#           1.529412           1.111111 
# 


t.test(tDiffPush~mslaborLand, var.equal = TRUE, data=postDf)
# t.test(tDiffPush~mslaborLand, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  tDiffPush by mslaborLand 
# t = -0.7314, df = 33, p-value = 0.4697
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -29.66000  13.97373 
# sample estimates:
#  mean in group low mean in group high 
#          -1.176471           6.666667 
# 
ddply(postDf,.(mslaborLand),function(df){
			data.frame(mean = mean(df$tDiffPush),
					se=sqrt(var(df$tDiffPush)/nrow(df)),
					n=nrow(df))
		})
#mslaborLand      mean       se  n
#1         low -1.176471 6.056253 17
#2        high  6.666667 8.707295 18



