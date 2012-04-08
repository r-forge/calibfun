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


#########################################################################
## check correlations between time scores and laborland
#########################################################################
groups3Df = data.frame(tActualAct = postDf$tActualAct, tPerceivedAct = postDf$tPerceivedAct,
		tDiffAct = postDf$tDiffAct, tActualPush = postDf$tActualPush, 
		tPerceivedPush = postDf$tPerceivedPush, tDiffPush = postDf$tDiffPush,
		laborLand = postDf$laborLand)
grp3corrtest = corr.test(groups3Df)
# correlations, unadjusted p and adjusted p
grp3cres = data.frame(round(grp3corrtest$r[,7],3),
		round(grp3corrtest$p[7,],4),
		round(grp3corrtest$p[,7],4))
names(grp3cres) = c("cor.","p-raw","p-adjusted")
grp3cres
# grp3cres
#                  cor.  p-raw p-adjusted
# tActualAct     -0.336 0.0486      0.816
# tPerceivedAct  -0.271 0.1160      1.000
# tDiffAct       -0.089 0.6097      1.000
# tActualPush    -0.170 0.3280      1.000
# tPerceivedPush -0.263 0.1267      1.000
# tDiffPush       0.131 0.4542      1.000
# laborLand       1.000 0.0000      0.000




#########################################################################
## check correlations between time scores and themes
#########################################################################
groups4Df = data.frame(tActualAct = postDf$tActualAct, tPerceivedAct = postDf$tPerceivedAct,
		tDiffAct = postDf$tDiffAct, tActualPush = postDf$tActualPush, 
		tPerceivedPush = postDf$tPerceivedPush, tDiffPush = postDf$tDiffPush,
		intuitMov = postDf$intuitMov)
grp4corrtest = corr.test(groups4Df)
# correlations, unadjusted p and adjusted p
grp4cres = data.frame(round(grp4corrtest$r[,7],3),
		round(grp4corrtest$p[7,],4),
		round(grp4corrtest$p[,7],4))
names(grp4cres) = c("cor.","p-raw","p-adjusted")
grp4cres
# grp4cres
#                  cor.  p-raw p-adjusted
# tActualAct     -0.196 0.2590          1
# tPerceivedAct  -0.311 0.0693          1
# tDiffAct        0.260 0.1309          1
# tActualPush     0.183 0.2932          1
# tPerceivedPush  0.095 0.5865          1
# tDiffPush       0.146 0.4024          1
# intuitMov       1.000 0.0000          0
# > cat("Synch1333835332832744000\n");


## none with outcomeMeasures, painExp, intuitMov, ...

#########################################################################
## plot time scores
#########################################################################
ggplot(postDf,aes(x=tActualAct, y=tPerceivedAct)) + 
		geom_point() + geom_jitter() + geom_abline()




#########################################################################
## look at within time-score correlations
#########################################################################
groups5Df = data.frame(tActualAct = postDf$tActualAct, tPerceivedAct = postDf$tPerceivedAct,
		tDiffAct = postDf$tDiffAct, tActualPush = postDf$tActualPush, 
		tPerceivedPush = postDf$tPerceivedPush, tDiffPush = postDf$tDiffPush)
grp5corrtest = corr.test(groups5Df)
grp5corrtest


round(grp5corrtest$r,2)
# round(grp5corrtest$r,2)
#                tActualAct tPerceivedAct tDiffAct tActualPush tPerceivedPush
# tActualAct           1.00          0.87     0.12        0.30           0.28
# tPerceivedAct        0.87          1.00    -0.37        0.11           0.13
# tDiffAct             0.12         -0.37     1.00        0.34           0.25
# tActualPush          0.30          0.11     0.34        1.00           0.80
# tPerceivedPush       0.28          0.13     0.25        0.80           1.00
# tDiffPush            0.05         -0.02     0.15        0.38          -0.26
#                tDiffPush
# tActualAct          0.05
# tPerceivedAct      -0.02
# tDiffAct            0.15
# tActualPush         0.38
# tPerceivedPush     -0.26
# tDiffPush           1.00


round(grp5corrtest$p,3)
# (Entries above the diagonal are adjusted for multiple tests.) 
# round(grp5corrtest$p,3)
#                tActualAct tPerceivedAct tDiffAct tActualPush tPerceivedPush
# tActualAct          0.000         0.000    1.000       0.833          0.986
# tPerceivedAct       0.000         0.000    0.334       1.000          1.000
# tDiffAct            0.474         0.028    0.000       0.528          1.000
# tActualPush         0.083         0.517    0.048       0.000          0.000
# tPerceivedPush      0.110         0.443    0.143       0.000          0.000
# tDiffPush           0.763         0.891    0.389       0.025          0.132
#                tDiffPush
# tActualAct         1.000
# tPerceivedAct      1.000
# tDiffAct           1.000
# tActualPush        0.326
# tPerceivedPush     1.000
# tDiffPush          0.000


# to test only the 2 correlations
groups6Df = data.frame(tActualAct = postDf$tActualAct, tPerceivedAct = postDf$tPerceivedAct)
grp6corrtest = corr.test(groups6Df)
grp6corrtest

groups7Df = data.frame(tActualPush = postDf$tActualPush, tPerceivedPush = postDf$tPerceivedPush)
grp7corrtest = corr.test(groups7Df)
grp7corrtest


#########################################################################
## look at laborland v. each outcome measures component
#########################################################################

groups2Df = data.frame(P065 = postDf$P065,
		P066 = postDf$P066, P122 = postDf$P122, P149 = postDf$P149,
		P150 = postDf$P150, P151 = postDf$P151,
		laborLand = postDf$laborLand)
grp2corrtest = corr.test(groups2Df)
# correlations, unadjusted p and adjusted p
grp2cres = data.frame(round(grp2corrtest$r[,7],3),
		round(grp2corrtest$p[7,],4),
		round(grp2corrtest$p[,7],4))
names(grp2cres) = c("cor.","p-raw","p-adjusted")
grp2cres
# grp2cres
#            cor.  p-raw p-adjusted
# P065      0.546 0.0007     0.0111
# P066      0.378 0.0253     0.2277
# P122      0.311 0.0688     0.3442
# P149      0.529 0.0011     0.0164
# P150      0.529 0.0011     0.0164
# P151      0.456 0.0059     0.0653
# laborLand 1.000 0.0000     0.0000


#########################################################################
## check these outcome measures questions against meeting expectations
#########################################################################
groups3Df = data.frame(P065 = postDf$P065,
		P066 = postDf$P066, P122 = postDf$P122, P149 = postDf$P149,
		P150 = postDf$P150, P151 = postDf$P151,
		expectations = postDf$expectations)
grp3corrtest = corr.test(groups3Df)
# correlations, unadjusted p and adjusted p
grp3cres = data.frame(round(grp3corrtest$r[,7],3),
		round(grp3corrtest$p[7,],4),
		round(grp3corrtest$p[,7],4))
names(grp3cres) = c("cor.","p-raw","p-adjusted")
grp3cres
# grp3cres
#               cor.  p-raw p-adjusted
# P065         0.337 0.0478     0.3485
# P066         0.251 0.1456     0.7281
# P122         0.138 0.4299     1.0000
# P149         0.414 0.0135     0.1753
# P150         0.375 0.0263     0.2897
# P151         0.603 0.0001     0.0022
# expectations 1.000 0.0000     0.0000

## NOPE :(

#########################################################################
## maybe doula helps? On a few - not interesting enough to report...
#########################################################################
aov1 = aov(P065~P026,data=postDf)
summary(aov1)
# summary(aov1)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# P026         1   4.80   4.803   4.841 0.0349 *
# Residuals   33  32.74   0.992                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

aov2 = aov(P066~P026,data=postDf)
summary(aov2)
# summary(aov2)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P026         1  0.046  0.0457    0.05  0.825
# Residuals   33 30.240  0.9164               

aov3 = aov(P122~P026,data=postDf)
summary(aov1)
# summary(aov1)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# P026         1   4.80   4.803   4.841 0.0349 *
# Residuals   33  32.74   0.992                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

aov4 = aov(P149~P026,data=postDf)
summary(aov4)
# summary(aov4)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P026         1   1.79   1.786   1.614  0.213
# Residuals   33  36.50   1.106               

aov5 = aov(P150~P026,data=postDf)
summary(aov5)
# summary(aov5)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# P026         1   9.95   9.946   5.403 0.0264 *
# Residuals   33  60.74   1.841                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

aov6 = aov(P151~P026,data=postDf)
summary(aov6)
# summary(aov6)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P026         1   0.56  0.5600    0.58  0.452
# Residuals   33  31.84  0.9648               







