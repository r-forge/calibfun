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

loopvars = c("laborLand","intuitMov","physEnv","emotEnv","fluidReal",
		"intensePres","painExp","expectations","outcomeMeasures")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"tDiffPush")]
	names(df)[1]="x"
	lm1=lm(tDiffPush~x,data=df)
	print(summary(lm1))
}

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
## analysis of variance based on categories of mslaborLand
#########################################################################
# IV: mslaborLand; DV: outcomeMeasures
aov1 = aov(outcomeMeasures~mslaborLand,data=postDf)
summary(aov1)
# summary(aov1)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# mslaborLand  1  228.6  228.55   17.74 0.000184 ***
# Residuals   33  425.1   12.88                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# this plot isn't very helpful - don't need to do it
plot(aov1)

# this code block plots om v. ll with error bars [CI] added
g1=ggplot(postDf,aes(x=mslaborLand, y=outcomeMeasures))
stat_sum_df <- function(fun, geom="crossbar", ...) { 
	stat_summary(fun.data=fun, colour="red", geom=geom, width=0.2, ...) 
}
# means plus 95% confidence interval
g1 + stat_sum_df("mean_cl_normal", geom = "errorbar") + 
		stat_sum_df("mean_cl_normal", geom = "point") 


#########################################################################
## 2x3 analysis of variance: outcomeMeasures~mslaborLand*lmhpanas
#########################################################################
aov2 = aov(outcomeMeasures~mslaborLand*lmhpanas,data=postDf)
summary(aov2)
# summary(aov2)
#                      Df Sum Sq Mean Sq F value   Pr(>F)    
# mslaborLand           1 228.55  228.55  21.472 7.01e-05 ***
# lmhpanas              2  45.10   22.55   2.119   0.1384    
# mslaborLand:lmhpanas  2  71.31   35.66   3.350   0.0491 *  
# Residuals            29 308.69   10.64                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 


## this plot isn't very helpful - don't need to do it each time
plot(aov2)

pdf("plots/2x3anova.pdf")
ggplot(postDf,aes(x=lmhpanas, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot") 
dev.off()

#########################################################################
## analysis of variance: effect of panas on laborLand
#########################################################################
aov3 = aov(laborLand~lmhpanas,data=postDf)
summary(aov3)
# summary(aov3)
#             Df Sum Sq Mean Sq F value Pr(>F)
# lmhpanas     2  215.6  107.82   1.094  0.347
# Residuals   32 3153.9   98.56                           

## this says that panas scores do not create a significant difference
## in laborLand scores

#########################################################################
## looking at other relationships
#########################################################################
# shows that painExp is much higher for high ll women
ggplot(postDf,aes(x=mslaborLand, y=painExp)) + 
		stat_summary(fun.data = "mean_cl_boot") 
# show that this is a significant difference
aov4 = aov(painExp~mslaborLand,data=postDf)
summary(aov4)
# summary(aov4)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
# mslaborLand  1  248.3  248.26   9.387 0.00433 **
# Residuals   33  872.8   26.45                   
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1               


# this shows that high ll is clustered in the corner of best
# painExp and best outcomeMeasures; but low ll is spread all 
# along the axis (but still w/ a correlation)
ggplot(postDf,aes(x=painExp, y=outcomeMeasures, color=mslaborLand))+geom_point()
aov5 = aov(outcomeMeasures~mspainExp*mslaborLand,data=postDf)
summary(aov5)
# summary(aov5)
#                       Df Sum Sq Mean Sq F value   Pr(>F)    
# mspainExp              1 192.01  192.01  19.152 0.000127 ***
# mslaborLand            1 128.67  128.67  12.835 0.001148 ** 
# mspainExp:mslaborLand  1  22.18   22.18   2.213 0.146986    
# Residuals             31 310.79   10.03                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
ggplot(postDf,aes(x=mspainExp, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot") 

# this plot is really interesting - shows that laborLand & painExp
# aren't really interacting for medium panas; but they are for
# low and high panas; means that if you're high panas, laborLand 
# really helps you, and that even if you are low panas, if you
# manage to get into laborLand, you can still reap its benefits 
# (same as for outcome measures)
ggplot(postDf,aes(x=lmhpanas, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov8 = aov(painExp~mslaborLand*lmhpanas,data=postDf)
summary(aov8)
# summary(aov8)
#                      Df Sum Sq Mean Sq F value Pr(>F)   
# mslaborLand           1  248.3  248.26  10.492  0.003 **
# lmhpanas              2   93.7   46.83   1.979  0.156   
# mslaborLand:lmhpanas  2   92.9   46.46   1.963  0.159   
# Residuals            29  686.2   23.66                  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## why isn't this interaction significant when the other one
# (y=outcomeMeasures) is??

#########################################################################
## does laborLand mediate the expectations->outcomeMeasures relationship?
#########################################################################
ggplot(postDf,aes(x=msexpectations, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov6 = aov(outcomeMeasures~msexpectations*mslaborLand,data=postDf)
summary(aov6)
# summary(aov6)
#                            Df Sum Sq Mean Sq F value   Pr(>F)    
# msexpectations              1  92.95   92.95   9.815  0.00377 ** 
# mslaborLand                 1 237.41  237.41  25.068 2.11e-05 ***
# msexpectations:mslaborLand  1  29.70   29.70   3.136  0.08642 .  
# Residuals                  31 293.59    9.47                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#what about expectations->painExp?
ggplot(postDf,aes(x=msexpectations, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov7 = aov(painExp~msexpectations*mslaborLand,data=postDf)
summary(aov7)
# summary(aov7)
#                            Df Sum Sq Mean Sq F value   Pr(>F)    
# msexpectations              1  275.3  275.30   16.55 0.000302 ***
# mslaborLand                 1  264.1  264.11   15.88 0.000381 ***
# msexpectations:mslaborLand  1   66.0   66.03    3.97 0.055182 .  
# Residuals                  31  515.6   16.63                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 


#########################################################################
## look at outcomeMeasures~mslaborLand as each individual question
#########################################################################
# outcomeMeasures = P065,P066,P122,P149,P150,P151

loopvars = c("P065","P066","P122","P149","P150","P151")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"mslaborLand")]
	names(df)[1]="y"
	aov9=aov(y~mslaborLand,data=df)
	print(summary(aov9))
}
# i= 1  var= P065 
# Df Sum Sq Mean Sq F value  Pr(>F)   
# mslaborLand  1  9.383   9.383   10.99 0.00223 **
# 		Residuals   33 28.160   0.853                   
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# i= 2  var= P066 
# Df Sum Sq Mean Sq F value Pr(>F)
# mslaborLand  1  2.243  2.2432    2.64  0.114
# Residuals   33 28.042  0.8498               
# i= 3  var= P122 
# Df Sum Sq Mean Sq F value Pr(>F)  
# mslaborLand  1  2.073  2.0729   3.309  0.078 .
# Residuals   33 20.670  0.6264                 
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# i= 4  var= P149 
# Df Sum Sq Mean Sq F value  Pr(>F)   
# mslaborLand  1  10.48  10.479   12.44 0.00126 **
# 		Residuals   33  27.81   0.843                   
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# i= 5  var= P150 
# Df Sum Sq Mean Sq F value Pr(>F)   
# mslaborLand  1  20.02  20.016   13.04  0.001 **
# 		Residuals   33  50.67   1.535                  
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# i= 6  var= P151 
# Df Sum Sq Mean Sq F value  Pr(>F)   
# mslaborLand  1  6.959   6.959   9.026 0.00505 **
# 		Residuals   33 25.441   0.771                   
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

## what might better predict P066(I felt in control of the care I 
## received) and P122(I was involved in all decision-making 
## processes (especially regarding interventions))



