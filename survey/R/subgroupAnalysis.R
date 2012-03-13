###############################################################################
## subgroupAnalysis.R
##
## splitting interactions into subgroups
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
## make new variable to get at laborland/panas interaction
#########################################################################
ggplot(postDf,aes(x=laborLand,y=outcomeMeasures, color=mspanas))+geom_point()
#panas/laborLand interaction
pdf("plots/outcomeMeasures~mspanas*mslaborLand.pdf")
ggplot(postDf,aes(x=mspanas, y=outcomeMeasures, color=mslaborLand))+ 
		stat_summary(fun.data = "mean_cl_boot") 
dev.off()
tempdf = postDf
tempdf$g1 = "high-high"
tempdf$g1[tempdf$mspanas=="low"&tempdf$mslaborLand=="low"] = "low-low"
tempdf$g1[tempdf$mspanas=="low"&tempdf$mslaborLand=="high"] = "low-high"
tempdf$g1[tempdf$mspanas=="high"&tempdf$mslaborLand=="low"] = "high-low"
tempdf$g1 = factor(tempdf$g1)

#painExp/laborLand interaction
pdf("plots/outcomeMeasures~mspainExp*mslaborLand.pdf")
ggplot(postDf,aes(x=mspainExp,y=outcomeMeasures, color=mslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")
dev.off()
tempdf$g2 = "high-high"
tempdf$g2[tempdf$mspainExp=="low"&tempdf$mslaborLand=="low"] = "low-low"
tempdf$g2[tempdf$mspainExp=="low"&tempdf$mslaborLand=="high"] = "low-high"
tempdf$g2[tempdf$mspainExp=="high"&tempdf$mslaborLand=="low"] = "high-low"
tempdf$g2 = factor(tempdf$g2)


ggplot(postDf,aes(x=msexpectations, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=msfluidReal, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=msintensePres, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=educationsplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=msactiveLabor, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=P027, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=P051, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=msactiveFeel, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

# y=painExp
ggplot(postDf,aes(x=mspanas, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=msexpectations, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=mspanas,y=painExp,color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

# y=tActualAct
ggplot(postDf,aes(x=primipsplit, y=tActualAct, color=agesplit)) + 
		stat_summary(fun.data = "mean_cl_boot")

#########################################################################
## now try new 1-way ANOVA strategy
#########################################################################
aov1 = aov(outcomeMeasures~g1,data=tempdf)
summary(aov1)
# summary(aov1)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
# g1           3  240.7   80.22   6.022 0.00235 **
# Residuals   31  413.0   13.32                   
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
TukeyHSD(aov1)
# TukeyHSD(aov1)
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = outcomeMeasures ~ g1, data = tempdf)
# 
# $g1
#                          diff        lwr       upr     p adj
# high-low-high-high -2.9634810  -7.916631  1.989669 0.3805173
# low-high-high-high  0.3356364  -4.617514  5.288787 0.9977437
# low-low-high-high  -5.7759628  -9.911086 -1.640840 0.0034584
# low-high-high-low   3.2991175  -2.420288  9.018523 0.4124046
# low-low-high-low   -2.8124818  -7.840120  2.215156 0.4391529
# low-low-low-high   -6.1115993 -11.139237 -1.083961 0.0123873
pdf("plots/outcomeMeasuresvg1.pdf")
ggplot(tempdf,aes(x=g1,y=outcomeMeasures, color=mslaborLand))+geom_point()
dev.off()
hist(postDf$panas,nclass=30)

#########################################################################
## look for factor that helps laborLand in bad circumstances
#########################################################################
subpanasdf=subset(tempdf,mspanas=="low")
#aov1b = aov(P023~g1,data=subdf)
#summary(aov1b)
loopvars = postNumVarNames
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=subpanasdf[,c(loopvars[i],"g1")]
	names(df)[1]="y"
	aov1b=aov(y~g1,data=df)
	print(summary(aov1b))
}
pdf("plots/inlowpanas-variableV.laborLand.pdf")
#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P042))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P042))+stat_summary(fun.data = "mean_cl_boot")

#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P062))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P062))+stat_summary(fun.data = "mean_cl_boot")

#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P063))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P063))+stat_summary(fun.data = "mean_cl_boot")

#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P071))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P071))+stat_summary(fun.data = "mean_cl_boot")

#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P087))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P087))+stat_summary(fun.data = "mean_cl_boot")

#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P114))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P114))+stat_summary(fun.data = "mean_cl_boot")

#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P124))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P124))+stat_summary(fun.data = "mean_cl_boot")

#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P125))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P125))+stat_summary(fun.data = "mean_cl_boot")

#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P133))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P133))+stat_summary(fun.data = "mean_cl_boot")

#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P142))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P142))+stat_summary(fun.data = "mean_cl_boot")

#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P145))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P145))+stat_summary(fun.data = "mean_cl_boot")

#ggplot(subpanasdf,aes(x=g1,y=laborLand,color=P156))+geom_point()
ggplot(subpanasdf,aes(x=mslaborLand,y=P156))+stat_summary(fun.data = "mean_cl_boot")
dev.off()
#########################################################################
## do comparisons within subgroups of painExp
#########################################################################
aov2 = aov(outcomeMeasures~g2,data=tempdf)
summary(aov2)
# summary(aov2)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# g2           3  334.7  111.56   10.84 4.95e-05 ***
# Residuals   31  319.0   10.29                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
TukeyHSD(aov2)
# TukeyHSD(aov2)
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = outcomeMeasures ~ g2, data = tempdf)
# 
# $g2
#                          diff        lwr       upr     p adj
# high-low-high-high -1.9117921  -6.264784  2.441199 0.6363602
# low-high-high-high -1.6074858  -5.960477  2.745506 0.7492193
# low-low-high-high  -7.3506137 -10.984696 -3.716531 0.0000301
# low-high-high-low   0.3043064  -4.722095  5.330708 0.9983864
# low-low-high-low   -5.4388215  -9.857275 -1.020368 0.0111542
# low-low-low-high   -5.7431279 -10.161582 -1.324674 0.0069120
#########################################################################
## look for factor that helps laborLand in bad circumstances
#########################################################################
subpaindf=subset(tempdf,mspainExp=="low")
loopvars = postNumVarNames
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=subpaindf[,c(loopvars[i],"g2")]
	names(df)[1]="y"
	aov2b=aov(y~g2,data=df)
	print(summary(aov2b))
}
ggplot(subdf,aes(x=g2,y=laborLand,color=P071))+geom_point()
ggplot(subdf,aes(x=mslaborLand,y=P071))+stat_summary(fun.data = "mean_cl_boot")

ggplot(subdf,aes(x=g2,y=laborLand,color=P087))+geom_point()
ggplot(subdf,aes(x=mslaborLand,y=P087))+stat_summary(fun.data = "mean_cl_boot")

ggplot(subdf,aes(x=g2,y=laborLand,color=P114))+geom_point()
ggplot(subdf,aes(x=mslaborLand,y=P114))+stat_summary(fun.data = "mean_cl_boot")

ggplot(subdf,aes(x=g2,y=laborLand,color=P133))+geom_point()
ggplot(subdf,aes(x=mslaborLand,y=P133))+stat_summary(fun.data = "mean_cl_boot")

ggplot(subdf,aes(x=g2,y=laborLand,color=P145))+geom_point()
ggplot(subdf,aes(x=mslaborLand,y=P145))+stat_summary(fun.data = "mean_cl_boot")

ggplot(subdf,aes(x=g2,y=laborLand,color=P156))+geom_point()
ggplot(subdf,aes(x=mslaborLand,y=P156))+stat_summary(fun.data = "mean_cl_boot")




