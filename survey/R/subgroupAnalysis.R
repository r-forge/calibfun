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
tempdf$g3 = "high-high"
tempdf$g3[tempdf$msexpectations=="low"&tempdf$mslaborLand=="low"] = "low-low"
tempdf$g3[tempdf$msexpectations=="low"&tempdf$mslaborLand=="high"] = "low-high"
tempdf$g3[tempdf$msexpectations=="high"&tempdf$mslaborLand=="low"] = "high-low"
tempdf$g3 = factor(tempdf$g3)

ggplot(postDf,aes(x=msfluidReal, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
tempdf$g4 = "high-high"
tempdf$g4[tempdf$msfluidReal=="low"&tempdf$mslaborLand=="low"] = "low-low"
tempdf$g4[tempdf$msfluidReal=="low"&tempdf$mslaborLand=="high"] = "low-high"
tempdf$g4[tempdf$msfluidReal=="high"&tempdf$mslaborLand=="low"] = "high-low"
tempdf$g4 = factor(tempdf$g4)

ggplot(postDf,aes(x=msintensePres, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
tempdf$g5= "high-high"
tempdf$g5[tempdf$msintensePres=="low"&tempdf$mslaborLand=="low"] = "low-low"
tempdf$g5[tempdf$msintensePres=="low"&tempdf$mslaborLand=="high"] = "low-high"
tempdf$g5[tempdf$msintensePres=="high"&tempdf$mslaborLand=="low"] = "high-low"
tempdf$g5 = factor(tempdf$g5)

ggplot(postDf,aes(x=educationsplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
tempdf$g6 = "high-high"
tempdf$g6[tempdf$educationsplit=="<4-year-college"&tempdf$mslaborLand=="low"] = "low-low"
tempdf$g6[tempdf$educationsplit=="<4-year-college"&tempdf$mslaborLand=="high"] = "low-high"
tempdf$g6[tempdf$educationsplit=="4-year-college+"&tempdf$mslaborLand=="low"] = "high-low"
tempdf$g6 = factor(tempdf$g6)

ggplot(postDf,aes(x=msactiveLabor, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
tempdf$g7 = "high-high"
tempdf$g7[tempdf$msactiveLabor=="<6hrs"&tempdf$mslaborLand=="low"] = "low-low"
tempdf$g7[tempdf$msactiveLabor=="<6hrs"&tempdf$mslaborLand=="high"] = "low-high"
tempdf$g7[tempdf$msactiveLabor==">6hrs"&tempdf$mslaborLand=="low"] = "high-low"
tempdf$g7 = factor(tempdf$g7)

ggplot(postDf,aes(x=P027, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
tempdf$g8 = "NO-high"
tempdf$g8[tempdf$P027=="1"&tempdf$mslaborLand=="low"] = "YES-low"
tempdf$g8[tempdf$P027=="1"&tempdf$mslaborLand=="high"] = "YES-high"
tempdf$g8[tempdf$P027=="2"&tempdf$mslaborLand=="low"] = "NO-low"
tempdf$g8 = factor(tempdf$g8)

ggplot(postDf,aes(x=P051, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
tempdf$g9 = "NO-high"
tempdf$g9[tempdf$P051=="2"&tempdf$mslaborLand=="low"] = "YES-low"
tempdf$g9[tempdf$P051=="2"&tempdf$mslaborLand=="high"] = "YES-high"
tempdf$g9[tempdf$P051=="1"&tempdf$mslaborLand=="low"] = "NO-low"
tempdf$g9 = factor(tempdf$g9)

ggplot(postDf,aes(x=msactiveFeel, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
tempdf$g10 = "high-high"
tempdf$g10[tempdf$msactiveFeel=="<4hrs"&tempdf$mslaborLand=="low"] = "low-low"
tempdf$g10[tempdf$msactiveFeel=="<4hrs"&tempdf$mslaborLand=="high"] = "low-high"
tempdf$g10[tempdf$msactiveFeel==">4hrs"&tempdf$mslaborLand=="low"] = "high-low"
tempdf$g10 = factor(tempdf$g10)

# y=painExp
ggplot(postDf,aes(x=mspanas, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=msexpectations, y=painExp, color=mslaborLand)) + 
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
## look for factor that helps laborLand in bad panas circumstances
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
ggplot(tempdf,aes(x=g2,y=outcomeMeasures, color=mslaborLand))+geom_point()

#########################################################################
## look for factor that helps laborLand in bad painExp circumstances
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


#########################################################################
## do comparisons within subgroups of msexpectations
#########################################################################
ggplot(postDf,aes(x=msexpectations, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov3 = aov(outcomeMeasures~g3,data=tempdf)
summary(aov3)
# summary(aov3)
#             Df Sum Sq Mean Sq F value  Pr(>F)    
# g3           3  275.3   91.78    7.52 0.00064 ***
# Residuals   31  378.3   12.20                    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
TukeyHSD(aov3)
# TukeyHSD(aov3)
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = outcomeMeasures ~ g3, data = tempdf)
# 
# $g3
#                          diff        lwr        upr     p adj
# high-low-high-high -2.3037662  -7.044493  2.4369609 0.5582299
# low-high-high-high -0.2081745  -4.948902  4.5325526 0.9993810
# low-low-high-high  -6.4159523 -10.373735 -2.4581700 0.0006562
# low-high-high-low   2.0955917  -3.378528  7.5697119 0.7281420
# low-low-high-low   -4.1121861  -8.924206  0.6998342 0.1154284
# low-low-low-high   -6.2077778 -11.019798 -1.3957575 0.0074017
ggplot(tempdf,aes(x=g3,y=outcomeMeasures, color=mslaborLand))+geom_point()

#########################################################################
## look for the factor that helps laborLand in bad expectations circumstances
#########################################################################
## see multipleRegs file

#########################################################################
## do comparisons within subgroups of msfluidReal
#########################################################################
##### this one doesn't quite follow the pattern of significance #####
ggplot(postDf,aes(x=msfluidReal, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov4 = aov(outcomeMeasures~g4,data=tempdf)
summary(aov4)
# summary(aov4)
#             Df Sum Sq Mean Sq F value Pr(>F)   
# g4           3  254.9   84.97   6.606 0.0014 **
# Residuals   31  398.7   12.86                  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
TukeyHSD(aov4)
# TukeyHSD(aov4)
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = outcomeMeasures ~ g4, data = tempdf)
# 
# $g4
#                         diff        lwr        upr     p adj
# high-low-high-high -2.935798  -8.058158  2.1865629 0.4180146
# low-high-high-high -1.361675  -6.484036  3.7606853 0.8877781
# low-low-high-high  -6.247463 -10.144175 -2.3507517 0.0007506
# low-high-high-low   1.574122  -4.582189  7.7304337 0.8986138
# low-low-high-low   -3.311666  -8.492974  1.8696423 0.3235750
# low-low-low-high   -4.885788 -10.067096  0.2955199 0.0702273
ggplot(tempdf,aes(x=g4,y=outcomeMeasures, color=mslaborLand))+geom_point()
##### low-low-low-high is not quite significant #####

#########################################################################
## do comparisons within subgroups of msintensePres
#########################################################################
##### this one doesn't quite follow the pattern of significance #####
ggplot(postDf,aes(x=msintensePres, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov5 = aov(outcomeMeasures~g5,data=tempdf)
summary(aov5)
# summary(aov5)
#             Df Sum Sq Mean Sq F value Pr(>F)   
# g5           3  232.9   77.63   5.719 0.0031 **
# Residuals   31  420.8   13.57                  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
TukeyHSD(aov5)
# TukeyHSD(aov5)
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = outcomeMeasures ~ g5, data = tempdf)
# 
# $g5
#                         diff        lwr        upr     p adj
# high-low-high-high -3.365759  -8.627692  1.8961735 0.3229281
# low-high-high-high -0.746337  -6.008270  4.5155957 0.9802492
# low-low-high-high  -5.826166  -9.829054 -1.8232780 0.0022548
# low-high-high-low   2.619422  -3.704634  8.9434783 0.6776602
# low-low-high-low   -2.460407  -7.782893  2.8620797 0.5978131
# low-low-low-high   -5.079829 -10.402315  0.2426575 0.0656744
ggplot(tempdf,aes(x=g5,y=outcomeMeasures, color=mslaborLand))+geom_point()
##### low-low-low-high is not quite significant #####

#########################################################################
## do comparisons within subgroups of educationsplit
#########################################################################
ggplot(postDf,aes(x=educationsplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov6 = aov(outcomeMeasures~g6,data=tempdf)
summary(aov6)
# summary(aov6)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# g6           3  278.3   92.77   7.662 0.000569 ***
# Residuals   31  375.3   12.11                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
TukeyHSD(aov6)
# TukeyHSD(aov6)
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = outcomeMeasures ~ g6, data = tempdf)
# 
# $g6
#                         diff        lwr        upr     p adj
# high-low-high-high -2.837609  -6.819567  1.1443486 0.2350178
# low-high-high-high  1.346267  -4.626669  7.3192040 0.9275744
# low-low-high-high  -6.733189 -10.867767 -2.5986105 0.0006204
# low-high-high-low   4.183877  -2.112152 10.4799047 0.2909080
# low-low-high-low   -3.895579  -8.484559  0.6934002 0.1190065
# low-low-low-high   -8.079456 -14.473103 -1.6858092 0.0088981
ggplot(tempdf,aes(x=g6,y=outcomeMeasures, color=mslaborLand))+geom_point()

#########################################################################
## do comparisons within subgroups of msactiveLabor
#########################################################################
ggplot(postDf,aes(x=msactiveLabor, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov7 = aov(outcomeMeasures~g7,data=tempdf)
summary(aov7)
# summary(aov7)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# g7           3  268.6   89.55    7.21 0.000831 ***
# Residuals   31  385.0   12.42                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
TukeyHSD(aov7)
# TukeyHSD(aov7)
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = outcomeMeasures ~ g7, data = tempdf)
# 
# $g7
#                         diff        lwr       upr     p adj
# high-low-high-high -7.411562 -12.350867 -2.472257 0.0016177
# low-high-high-high -1.664647  -6.447109  3.117814 0.7811549
# low-low-high-high  -3.995543  -9.316975  1.325889 0.1963131
# low-high-high-low   5.746915   1.651459  9.842370 0.0033009
# low-low-high-low    3.416019  -1.297627  8.129664 0.2222280
# low-low-low-high   -2.330896  -6.879924  2.218133 0.5144675
ggplot(tempdf,aes(x=g7,y=outcomeMeasures, color=mslaborLand))+geom_point()

#########################################################################
## do comparisons within subgroups of P027 (birthplan)
#########################################################################
ggplot(postDf,aes(x=P027, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov8 = aov(outcomeMeasures~g8,data=tempdf)
summary(aov8)
# summary(aov8)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
# g8           3  247.2   82.40   6.284 0.00186 **
# Residuals   31  406.5   13.11                   
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
TukeyHSD(aov8)
# TukeyHSD(aov8)
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = outcomeMeasures ~ g8, data = tempdf)
# 
# $g8
#                       diff         lwr        upr     p adj
# NO-low-NO-high   -3.407843  -7.6158493  0.8001633 0.1461933
# YES-high-NO-high  1.487897  -3.1738306  6.1496252 0.8220735
# YES-low-NO-high  -6.216482 -11.5993818 -0.8335823 0.0186429
# YES-high-NO-low   4.895740   0.4099905  9.3814901 0.0282233
# YES-low-NO-low   -2.808639  -8.0398773  2.4225992 0.4747086
# YES-low-YES-high -7.704379 -13.3070791 -2.1016797 0.0040442
ggplot(tempdf,aes(x=g8,y=outcomeMeasures, color=mslaborLand))+geom_point()

#########################################################################
## do comparisons within subgroups of P051 (people present)
#########################################################################
ggplot(postDf,aes(x=P051, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov9 = aov(outcomeMeasures~g9,data=tempdf)
summary(aov9)
# summary(aov9)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# g9           3  269.8   89.93   7.263 0.000795 ***
# Residuals   31  383.9   12.38                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
TukeyHSD(aov9)
# TukeyHSD(aov9)
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = outcomeMeasures ~ g9, data = tempdf)
# 
# $g9
#                       diff         lwr       upr     p adj
# NO-low-NO-high   -1.878113  -6.9830838  3.226857 0.7513460
# YES-high-NO-high  3.421866  -1.1957532  8.039486 0.2057517
# YES-low-NO-high  -3.452224  -8.1587737  1.254327 0.2132538
# YES-high-NO-low   5.299980   0.6823601  9.917599 0.0195390
# YES-low-NO-low   -1.574110  -6.2806604  3.132440 0.8008494
# YES-low-YES-high -6.874090 -11.0470146 -2.701165 0.0005381
ggplot(tempdf,aes(x=g9,y=outcomeMeasures, color=mslaborLand))+geom_point()

#########################################################################
## do comparisons within subgroups of msactiveFeel
#########################################################################
ggplot(postDf,aes(x=msactiveFeel, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov10 = aov(outcomeMeasures~g10,data=tempdf)
summary(aov10)
# summary(aov10)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# g10          3  289.8   96.61   8.231 0.000357 ***
# Residuals   31  363.8   11.74                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
TukeyHSD(aov10)
# TukeyHSD(aov10)
#   Tukey multiple comparisons of means
#     95% family-wise confidence level
# 
# Fit: aov(formula = outcomeMeasures ~ g10, data = tempdf)
# 
# $g10
#                         diff         lwr        upr     p adj
# high-low-high-high -8.083920 -12.8961251 -3.2717157 0.0004203
# low-high-high-high -1.280581  -5.6910390  3.1298779 0.8593121
# low-low-high-high  -3.872535  -8.2829930  0.5379239 0.1015406
# low-high-high-low   6.803340   2.2212045 11.3854753 0.0018178
# low-low-high-low    4.211386  -0.3707495  8.7935213 0.0806122
# low-low-low-high   -2.591954  -6.7501742  1.5662661 0.3450172
ggplot(tempdf,aes(x=g10,y=outcomeMeasures, color=mslaborLand))+geom_point()

