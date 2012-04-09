###############################################################################
## multipleRegs.R
##
## Test different regression models
## 
## Author: Haaland
###############################################################################
library(ggplot2)
library(psych)
library(GPArotation)
library(Hmisc)
load("rdata/postData.RData")

#########################################################################
## Test different regression models with dv: outcomeMeasures
#########################################################################
fit1 <- lm(outcomeMeasures ~ painExp + laborLand + P027, data=postDf)
summary(fit1)
# summary(fit1)
# 
# Call:
# lm(formula = outcomeMeasures ~ painExp + laborLand + panas, data = postDf)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5.7782 -0.9360  0.0621  1.7008  5.0374 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -6.122e-16  4.506e-01   0.000   1.0000    
# painExp      4.370e-01  9.473e-02   4.613  6.5e-05 ***
# laborLand    1.359e-01  5.895e-02   2.305   0.0281 *  
# panas        5.022e-02  5.263e-02   0.954   0.3474    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Residual standard error: 2.666 on 31 degrees of freedom
# Multiple R-squared: 0.663,	Adjusted R-squared: 0.6304 
# F-statistic: 20.33 on 3 and 31 DF,  p-value: 1.789e-07 
# 


#########################################################################
## stepwise regression
#########################################################################
summary(lm1 <- lm(outcomeMeasures ~ laborLand + panas + painExp + 
						expectations + P006 + 
						tActualAct + P027 + P051 + tPerceivedAct, data = postDf))
#Call:
#lm(formula = outcomeMeasures ~ laborLand + panas + painExp + 
#				expectations + P006 + tActualAct + P027 + P051 + tPerceivedAct, 
#				data = postDf)
#
#Residuals:
#	Min      1Q  Median      3Q     Max 
#-5.2141 -1.1924  0.0773  2.1372  3.9964 
#
#Coefficients:
#				Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   -5.349053   5.436120  -0.984  0.33455   
#laborLand      0.131377   0.068016   1.932  0.06483 . 
#panas          0.055713   0.064567   0.863  0.39641   
#painExp        0.398170   0.116152   3.428  0.00211 **
#expectations   0.087443   0.187752   0.466  0.64544   
#P006           0.544574   0.551428   0.988  0.33283   
#tActualAct    -0.005641   0.266206  -0.021  0.98326   
#P027           0.507098   1.209179   0.419  0.67852   
#P051           0.945231   1.307712   0.723  0.47650   
#tPerceivedAct  0.106321   0.254123   0.418  0.67924   
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#Residual standard error: 2.868 on 25 degrees of freedom
#Multiple R-squared: 0.6854,	Adjusted R-squared: 0.5721 
#F-statistic: 6.052 on 9 and 25 DF,  p-value: 0.0001668 

slm1 <- step(lm1,direction = "both")
summary(slm1)
# summary(slm1)
# 
# Call:
# lm(formula = outcomeMeasures ~ laborLand + painExp, data = postDf)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5.8214 -0.9958  0.1966  1.4841  5.7410 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -6.191e-16  4.499e-01   0.000   1.0000    
# laborLand    1.500e-01  5.697e-02   2.633   0.0129 *  
# painExp      4.547e-01  9.277e-02   4.901 2.65e-05 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Residual standard error: 2.662 on 32 degrees of freedom
# Multiple R-squared: 0.6531,	Adjusted R-squared: 0.6315 
# F-statistic: 30.13 on 2 and 32 DF,  p-value: 4.388e-08 

slm1$anova
# slm1$anova
#              Step Df    Deviance Resid. Df Resid. Dev      AIC
# 1                 NA          NA        25   205.6431 81.97779
# 2    - tActualAct  1 0.003693231        26   205.6468 79.97842
# 3          - P027  1 1.444801151        27   207.0916 78.22346
# 4  - expectations  1 1.329122782        28   208.4207 76.44737
# 5 - tPerceivedAct  1 2.727016441        29   211.1477 74.90235
# 6          - P051  1 6.443944950        30   217.5917 73.95453
# 7          - P006  1 2.661441387        31   220.2531 72.38003
# 8         - panas  1 6.468920063        32   226.7220 71.39318

fitmodel <- lm(outcomeMeasures ~ painExp + laborLand, data=postDf)
summary(fitmodel)
# summary(fitmodel)
# 
# Call:
# lm(formula = outcomeMeasures ~ painExp + laborLand, data = postDf)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5.8214 -0.9958  0.1966  1.4841  5.7410 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -6.191e-16  4.499e-01   0.000   1.0000    
# painExp      4.547e-01  9.277e-02   4.901 2.65e-05 ***
# laborLand    1.500e-01  5.697e-02   2.633   0.0129 *  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Residual standard error: 2.662 on 32 degrees of freedom
# Multiple R-squared: 0.6531,	Adjusted R-squared: 0.6315 
# F-statistic: 30.13 on 2 and 32 DF,  p-value: 4.388e-08 
print(fitmodel)
# print(fitmodel)
# 
# Call:
# lm(formula = outcomeMeasures ~ painExp + laborLand, data = postDf)
# 
# Coefficients:
# (Intercept)      painExp    laborLand  
#  -6.191e-16    4.547e-01    1.500e-01  



ggplot(postDf,aes(x=mslaborLand, y=outcomeMeasures, color=mspainExp)) + 
		facet_wrap(~msexpectations) +
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=mslaborLand, y=outcomeMeasures, color=msexpectations)) + 
		facet_wrap(~mspainExp) +
		stat_summary(fun.data = "mean_cl_boot")


#########################################################################
## version #2
#########################################################################
summary(lm1 <- lm(outcomeMeasures ~ laborLand + panas + intuitMov + 
						physEnv + emotEnv + fluidReal + intensePres +
						painExp + expectations + memory + vocals + 
						P006 + P009 + P021 +
						tActualAct + P027 + P051 + tPerceivedAct, data = postDf))
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-3.0658 -1.5006 -0.1622  1.2315  5.4280 
#
#Coefficients:
#				Estimate Std. Error t value Pr(>|t|)  
#(Intercept)   -1.427729   9.392779  -0.152   0.8811  
#laborLand      0.064298   0.097701   0.658   0.5198  
#panas         -0.005773   0.075787  -0.076   0.9402  
#intuitMov      0.530903   0.390234   1.360   0.1925  
#physEnv        0.302626   0.419246   0.722   0.4808  
#emotEnv       -0.099652   0.223334  -0.446   0.6614  
#fluidReal      0.132463   0.236612   0.560   0.5833  
#intensePres    0.101641   0.162912   0.624   0.5415  
#painExp        0.361209   0.147607   2.447   0.0263 *
#expectations  -0.036364   0.229713  -0.158   0.8762  
#memory        -0.255681   0.198298  -1.289   0.2156  
#vocals         0.038483   0.341053   0.113   0.9116  
#P006           0.485612   0.746524   0.650   0.5246  
#P009          -1.075617   0.922466  -1.166   0.2607  
#P021           0.156894   0.378312   0.415   0.6839  
#tActualAct    -0.194681   0.318464  -0.611   0.5496  
#P027           0.672219   1.504543   0.447   0.6610  
#P051           0.030890   1.984731   0.016   0.9878  
#tPerceivedAct  0.309915   0.300265   1.032   0.3174  
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#Residual standard error: 2.808 on 16 degrees of freedom
#Multiple R-squared: 0.807,	Adjusted R-squared: 0.5899 
#F-statistic: 3.717 on 18 and 16 DF,  p-value: 0.005583 
slm1 <- step(lm1,direction = "both")
summary(slm1)
# summary(slm1)
# 
# Call:
# lm(formula = outcomeMeasures ~ laborLand + intuitMov + physEnv + 
#     painExp + memory + P006 + P009 + tPerceivedAct, data = postDf)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.5754 -1.3957  0.1725  1.2876  4.8806 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -1.13590    2.20691  -0.515 0.611111    
# laborLand      0.10616    0.05781   1.837 0.077741 .  
# intuitMov      0.34085    0.19293   1.767 0.089016 .  
# physEnv        0.32095    0.17000   1.888 0.070242 .  
# painExp        0.37543    0.09096   4.127 0.000335 ***
# memory        -0.18558    0.13888  -1.336 0.193020    
# P006           0.55689    0.38681   1.440 0.161883    
# P009          -0.81278    0.53889  -1.508 0.143544    
# tPerceivedAct  0.15935    0.10888   1.464 0.155314    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Residual standard error: 2.332 on 26 degrees of freedom
# Multiple R-squared: 0.7837,	Adjusted R-squared: 0.7171 
# F-statistic: 11.77 on 8 and 26 DF,  p-value: 6.495e-07 

slm1$anova
# slm1$anova
#              Step Df    Deviance Resid. Df Resid. Dev      AIC
# 1                 NA          NA        16   126.1461 82.87325
# 2          - P051  1 0.001909759        17   126.1480 80.87378
# 3         - panas  1 0.053047059        18   126.2011 78.88850
# 4        - vocals  1 0.129607598        19   126.3307 76.92442
# 5  - expectations  1 0.319451200        20   126.6501 75.01282
# 6          - P021  1 1.850833999        21   128.5010 73.52060
# 7       - emotEnv  1 1.925311878        22   130.4263 72.04111
# 8    - tActualAct  1 1.720660459        23   132.1469 70.49983
# 9     - fluidReal  1 1.677594119        24   133.8245 68.94135
# 10         - P027  1 2.543307538        25   136.3679 67.60028
# 11  - intensePres  1 5.027023445        26   141.3949 66.86730


#########################################################################
## try on option by hand
#########################################################################
fit1 <- lm(outcomeMeasures ~ painExp + laborLand + physEnv + intuitMov, data=postDf)
summary(fit1)
# summary(fit1)
# 
# Call:
# lm(formula = outcomeMeasures ~ painExp + laborLand + physEnv + 
#     intuitMov, data = postDf)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -4.5117 -1.2654  0.0904  1.5107  4.8039 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -7.161e-16  3.998e-01   0.000 1.000000    
# painExp      3.513e-01  8.864e-02   3.963 0.000423 ***
# laborLand    1.006e-01  5.792e-02   1.736 0.092762 .  
# physEnv      2.406e-01  1.683e-01   1.429 0.163320    
# intuitMov    3.215e-01  1.943e-01   1.655 0.108400    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Residual standard error: 2.365 on 30 degrees of freedom
# Multiple R-squared: 0.7432,	Adjusted R-squared: 0.709 
# F-statistic: 21.71 on 4 and 30 DF,  p-value: 1.691e-08 

## It looks like this secondary option ultimately gets me to the 
## same place - BUT, I get a larger Rsqrd if I include physEnv
## and intuitMov...is this helpful? Or should I stick with what
## I had before??


#########################################################################
## check what helps you get into laborLand if you have low painExp
#########################################################################
subpaindf=subset(postDf,mspainExp=="low")
loopvars = postNumVarNames[45:153]
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=subpaindf[,c(loopvars[i],"mslaborLand")]
	names(df)[1]="y"
	aov1=aov(y~mslaborLand,data=df)
	print(summary(aov1))
}
length(loopvars)
# length(loopvars)
# [1] 153
laborLandNames = c("P080","P085","P086","P089","P093",
		"P094","P101","P107","P108","P115","P123","P127","P130",
		"P136")
length(laborLandNames)
# length(laborLandNames)
# [1] 14
loopvars = loopvars[!loopvars%in%laborLandNames]
length(loopvars)
# length(loopvars)
# [1] 139
painExpNames = c("P124","P131","P133","P142","P145",
		"P148","P155","P156")
length(painExpNames)
# length(painExpNames)
# [1] 8
loopvars = loopvars[!loopvars%in%painExpNames]
length(loopvars)
# length(loopvars)
# [1] 131
outcomeMeasuresNames = c("P065","P066","P122","P149","P150","P151")
length(outcomeMeasuresNames)
# length(outcomeMeasuresNames)
# [1] 6
loopvars = loopvars[!loopvars%in%outcomeMeasuresNames]
length(loopvars)
# length(loopvars)
# [1] 125
#we decided to do this with only the themes
subpaindf=subset(postDf,mspainExp=="low")
themevars= c("intuitMov","physEnv","emotEnv","fluidReal","intensePres",
		"expectations","memory","vocals","panas")
result = lapply(themevars,function(vname) {
			df=subpaindf[,c(vname,"mslaborLand")]
			names(df)[1]="y"
			aov1=aov(y~mslaborLand,data=df)
			data.frame(var=vname,pval=unlist(summary(aov1))[9])
		})
rdf = ldply(result)
names(rdf)
rdf = rdf[order(rdf$pval),]
rdf$pval.adj=p.adjust(rdf$pval,method="holm")
head(rdf)
# head(rdf)
#      var        pval  pval.adj
# 110 P129 0.006231875 0.7789844
# 92  P104 0.008572385 1.0000000
# 115 P138 0.016285973 1.0000000
# 79  P087 0.024431748 1.0000000
# 101 P116 0.037770051 1.0000000
# 100 P114 0.038924128 1.0000000

# head(rdf)
#      var        pval
# 110 P129 0.006231875   My labor pain was productive
# 92  P104 0.008572385   My sense of self dissolved
# 115 P138 0.016285973   I felt connected to all the women who have labored and birthed before me
# 79  P087 0.024431748   I felt uninhibited
# 101 P116 0.037770051	 My "mental chatter" disappeared
# 100 P114 0.038924128	 INV - I let myself down











ggplot(postDf,aes(x=P129,y=laborLand))+facet_wrap(~mspainExp)+
		geom_point()+geom_smooth(method=lm)
t.test(P129~mslaborLand, var.equal = TRUE, data=postDf)
# t.test(P129~mslaborLand, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  P129 by mslaborLand 
# t = -2.5448, df = 33, p-value = 0.0158
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -1.4348795 -0.1598917 
# sample estimates:
#  mean in group low mean in group high 
#           3.647059           4.444444 
ddply(postDf,.(mslaborLand),function(df){
			data.frame(mean = mean(df$P129),
					sd=sd(df$P129))
		})
#  mslaborLand     mean        sd
#1         low 3.647059 1.1147408
#2        high 4.444444 0.7047922


ggplot(postDf,aes(x=P104,y=laborLand))+facet_wrap(~mspainExp)+
		geom_point()+geom_smooth(method=lm)
t.test(P104~mslaborLand, var.equal = TRUE, data=postDf)
# t.test(P104~mslaborLand, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  P104 by mslaborLand 
# t = 1.1954, df = 33, p-value = 0.2404
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -0.3578275  1.3774353 
# sample estimates:
#  mean in group low mean in group high 
#           3.176471           2.666667 
ddply(postDf,.(mslaborLand),function(df){
			data.frame(mean = mean(df$P104),
					sd=sd(df$P104))
		})
#  mslaborLand     mean       sd
#1         low 3.176471 1.014599
#2        high 2.666667 1.455214

ggplot(postDf,aes(x=P138,y=laborLand))+facet_wrap(~mspainExp)+
		geom_point()+geom_smooth(method=lm)
t.test(P138~mslaborLand, var.equal = TRUE, data=postDf)
# t.test(P138~mslaborLand, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  P138 by mslaborLand 
# t = -1.8723, df = 33, p-value = 0.07006
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -1.80708121  0.07505506 
# sample estimates:
#  mean in group low mean in group high 
#           2.411765           3.277778 
ddply(postDf,.(mslaborLand),function(df){
			data.frame(mean = mean(df$P138),
					sd=sd(df$P138))
		})
#  mslaborLand     mean       sd
#1         low 2.411765 1.227743
#2        high 3.277778 1.487420


ggplot(postDf,aes(x=P087,y=laborLand))+facet_wrap(~mspainExp)+
		geom_point()+geom_smooth(method=lm)
t.test(P087~mslaborLand, var.equal = TRUE, data=postDf)
# t.test(P087~mslaborLand, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  P087 by mslaborLand 
# t = -4.3981, df = 33, p-value = 0.0001073
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -1.395671 -0.512826 
# sample estimates:
#  mean in group low mean in group high 
#           3.823529           4.777778 
ddply(postDf,.(mslaborLand),function(df){
			data.frame(mean = mean(df$P087),
					sd=sd(df$P087))
		})
#  mslaborLand     mean        sd
#1         low 3.823529 0.8089572
#2        high 4.777778 0.4277926

ggplot(postDf,aes(x=P116,y=laborLand))+facet_wrap(~mspainExp)+
		geom_point()+geom_smooth(method=lm)
t.test(P116~mslaborLand, var.equal = TRUE, data=postDf)
# t.test(P116~mslaborLand, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  P116 by mslaborLand 
# t = -2.192, df = 33, p-value = 0.03554
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -1.48077949 -0.05516822 
# sample estimates:
#  mean in group low mean in group high 
#           3.176471           3.944444 
ddply(postDf,.(mslaborLand),function(df){
			data.frame(mean = mean(df$P116),
					sd=sd(df$P116))
		})
#  mslaborLand     mean        sd
#1         low 3.176471 1.1850788
#2        high 3.944444 0.8726041


ggplot(postDf,aes(x=P114,y=laborLand))+facet_wrap(~mspainExp)+
		geom_point()+geom_smooth(method=lm)
t.test(P114~mslaborLand, var.equal = TRUE, data=postDf)
# t.test(P114~mslaborLand, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  P114 by mslaborLand 
# t = -3.2097, df = 33, p-value = 0.002957
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -1.9488976 -0.4367233 
# sample estimates:
#  mean in group low mean in group high 
#           3.529412           4.722222 
ddply(postDf,.(mslaborLand),function(df){
			data.frame(mean = mean(df$P114),
					sd=sd(df$P114))
		})
#  mslaborLand     mean        sd
#1         low 3.529412 1.5048940
#2        high 4.722222 0.4608886

#########################################################################
## check what helps you get into laborLand if you have low expectations
#########################################################################
subexpdf=subset(postDf,msexpectations=="low")
loopvars = postNumVarNames
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=subexpdf[,c(loopvars[i],"mslaborLand")]
	names(df)[1]="y"
	aov2=aov(y~mslaborLand,data=df)
	print(summary(aov2))
}
length(loopvars)
# length(loopvars)
# [1] 153
laborLandNames = c("P080","P085","P086","P089","P093",
		"P094","P101","P107","P108","P115","P123","P127","P130",
		"P136")
length(laborLandNames)
# length(laborLandNames)
# [1] 14
loopvars = postNumVarNames[!postNumVarNames%in%laborLandNames]
length(loopvars)
# length(loopvars)
# [1] 139
expectationsNames = c("P067","P081","P084","P088","P091")
length(expectationsNames)
# length(expectationsNames)
# [1] 5
loopvars = loopvars[!loopvars%in%expectationsNames]
length(loopvars)
# length(loopvars)
# [1] 134
outcomeMeasuresNames = c("P065","P066","P122","P149","P150","P151")
length(outcomeMeasuresNames)
# length(outcomeMeasuresNames)
# [1] 6
loopvars = loopvars[!loopvars%in%outcomeMeasuresNames]
length(loopvars)
# length(loopvars)
# [1] 128
result = lapply(loopvars,function(vname) {
			df=subexpdf[,c(vname,"mslaborLand")]
			names(df)[1]="y"
			aov2=aov(y~mslaborLand,data=df)
			data.frame(var=vname,pval=unlist(summary(aov2))[9])
		})
rdf = ldply(result)
names(rdf)
rdf = rdf[order(rdf$pval),]
head(rdf)
# head(rdf)
#      var       pval
# 102 P124 0.03151799   INV - My labor pain felt like suffering
# 76  P087 0.03461142   I felt uninhibited
# 95  P114 0.04327635   INV - I let myself down
# 4   P006 0.04421526   Highest educational level achieved
# 126 P154 0.08726351
# 84  P100 0.09095422   

ggplot(postDf,aes(x=P124,y=laborLand))+facet_wrap(~msexpectations)+
		geom_point()+geom_smooth(method=lm)
## P087 is an element of intuitMov
ggplot(postDf,aes(x=P087,y=laborLand))+facet_wrap(~msexpectations)+
		geom_point()+geom_smooth(method=lm)+
		opts(title="I felt uninhibited")
ggplot(postDf,aes(x=P114,y=laborLand))+facet_wrap(~msexpectations)+
		geom_point()+geom_smooth(method=lm)
ggplot(postDf,aes(x=P006,y=laborLand))+facet_wrap(~msexpectations)+
		geom_point()+geom_smooth(method=lm)

ggplot(postDf,aes(x=painExp,y=panas))+geom_point()


#########################################################################
## partial correlation analysis
#########################################################################

## this is the biggest finding
## pain is the biggest determiner of outcomes
lma = lm(outcomeMeasures~painExp,data=postDf)
summary(lma)
# painExp      5.805e-01  8.635e-02   6.723 1.17e-07 ***

ggplot(postDf,aes(x=painExp,y=outcomeMeasures)) + geom_point() +
		geom_smooth(method=lm)
## these are clearly correlated so you have to take this out
ggplot(postDf,aes(x=painExp,y=laborLand)) + geom_point() +
		geom_smooth(method=lm)

## also remove the effect of pain from laborland
lmb = lm(laborLand~painExp,data=postDf)
summary(lmb)$coef
# summary(lmb)$coef
#                 Estimate Std. Error      t value    Pr(>|t|)
# (Intercept) 1.428818e-15  1.3746719 1.039388e-15 1.000000000
# painExp     8.390771e-01  0.2428971 3.454455e+00 0.001534314

## now get the parts of laborland and outcome that aren't explained
## by pain
## "partial correlation"
resdf = data.frame(laborres=lmb$residual,outres = lma$residual)
head(resdf)
lmc = lm(outres ~laborres,data=resdf)
summary(lmc)$coef
# summary(lmc)$coef
#                 Estimate Std. Error      t value   Pr(>|t|)
# (Intercept) 1.501296e-16 0.44305319 3.388522e-16 1.00000000
# laborres    1.500193e-01 0.05610478 2.673914e+00 0.01156778


groups1Df = data.frame(laborres = resdf$laborres, outres = resdf$outres)
grp1corrtest = corr.test(groups1Df)
# correlations, unadjusted p and adjusted p
grp1cres = data.frame(round(grp1corrtest$r[,2],3),
		round(grp1corrtest$p[2,],4),
		round(grp1corrtest$p[,2],4))
names(grp1cres) = c("cor.","p-raw","p-adjusted")
grp1cres
# grp1cres
#           cor.  p-raw p-adjusted
# laborres 0.422 0.0116     0.0116
# outres   1.000 0.0000     0.0000

## after you take away the parts of outcome and laborland explained by 
## pain, more laborland is still better
ggplot(resdf,aes(x=laborres,y=outres)) + geom_point()

summary(lm(outcomeMeasures~painExp+laborLand,data=postDf))$coef
summary(lm(outcomeMeasures~painExp+laborLand+panas,data=postDf))$coef

## this code isn't working...
notvars = postNumVarNames[!postNumVarNames %in% c(laborLandNames,
				painExpNames,outcomeMeasuresNames)]
regdf = postDf[,c("outcomeMeasures","laborLand","painExp",notvars)]
g1 = lm(outcomeMeasures~.,data=regdf)
add1(g1,~painExp+laborLand+.)
sg1 = step(g1,direction="both",scope=list(lower=(outcomeMeasures~painExp+laborLand)))
sg1$anova

#########################################################################
## create median split variable for the laborLand residuals
#########################################################################
postDf$laborres = lmb$residual
llresindex = postDf$laborres>=median(postDf$laborres)
# selects TRUE values
postDf$laborres[llresindex]
postDf$msreslaborLand = "low"
postDf$msreslaborLand[llresindex] = "high"
postDf$msreslaborLand = factor(postDf$msreslaborLand,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=msreslaborLand))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high laborLand
ggplot(postDf,aes(x=msreslaborLand, y=outcomeMeasures))+geom_boxplot()

postDf$outres = lma$residual
## this plot doesn't actually show the same pattern --> what does that mean??
## does this say that the pattern observed is driven only by painExp
## (b/c it goes away when painExp is taken out of laborLand??)
ggplot(postDf,aes(x=mspainExp,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=mspanas,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")
ggplot(postDf,aes(x=msexpectations,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")
ggplot(postDf,aes(x=educationsplit,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")
ggplot(postDf,aes(x=msactiveLabor,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")
### possibly?
ggplot(postDf,aes(x=P027,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")
ggplot(postDf,aes(x=P051,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")
ggplot(postDf,aes(x=msactiveFeel,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")







