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


ggplot(postDf,aes(x=mslaborLand, y=outcomeMeasures, color=mspainExp)) + 
		facet_wrap(~msexpectations) +
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=mslaborLand, y=outcomeMeasures, color=msexpectations)) + 
		facet_wrap(~mspainExp) +
		stat_summary(fun.data = "mean_cl_boot")

#########################################################################
## check what helps you get into laborLand if you have low painExp
#########################################################################
subpaindf=subset(postDf,mspainExp=="low")
loopvars = postNumVarNames
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
loopvars = postNumVarNames[!postNumVarNames%in%laborLandNames]
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
result = lapply(loopvars,function(vname) {
			df=subpaindf[,c(vname,"mslaborLand")]
			names(df)[1]="y"
			aov1=aov(y~mslaborLand,data=df)
			data.frame(var=vname,pval=unlist(summary(aov1))[9])
		})
rdf = ldply(result)
names(rdf)
rdf = rdf[order(rdf$pval),]
head(rdf)
# head(rdf)
#      var        pval
# 110 P129 0.006231875   My labor pain was productive
# 92  P104 0.008572385   My sense of self dissolved
# 115 P138 0.016285973   I felt connected to all the women who have labored and birthed before me
# 79  P087 0.024431748   I felt uninhibited
# 101 P116 0.037770051
# 100 P114 0.038924128

ggplot(postDf,aes(x=P129,y=laborLand))+facet_wrap(~mspainExp)+
		geom_point()+geom_smooth(method=lm)
ggplot(postDf,aes(x=P104,y=laborLand))+facet_wrap(~mspainExp)+
		geom_point()+geom_smooth(method=lm)
ggplot(postDf,aes(x=P138,y=laborLand))+facet_wrap(~mspainExp)+
		geom_point()+geom_smooth(method=lm)
ggplot(postDf,aes(x=P087,y=laborLand))+facet_wrap(~mspainExp)+
		geom_point()+geom_smooth(method=lm)

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
# > cat("Synch1332298962181572000\n");

## now get the parts of laborland and outcome that aren't explained
## by pain
resdf = data.frame(laborres=lmb$residual,outres = lma$residual)
head(resdf)
lmc = lm(outres ~laborres,data=resdf)
summary(lmc)$coef
# summary(lmc)$coef
#                 Estimate Std. Error      t value   Pr(>|t|)
# (Intercept) 1.501296e-16 0.44305319 3.388522e-16 1.00000000
# laborres    1.500193e-01 0.05610478 2.673914e+00 0.01156778

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







