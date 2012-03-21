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
						expectations + fluidReal + intensePres + P006 + 
						tActualAct + P027 + P051 + tPerceivedAct, data = postDf))
#Call:
#		lm(formula = outcomeMeasures ~ laborLand + panas + painExp + 
#						expectations + fluidReal + intensePres + P006 + tActualAct + 
#						P027 + P051 + tPerceivedAct, data = postDf)
#
#Residuals:
#		Min      1Q  Median      3Q     Max 
#-5.2638 -1.2019  0.0037  1.6547  3.5806 
#
#Coefficients:
#		Estimate Std. Error t value Pr(>|t|)  
#(Intercept)   -3.36404    5.70114  -0.590    0.561  
#laborLand      0.11653    0.07066   1.649    0.113  
#panas          0.04106    0.06467   0.635    0.532  
#painExp        0.33184    0.12767   2.599    0.016 *
#expectations   0.46421    0.32722   1.419    0.169  
#fluidReal      0.13718    0.14199   0.966    0.344  
#intensePres   -0.02924    0.12283  -0.238    0.814  
#P006           0.34286    0.58221   0.589    0.562  
#tActualAct    -0.03960    0.26755  -0.148    0.884  
#P027           0.64642    1.21413   0.532    0.600  
#P051          -0.02037    1.42755  -0.014    0.989  
#tPerceivedAct  0.20327    0.26128   0.778    0.445  
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#
#Residual standard error: 2.838 on 23 degrees of freedom
#Multiple R-squared: 0.7166,	Adjusted R-squared: 0.5811 
#F-statistic: 5.287 on 11 and 23 DF,  p-value: 0.0003808 

slm1 <- step(lm1,direction = "both")
summary(slm1)
# summary(slm1)
# 
# Call:
# lm(formula = outcomeMeasures ~ laborLand + painExp + expectations, 
#     data = postDf)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5.6298 -1.0116  0.4798  1.6262  4.4504 
# 
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -6.657e-16  4.402e-01   0.000 1.000000    
# laborLand     1.384e-01  5.624e-02   2.461 0.019638 *  
# painExp       3.811e-01  1.023e-01   3.726 0.000777 ***
# expectations  3.432e-01  2.201e-01   1.559 0.129124    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Residual standard error: 2.604 on 31 degrees of freedom
# Multiple R-squared: 0.6784,	Adjusted R-squared: 0.6472 
# F-statistic: 21.79 on 3 and 31 DF,  p-value: 8.786e-08 
# 

slm1$anova
# slm1$anova
#              Step Df    Deviance Resid. Df Resid. Dev      AIC
# 1                 NA          NA        23   185.2487 82.32230
# 2          - P051  1 0.001640397        24   185.2504 80.32261
# 3    - tActualAct  1 0.181072874        25   185.4314 78.35680
# 4   - intensePres  1 0.444677151        26   185.8761 76.44063
# 5          - P027  1 2.730326634        27   188.6064 74.95101
# 6          - P006  1 2.496657882        28   191.1031 73.41127
# 7         - panas  1 4.819083230        29   195.9222 72.28293
# 8 - tPerceivedAct  1 6.652404256        30   202.5746 71.45160
# 9     - fluidReal  1 7.662210488        31   210.2368 70.75103


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
subpaindf=subset(postDf,msexpectations=="low")
loopvars = postNumVarNames
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=subpaindf[,c(loopvars[i],"mslaborLand")]
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
expectationsNames = c("P067","P084","P088")
length(expectationsNames)
# length(expectationsNames)
# [1] 3
loopvars = loopvars[!loopvars%in%expectationsNames]
length(loopvars)
# length(loopvars)
# [1] 136
outcomeMeasuresNames = c("P065","P066","P122","P149","P150","P151")
length(outcomeMeasuresNames)
# length(outcomeMeasuresNames)
# [1] 6
loopvars = loopvars[!loopvars%in%outcomeMeasuresNames]
length(loopvars)
# length(loopvars)
# [1] 130
result = lapply(loopvars,function(vname) {
			df=subpaindf[,c(vname,"mslaborLand")]
			names(df)[1]="y"
			aov2=aov(y~mslaborLand,data=df)
			data.frame(var=vname,pval=unlist(summary(aov2))[9])
		})
rdf = ldply(result)
names(rdf)
rdf = rdf[order(rdf$pval),]
head(rdf)
# head(rdf)
#      var        pval
# 77  P087 0.001705419   I felt uninhibited
# 104 P124 0.011356421   INV - My labor pain felt like suffering
# 86  P100 0.021118981   I believe that my body holds the wisdom to give birth on its own
# 97  P114 0.025358851   INV - I let myself down
# 122 P145 0.029284741
# 119 P142 0.035264023

## P087 is an element of intuitMov
ggplot(postDf,aes(x=P087,y=laborLand))+facet_wrap(~msexpectations)+
		geom_point()+geom_smooth(method=lm)+
		opts(title="I felt uninhibited")
ggplot(postDf,aes(x=P124,y=laborLand))+facet_wrap(~msexpectations)+
		geom_point()+geom_smooth(method=lm)
ggplot(postDf,aes(x=P100,y=laborLand))+facet_wrap(~msexpectations)+
		geom_point()+geom_smooth(method=lm)
ggplot(postDf,aes(x=P114,y=laborLand))+facet_wrap(~msexpectations)+
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
ggplot(postDf,aes(x=mspainExp,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=mspanas,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")
ggplot(postDf,aes(x=msexpectations,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")
ggplot(postDf,aes(x=msfluidReal,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")
### possibly?
ggplot(postDf,aes(x=msintensePres,y=outres, color=msreslaborLand))+
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
### possibly?
ggplot(postDf,aes(x=msactiveFeel,y=outres, color=msreslaborLand))+
		stat_summary(fun.data = "mean_cl_boot")







