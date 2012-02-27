###############################################################################
## medianSplits.R
##
## Create median splits for important variables
## 
## Author: Haaland
###############################################################################
library(ggplot2)
library(psych)
library(GPArotation)
library(Hmisc)
load("rdata/postData.RData")

#########################################################################
## create a new variable based on median split of laborLand
#########################################################################
llindex = postDf$laborLand>=median(postDf$laborLand)
# selects TRUE values
postDf$laborLand[llindex]
postDf$mslaborLand = "low"
postDf$mslaborLand[llindex] = "high"
postDf$mslaborLand = factor(postDf$mslaborLand,levels = c("low","high"))
ggplot(postDf,aes(x=mslaborLand))+geom_histogram()
ggplot(postDf,aes(x=mslaborLand, y=outcomeMeasures))+geom_boxplot()
#########################################################################
## analysis of variance based on categories of mslaborLand
#########################################################################
aov1 = aov(outcomeMeasures~mslaborLand,data=postDf)
summary(aov1)
# summary(aov1)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# mslaborLand  1  203.5  203.49   14.92 0.000496 ***
# Residuals   33  450.2   13.64                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
plot(aov1)

g1=ggplot(postDf,aes(x=mslaborLand, y=outcomeMeasures))
stat_sum_df <- function(fun, geom="crossbar", ...) { 
  stat_summary(fun.data=fun, colour="red", geom=geom, width=0.2, ...) 
}
# means plus 95% confidence interval
g1 + stat_sum_df("mean_cl_normal", geom = "errorbar") + 
		stat_sum_df("mean_cl_normal", geom = "point") 


#########################################################################
## create a new variable based on median split of panas
#########################################################################
panas
panasbreaks = c(min(panas)-.5,quantile(panas, probs = c(.33,.67)),
		max(panas)+.5)
panasbreaks
lmhpanas = cut(panas, breaks=panasbreaks,labels=c("low","medium","high"))
table(lmhpanas)
# table(panaslmh)
# panaslmh
#    low medium   high 
#     12     11     12 
postDf$lmhpanas = lmhpanas
ggplot(postDf,aes(x=lmhpanas))+geom_histogram()
ggplot(postDf,aes(x=lmhpanas, y=laborLand))+geom_boxplot()
ggplot(postDf,aes(x=lmhpanas, color=mslaborLand))+geom_histogram()

#########################################################################
## 2x3 analysis of variance 
#########################################################################
aov2 = aov(outcomeMeasures~mslaborLand*lmhpanas,data=postDf)
summary(aov2)
# summary(aov2)
#                      Df Sum Sq Mean Sq F value   Pr(>F)    
# mslaborLand           1 203.49  203.49  19.234 0.000139 ***
# lmhpanas              2  58.25   29.12   2.753 0.080430 .  
# mslaborLand:lmhpanas  2  85.09   42.55   4.021 0.028745 *  
# Residuals            29 306.82   10.58                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

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
# > summary(aov3)
#             Df Sum Sq Mean Sq F value Pr(>F)
# lmhpanas     2  187.1   93.57    1.77  0.187
# Residuals   32 1692.1   52.88               

#########################################################################
## find the respondent with the worst outcome
#########################################################################
# this graph plots om v ll with P015 as color (red color = cesarean)
ggplot(postDf,aes(x=laborLand, y=outcomeMeasures, color=P015))+geom_point()
subset(postDf,outcomeMeasures==min(outcomeMeasures))

ggplot(postDf,aes(x=painExp, y=outcomeMeasures, color=P015))+geom_point()

ggplot(postDf,aes(x=painExp, y=outcomeMeasures, color=mslaborLand))+geom_point()

ggplot(postDf,aes(x=mslaborLand, y=painExp)) + 
		stat_summary(fun.data = "mean_cl_boot") 

ggplot(postDf,aes(x=lmhpanas, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot") 