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
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=mslaborLand))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high laborLand
ggplot(postDf,aes(x=mslaborLand, y=outcomeMeasures))+geom_boxplot()
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
## create a new variable based on median split of panas
#########################################################################
panas
panasbreaks = c(min(panas)-.5,quantile(panas, probs = c(.33,.67)),
		max(panas)+.5)
panasbreaks
lmhpanas = cut(panas, breaks=panasbreaks,labels=c("low","medium","high"))
table(lmhpanas)
# table(lmhpanas)
# panaslmh
#    low medium   high 
#     12     11     12 
postDf$lmhpanas = lmhpanas
# histogram confirms that it's been split properly
ggplot(postDf,aes(x=lmhpanas))+geom_histogram()
# boxplot of laborLand v panas
ggplot(postDf,aes(x=lmhpanas, y=laborLand))+geom_boxplot()
# show histogram of how many ppl were low v. high ll based on panas
ggplot(postDf,aes(x=lmhpanas, color=mslaborLand))+geom_histogram()

#########################################################################
## 2x3 analysis of variance 
#########################################################################
aov2 = aov(outcomeMeasures~mslaborLand*lmhpanas,data=postDf)
summary(aov2)
# summary(aov2)
#                      Df Sum Sq Mean Sq F value   Pr(>F)    
# mslaborLand           1 228.55  228.55  21.864 6.24e-05 ***
# lmhpanas              2  55.62   27.81   2.660   0.0869 .  
# mslaborLand:lmhpanas  2  66.34   33.17   3.173   0.0567 .  
# Residuals            29 303.14   10.45                     
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
# lmhpanas     2    128    63.9   0.631  0.539
# Residuals   32   3242   101.3               
           

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

# this plot is really interesting - shows that laborLand & painExp
# aren't really interacting for medium panas; but they are for
# low and high panas; means that if you're high panas, laborLand 
# really helps you, and that even if you are low panas, if you
# manage to get into laborLand, you can still reap its benefits 
# (same as for outcome measures)
ggplot(postDf,aes(x=lmhpanas, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot") 




save(postDf,newpostDf,origPostDf,descPostDf,postNumVarNames,file="rdata/postData.RData")
