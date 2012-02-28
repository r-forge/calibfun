###############################################################################
## checkData.R
##
## look for outliers in repondents, questions to be thrown out, etc.
## 
## Author: Haaland
###############################################################################
library(ggplot2)
library(psych)
library(GPArotation)
library(Hmisc)
load("rdata/postData.RData")

#########################################################################
## are cesarean women outliers? looking at P015 against all themes
#########################################################################
# fine
ggplot(postDf,aes(x=P015, y=laborLand, color=P015))+geom_point()
#fine
ggplot(postDf,aes(x=intuitMov, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=intuitMov, color=P015))+geom_point()
# looks like 2 cesareans are lowest on physEnv
ggplot(postDf,aes(x=physEnv, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=physEnv, color=P015))+geom_point()
# fine
ggplot(postDf,aes(x=emotEnv, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=emotEnv, color=P015))+geom_point()
# very large range: 1 is best; 1 is almost worst
ggplot(postDf,aes(x=fluidReal, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=fluidReal, color=P015))+geom_point()
# fine
ggplot(postDf,aes(x=intensePres, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=intensePres, color=P015))+geom_point()
# 3 are very high; 1 is very low (but within range)
ggplot(postDf,aes(x=painExp, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=painExp, color=P015))+geom_point()
# all had low; two had lowest (expectations) **********
ggplot(postDf,aes(x=expectations, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=expectations, color=P015))+geom_point()
# fine 
ggplot(postDf,aes(x=laborLand, y=outcomeMeasures, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=outcomeMeasures, color=P015))+geom_point()
# fine
ggplot(postDf,aes(x=laborLand, y=stateQs, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=stateQs, color=P015))+geom_point()
# fine
ggplot(postDf,aes(x=laborLand, y=panas, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=panas, color=P015))+geom_point()

# check anovas for unexpected (fluidReal; painExp; expectations)
aovCheck1 = aov(fluidReal~P015,data=postDf)
summary(aovCheck1)
# summary(aovCheck1)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P015         1    0.7   0.739   0.044  0.834
# Residuals   33  549.2  16.644               

aovCheck2 = aov(painExp~P015,data=postDf)
summary(aovCheck2)
# summary(aovCheck2)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P015         1   37.2   37.17   1.132  0.295
# Residuals   33 1083.9   32.84               

aovCheck3 = aov(expectations~P015,data=postDf)
summary(aovCheck3)
# summary(aovCheck3)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# P015         1  23.32  23.317   4.154 0.0496 *
# Residuals   33 185.23   5.613                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

## based on anova, cesareans are only significantly different from
## non-cesarean population on expectations (are any of these matched
## samples? If so, do further investigation of expectations)


#########################################################################
## check for any effects of postpartum length
#########################################################################
postDf$P002
# 5/4/11, 7/16/12???, 1/3/11, 5/11/11, 6/26/11, 9/23/10
# 18, 19, 20*, 22, 25, 30*

# split participants into two groups (shortpp and longpp)
# ?????



#########################################################################
## check for any effects of pain medications
#########################################################################




#########################################################################
## find the respondent with the worst outcome
#########################################################################
# this graph plots om v ll with P015 as color (red color = cesarean)
ggplot(postDf,aes(x=laborLand, y=outcomeMeasures, color=P015))+geom_point()
# pulls out the responses for this worst outcomes woman
subset(postDf,outcomeMeasures==min(outcomeMeasures))
# finding that women on this graph, she also has almost worst painExp
ggplot(postDf,aes(x=painExp, y=outcomeMeasures, color=P015))+geom_point()



