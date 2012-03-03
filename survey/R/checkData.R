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
load("rdata/origPostData.RData")

#########################################################################
## Who might need to be removed:
## Cesareans - 4
## Medications - 4 additional (8 total)
## Long pp interval women - 6 (maybe only longest 2?)
#########################################################################



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
## check for any effects of pain medications/hospital birth
#########################################################################
ggplot(postDf,aes(x=P016, y=laborLand, color=P016))+geom_point()
# who is this woman with the worst laborLand score (had epidural)
# low laborLand, intuitMov, emotEnv, painExp, outcomeMeasures, panas
subset(postDf,laborLand==min(laborLand))

ggplot(postDf,aes(x=P016, y=painExp, color=P016))+geom_point()
ggplot(postDf,aes(x=P016, y=outcomeMeasures, color=P016))+geom_point()

ggplot(postDf,aes(x=laborLand, y=painExp, color=P016))+geom_point()
ggplot(postDf,aes(x=drugsplit, y=laborLand))+geom_point()

# find four women who had cesareans
subset(postDf,P015==2)
# find 8 women who used pain medication
subset(postDf,P016!=1)
# R_dnfNFyZENCZUYHa had very negative scores on all measures?
# had vaginal birth w/ epidural; in hospital, but did not intend 
# to be there

# how many hospital births were intended to be hospital births?
# only 5 (of 13) planned hospital births; 4 of these 5 had no drugs;
# of transfers, 4 due to cesarean; 3 more were to receive drugs; 1
# transfered but did not receive drugs (possibly got pitocin)
subset(postDf,P017==3,select=c(P015,P016,P017,P018))

#split participants in to drugs v. no drugs
drugsindex = origPostDf$P016!=1
# selects TRUE values
origPostDf$P016[drugsindex]
origPostDf$drugsplit = "nodrugs"
origPostDf$drugsplit[drugsindex] = "drugs"
origPostDf$drugsplit = factor(origPostDf$drugsplit,levels = c("nodrugs","drugs"))
# this plot checks to make sure they were divided correctly
ggplot(origPostDf,aes(x=drugsplit))+geom_histogram()

# boxplot of laborLand based on drugs v. no drugs
ggplot(postDf,aes(x=drugsplit, y=laborLand))+geom_boxplot()
aovCheck6 = aov(laborLand~drugsplit,data=postDf)
summary(aovCheck6)
# summary(aovCheck6)
#             Df Sum Sq Mean Sq F value Pr(>F)
# drugsplit    1     56   56.29   0.544  0.466
# Residuals   32   3311  103.48                           

ggplot(postDf,aes(x=drugsplit, y=outcomeMeasures))+geom_boxplot()
aovCheck7 = aov(outcomeMeasures~drugsplit,data=postDf)
summary(aovCheck7)
# summary(aovCheck7)
#             Df Sum Sq Mean Sq F value Pr(>F)
# drugsplit    1   51.4   51.40   2.769  0.106
# Residuals   32  594.0   18.56               

ggplot(postDf,aes(x=drugsplit, y=painExp))+geom_boxplot()
aovCheck8 = aov(painExp~drugsplit,data=postDf)
summary(aovCheck8)
# summary(aovCheck8)
#             Df Sum Sq Mean Sq F value Pr(>F)
# drugsplit    1   55.1   55.12   1.665  0.206
# Residuals   32 1059.6   33.11               


#########################################################################
## Test for w/ or w/o doula --> no sig. difference
#########################################################################
ggplot(postDf,aes(x=P026, y=laborLand))+geom_point()
ggplot(postDf,aes(x=laborLand, y=outcomeMeasures, color=P026))+geom_point()
aovCheck4 = aov(laborLand~P026,data=postDf)
summary(aovCheck4)
# summary(aovCheck4)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P026         1    171  171.44   1.769  0.193
# Residuals   33   3198   96.91               
       

#########################################################################
## Test for w/ or w/o birth plan --> no sig. difference
#########################################################################
ggplot(postDf,aes(x=P027, y=laborLand))+geom_point()
ggplot(postDf,aes(x=laborLand, y=outcomeMeasures, color=P027))+geom_point()
aovCheck5 = aov(laborLand~P027,data=postDf)
summary(aovCheck5)
# summary(aovCheck5)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P027         1      0     0.0       0      1
# Residuals   33   3370   102.1               
 



#########################################################################
## check for any effects of postpartum length
#########################################################################
postDf$P002
# 5/4/11, 7/16/12???, 1/3/11, 5/11/11, 6/26/11, 9/23/10
# 18, 19, 20*, 22, 25, 30*

# split participants into two groups (shortpp and longpp)
# ?????

ggplot(postDf,aes(x=laborLand, y=P002, color=P002))+geom_point()



#########################################################################
## find the respondent with the worst outcome
#########################################################################
# this graph plots om v ll with P015 as color (red color = cesarean)
ggplot(postDf,aes(x=laborLand, y=outcomeMeasures, color=P015))+geom_point()
# pulls out the responses for this worst outcomes woman
subset(postDf,outcomeMeasures==min(outcomeMeasures))
# finding that women on this graph, she also has almost worst painExp
ggplot(postDf,aes(x=painExp, y=outcomeMeasures, color=P015))+geom_point()
ggplot(postDf,aes(x=laborLand, y=painExp, color=P015))+geom_point()



