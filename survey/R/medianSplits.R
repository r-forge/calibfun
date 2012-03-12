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
llindex = postDf$panas>=median(postDf$panas)
# selects TRUE values
postDf$panas[llindex]
postDf$mslaborLand = "low"
postDf$mslaborLand[llindex] = "high"
postDf$mslaborLand = factor(postDf$mslaborLand,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=mslaborLand))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high laborLand
ggplot(postDf,aes(x=mslaborLand, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## create a new variable based on median split of panas
#########################################################################
postDf$panas
panasbreaks = c(min(postDf$panas)-.5,quantile(postDf$panas, probs = c(.33,.67)),
		max(postDf$panas)+.5)
panasbreaks
lmhpanas = cut(postDf$panas, breaks=panasbreaks,labels=c("low","medium","high"))
table(lmhpanas)
# table(lmhpanas)
# lmhpanas
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
## split panas into only two categories
#########################################################################
panasindex = postDf$panas>=median(postDf$panas)
# selects TRUE values
postDf$panas[panasindex]
postDf$mspanas = "low"
postDf$mspanas[panasindex] = "high"
postDf$mspanas = factor(postDf$mspanas,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=mspanas))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high laborLand
ggplot(postDf,aes(x=mspanas, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## split into drugs v. no drugs (copied from checkData)
#########################################################################
drugsindex = postDf$P016!=1
# selects TRUE values
postDf$P016[drugsindex]
postDf$drugsplit = "nodrugs"
postDf$drugsplit[drugsindex] = "drugs"
postDf$drugsplit = factor(postDf$drugsplit,levels = c("drugs","nodrugs"))
# this plot checks to make sure they were divided correctly
ggplot(postDf,aes(x=drugsplit))+geom_histogram()

# boxplot of laborLand based on drugs v. no drugs
ggplot(postDf,aes(x=drugsplit, y=laborLand))+geom_boxplot()

#########################################################################
## split painExp into two groups
#########################################################################
peindex = postDf$painExp>=median(postDf$painExp)
# selects TRUE values
postDf$painExp[peindex]
postDf$mspainExp = "low"
postDf$mspainExp[peindex] = "high"
postDf$mspainExp = factor(postDf$mspainExp,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=mspainExp))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high painExp
ggplot(postDf,aes(x=mspainExp, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## split expectations into two groups
#########################################################################
exindex = postDf$expectations>=median(postDf$expectations)
# selects TRUE values
postDf$expectations[exindex]
postDf$msexpectations = "low"
postDf$msexpectations[exindex] = "high"
postDf$msexpectations = factor(postDf$msexpectations,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=msexpectations))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=msexpectations, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## split into multiparious and primiparious
#########################################################################
pmindex = postDf$P001>=2
# selects TRUE values
postDf$P001[pmindex]
postDf$primipsplit = "primip"
postDf$primipsplit[pmindex] = "multip"
postDf$primipsplit = factor(postDf$primipsplit,levels = c("primip","multip"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=primipsplit))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=primipsplit, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## split into younger (<30) and older (>30)
#########################################################################
ageindex = postDf$P004>=3
# selects TRUE values
postDf$P004[ageindex]
postDf$agesplit = "younger"
postDf$agesplit[ageindex] = "older"
postDf$agesplit = factor(postDf$agesplit,levels = c("younger","older"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=agesplit))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=agesplit, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## split into low/high vocals
#########################################################################
vcindex = postDf$vocals>=median(postDf$vocals)
# selects TRUE values
postDf$vocals[vcindex]
postDf$vocalsplit = "low"
postDf$vocalsplit[vcindex] = "high"
postDf$vocalsplit = factor(postDf$vocalsplit,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=vocalsplit))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=vocalsplit, y=outcomeMeasures))+geom_boxplot()

save(postDf,newpostDf,origPostDf,descPostDf,postNumVarNames,file="rdata/postData.RData")
