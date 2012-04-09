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
## split into drugs v. no drugs
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

#########################################################################
## split all the themes
#########################################################################
# intuitMov
imindex = postDf$intuitMov>=median(postDf$intuitMov)
# selects TRUE values
postDf$intuitMov[imindex]
postDf$msintuitMov = "low"
postDf$msintuitMov[imindex] = "high"
postDf$msintuitMov = factor(postDf$msintuitMov,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=msintuitMov))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=msintuitMov, y=outcomeMeasures))+geom_boxplot()

# physEnv
peindex = postDf$physEnv>=median(postDf$physEnv)
# selects TRUE values
postDf$physEnv[peindex]
postDf$msphysEnv = "low"
postDf$msphysEnv[peindex] = "high"
postDf$msphysEnv = factor(postDf$msphysEnv,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=msphysEnv))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=msphysEnv, y=outcomeMeasures))+geom_boxplot()

# emotEnv
eeindex = postDf$emotEnv>=median(postDf$emotEnv)
# selects TRUE values
postDf$emotEnv[eeindex]
postDf$msemotEnv = "low"
postDf$msemotEnv[eeindex] = "high"
postDf$msemotEnv = factor(postDf$msemotEnv,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=msemotEnv))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=msemotEnv, y=outcomeMeasures))+geom_boxplot()

# fluidReal
frindex = postDf$fluidReal>=median(postDf$fluidReal)
# selects TRUE values
postDf$fluidReal[frindex]
postDf$msfluidReal = "low"
postDf$msfluidReal[frindex] = "high"
postDf$msfluidReal = factor(postDf$msfluidReal,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=msfluidReal))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=msfluidReal, y=outcomeMeasures))+geom_boxplot()

# intensePres
ipindex = postDf$intensePres>=median(postDf$intensePres)
# selects TRUE values
postDf$intensePres[ipindex]
postDf$msintensePres = "low"
postDf$msintensePres[ipindex] = "high"
postDf$msintensePres = factor(postDf$msintensePres,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=msintensePres))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=msintensePres, y=outcomeMeasures))+geom_boxplot()

memindex = postDf$memory>=median(postDf$memory)
# selects TRUE values
postDf$memory[memindex]
postDf$msmemory = "low"
postDf$msmemory[memindex] = "high"
postDf$msmemory = factor(postDf$msmemory,levels = c("low","high"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=msmemory))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=msmemory, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## split education into less than 4-year-college and 4-year-college plus
#########################################################################
eduindex = postDf$P006>=5
# selects TRUE values
postDf$P006[eduindex]
postDf$educationsplit = "<4-year-college"
postDf$educationsplit[eduindex] = "4-year-college+"
postDf$educationsplit = factor(postDf$educationsplit,levels = c("<4-year-college","4-year-college+"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=educationsplit))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=educationsplit, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## split income into less than $50,000 and more than $50,000
#########################################################################
incindex = postDf$P009>=3
# selects TRUE values
postDf$P009[incindex]
postDf$incomesplit = "<$50,000"
postDf$incomesplit[incindex] = ">$50,000"
postDf$incomesplit = factor(postDf$incomesplit,levels = c("<$50,000",">$50,000"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=incomesplit))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=incomesplit, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## split into before/on due date and post-date
#########################################################################
ddindex = postDf$P021>=5
# selects TRUE values
postDf$P021[ddindex]
postDf$duedatesplit = "on/before due date"
postDf$duedatesplit[ddindex] = "post-date"
postDf$duedatesplit = factor(postDf$duedatesplit,levels = c("on/before due date","post-date"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=duedatesplit))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=duedatesplit, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## split by time spent in active labor
#########################################################################
atindex = postDf$P022>=4
# selects TRUE values
postDf$P022[atindex]
postDf$msactiveLabor = "<6hrs"
postDf$msactiveLabor[atindex] = ">6hrs"
postDf$msactiveLabor = factor(postDf$msactiveLabor,levels = c("<6hrs",">6hrs"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=msactiveLabor))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=msactiveLabor, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## split by time spent pushing
#########################################################################
pushindex = postDf$P023>=3
# selects TRUE values
postDf$P023[pushindex]
postDf$mspushing = "<40min"
postDf$mspushing[pushindex] = ">40min"
postDf$mspushing = factor(postDf$mspushing,levels = c("<40min",">40min"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=mspushing))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=mspushing, y=outcomeMeasures))+geom_boxplot()


#########################################################################
## split by time felt in active labor
#########################################################################
afindex = postDf$P152>median(postDf$P152)
# selects TRUE values
postDf$P152[afindex]
postDf$msactiveFeel = "<4hrs"
postDf$msactiveFeel[afindex] = ">4hrs"
postDf$msactiveFeel = factor(postDf$msactiveFeel,levels = c("<4hrs",">4hrs"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=msactiveFeel))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=msactiveFeel, y=outcomeMeasures))+geom_boxplot()

#########################################################################
## split by time felt pushing
#########################################################################
pushfindex = postDf$P153>=3
# selects TRUE values
postDf$P153[pushfindex]
postDf$mspushingFeel = "<40min"
postDf$mspushingFeel[pushfindex] = ">40min"
postDf$mspushingFeel = factor(postDf$mspushingFeel,levels = c("<40min",">40min"))
# this plot checks to make sure they were divided evenly
ggplot(postDf,aes(x=mspushingFeel))+geom_histogram()
# boxplot of outcomeMeasures based on low v. high expectations
ggplot(postDf,aes(x=mspushingFeel, y=outcomeMeasures))+geom_boxplot()


save(postDf,newpostDf,origPostDf,descPostDf,postNumVarNames,file="rdata/postData.RData")
