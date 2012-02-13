###############################################################################
## makeScalesPost.R
##
## make attribute scales
## 
## Author: Haaland
###############################################################################
load("rdata/postData.RData")
## complex example
#scaleNames1 = c("P011","P012","P020","P035","P067")
#sDf1 = postDf[,scaleNames1]
#describe(sDf1)
#key.list <- list(all = c(1,2,3,4,5), first=c(1,2,3))
#keys <- make.keys(5,key.list,item.labels = colnames(sDf1))
#keys
#scores <- score.items(keys,sDf1)
#scores
#summary(scores)
#
#data(bfi)
#keys.list <- list(agree=c(-1,2:5),conscientious=c(6:8,-9,-10),extraversion=c(-11,-12,13:15),neuroticism=c(16:20),openness = c(21,-22,23,24,-25))
#keys <- make.keys(25,keys.list,item.labels=colnames(bfi)[1:25])
#scores <- score.items(keys,bfi[,1:25])
#summary(scores)



physEnvNames = c("P048","P049","P051","P083","P102","P110","P118")
physEnvDf = postDf[,physEnvNames]
physEnvDf_scale = scale(physEnvDf)
describe(physEnvDf_scale)
physEnv = apply(physEnvDf_scale,1,sum)
physEnvDf_scale = data.frame(physEnvDf_scale, physEnv = physEnv)
physEnvDf_scale
cor(physEnvDf_scale)
postDf$physEnv=physEnv

#emotEnvNames = c("P036","P041","P101","P111","P029")
#emotEnvDf = postDf[,emotEnvNames]
#emotEnvDf = scale(emotEnvDf)
#describe(emotEnvDf)
#emotEnv = apply(emotEnvDf,1,sum)
#emotEnvDf = data.frame(emotEnvDf, emotEnv = emotEnv)
#emotEnvDf
#cor(emotEnvDf)

laborLandNames = c("P071","P072","P073","P079","P080","P085","P086",
		"P089","P093","P094","P096","P108","P115","P132","P146")
laborLandDf = postDf[,laborLandNames]
laborLandDf_scale = scale(laborLandDf)
describe(laborLandDf_scale)
laborLand = apply(laborLandDf_scale,1,sum)
laborLandDf_scale = data.frame(laborLandDf_scale, laborLand = laborLand)
laborLandDf_scale
cor(laborLandDf_scale)
postDf$laborLand=laborLand


groupsDf = data.frame(physEnv = physEnv, laborLand = laborLand)
cor(groupsDf)
library(ggplot2)
ggplot(groupsDf,aes(x=physEnv, y=laborLand))+geom_point()

#########################################################################
## creating difference scores for actual vs. perceived time in active
## labor and pushing
#########################################################################

## 0-2 hours | 2-4 hours | 4-6 hours | 6-8 hours | 8-10 hours | 
## 10-12 hours | More than 12 hours
tValsAct = c(1,3,5,7,9,11,13)
## values for active labor
tActualAct = tValsAct[postDf[,c("P022")]]
tPerceivedAct = tValsAct[postDf[,c("P152")]]
tDiffAct = tActualAct - tPerceivedAct
## add these variables to the dataframe
postDf$tActualAct=tActualAct
postDf$tPerceivedAct=tPerceivedAct
postDf$tDiffAct=tDiffAct

## 0-19 minutes | 20-39 minutes | 40-59 minutes | 60-79 minutes | 
## 80-99 minutes | 100-120 minutes | more than 2 hours
tValsPush = c(10,30,50,70,90,110,130)
##values for pushing
tActualPush = tValsPush[postDf[,c("P023")]]
tPerceivedPush = tValsPush[postDf[,c("P153")]]
tDiffPush = tActualPush - tPerceivedPush
## add these variables to the dataframe
postDf$tActualPush=tActualPush
postDf$tPerceivedPush=tPerceivedPush
postDf$tDiffPush=tDiffPush

save(postDf,descPostDf,postNumVarNames,file="rdata/postData.RData")