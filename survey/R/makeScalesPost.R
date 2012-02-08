###############################################################################
## makeScalesPost.R
##
## make attribute scales
## 
## Author: Haaland
###############################################################################
load("rdata/postData.RData")
## complex example
scaleNames1 = c("P011","P012","P020","P035","P067")
sDf1 = postDf[,scaleNames1]
describe(sDf1)
key.list <- list(all = c(1,2,3,4,5), first=c(1,2,3))
keys <- make.keys(5,key.list,item.labels = colnames(sDf1))
keys
scores <- score.items(keys,sDf1)
scores
summary(scores)

data(bfi)
keys.list <- list(agree=c(-1,2:5),conscientious=c(6:8,-9,-10),extraversion=c(-11,-12,13:15),neuroticism=c(16:20),openness = c(21,-22,23,24,-25))
keys <- make.keys(25,keys.list,item.labels=colnames(bfi)[1:25])
scores <- score.items(keys,bfi[,1:25])
summary(scores)


## simple example
scaleNames1 = c("P011","P012","P020","P035","P067")
sDf1 = postDf[,scaleNames1]
describe(sDf1)
scale1 = apply(sDf1[,1:3],1,sum)
scale2 = apply(sDf1[,4:5],1,sum)
sDf = data.frame(sDf1, scale1 = scale1, scale2 = scale2)
cor(sDf)

physEnvNames = c("P011","P012","P020","P035","P067")
sDf1 = postDf[,physEnvNames]
sDf1 = scale(sDf1)
describe(sDf1)
physEnv = apply(sDf1,1,sum)
sDf = data.frame(sDf1, physEnv = physEnv)
sDf
cor(sDf)

emotEnvNames = c("P036","P041","P101","P111","P029")
emotEnvDf = postDf[,emotEnvNames]
emotEnvDf = scale(emotEnvDf)
describe(emotEnvDf)
emotEnv = apply(emotEnvDf,1,sum)
emotEnvDf = data.frame(emotEnvDf, emotEnv = emotEnv)
emotEnvDf
cor(emotEnvDf)

## TODO: practice with two or three most important

groupsDf = data.frame(physEnv = physEnv, emotEnv = emotEnv)
cor(groupsDf)
library(ggplot2)
ggplot(groupsDf,aes(x=physEnv, y=emotEnv))+geom_point()