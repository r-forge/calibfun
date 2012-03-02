###############################################################################
## plotScalesPost.R
##
## plot scales (correlations)
## 
## Author: Haaland
###############################################################################
library(ggplot2)
library(psych)
library(GPArotation)
library(Hmisc)
load("rdata/postData.RData")

groupsDf = data.frame(intimateSec = intimateSec, RRR = RRR,
		spaceTime = spaceTime, emotEnv = emotEnv, surrender = surrender, 
		painExp = painExp, expectations = expectations, memory = memory,
		outcomeMeasures = outcomeMeasures, panas = panas,
		laborLand = laborLand)
grpcorrtest = corr.test(groupsDf)
# correlations, unadjusted p and adjusted p
grpcres = data.frame(round(grpcorrtest$r[,11],3),
		round(grpcorrtest$p[11,],4),
		round(grpcorrtest$p[,11],4))
names(grpcres) = c("cor.","p-raw","p-adjusted")
grpcres
# grpcres
#                   cor.  p-raw p-adjusted
# intimateSec      0.366 0.0657     1.0000
# RRR              0.432 0.0275     0.9071
# spaceTime        0.339 0.0898     1.0000
# emotEnv          0.470 0.0154     0.6001
# surrender        0.243 0.2320     1.0000
# painExp          0.405 0.0402     1.0000
# expectations     0.375 0.0590     1.0000
# memory          -0.141 0.4908     1.0000
# outcomeMeasures  0.572 0.0023     0.1095
# panas            0.410 0.0377     1.0000
# laborLand        1.000 0.0000     0.0000

## to get the whole correlation matrix
grpcorrtest$r
# grpcorrtest$r
#                 intimateSec       RRR  spaceTime   emotEnv   surrender
# intimateSec      1.00000000 0.2454642  0.6523591 0.4791270  0.07571398
# RRR              0.24546421 1.0000000  0.4459724 0.5480445  0.00824280
# spaceTime        0.65235913 0.4459724  1.0000000 0.2535162 -0.02588060
# emotEnv          0.47912701 0.5480445  0.2535162 1.0000000  0.44447703
# surrender        0.07571398 0.0082428 -0.0258806 0.4444770  1.00000000
# painExp          0.33710623 0.4604491  0.2366106 0.6049429  0.30894921
# expectations     0.54760045 0.5706746  0.2801225 0.6392933  0.17781550
# memory           0.15475381 0.1074732  0.2492438 0.1658386 -0.23032947
# outcomeMeasures  0.39924964 0.5823228  0.4321095 0.7192045  0.48339497
# panas            0.37804045 0.4533526  0.1924924 0.4553272  0.18430693
# laborLand        0.36630389 0.4319762  0.3394228 0.4700587  0.24280619
#                    painExp expectations      memory outcomeMeasures      panas
# intimateSec     0.33710623    0.5476004  0.15475381      0.39924964 0.37804045
# RRR             0.46044912    0.5706746  0.10747317      0.58232280 0.45335256
# spaceTime       0.23661057    0.2801225  0.24924384      0.43210953 0.19249238
# emotEnv         0.60494285    0.6392933  0.16583859      0.71920452 0.45532721
# surrender       0.30894921    0.1778155 -0.23032947      0.48339497 0.18430693
# painExp         1.00000000    0.5821059  0.05880164      0.71474497 0.50879899
# expectations    0.58210591    1.0000000  0.19796676      0.54051818 0.36652964
# memory          0.05880164    0.1979668  1.00000000      0.04288652 0.09607528
# outcomeMeasures 0.71474497    0.5405182  0.04288652      1.00000000 0.47361407
# panas           0.50879899    0.3665296  0.09607528      0.47361407 1.00000000
# laborLand       0.40486065    0.3750484 -0.14141489      0.57166053 0.40955837
#                  laborLand
# intimateSec      0.3663039
# RRR              0.4319762
# spaceTime        0.3394228
# emotEnv          0.4700587
# surrender        0.2428062
# painExp          0.4048606
# expectations     0.3750484
# memory          -0.1414149
# outcomeMeasures  0.5716605
# panas            0.4095584
# laborLand        1.0000000

# old correlations with old themes
# grpcorrtest$r
#                 intuitMov   physEnv   emotEnv  fluidReal intensePres   painExp
# intuitMov       1.0000000 0.3451254 0.6824775 0.37299954   0.4460095 0.5881909
# physEnv         0.3451254 1.0000000 0.5975637 0.51913543   0.2404237 0.4293421
# emotEnv         0.6824775 0.5975637 1.0000000 0.66897639   0.4582295 0.6554013
# fluidReal       0.3729995 0.5191354 0.6689764 1.00000000   0.3609485 0.5124390
# intensePres     0.4460095 0.2404237 0.4582295 0.36094851   1.0000000 0.5556691
# painExp         0.5881909 0.4293421 0.6554013 0.51243903   0.5556691 1.0000000
# expectations    0.5265144 0.5793594 0.6313744 0.39481053   0.4832478 0.5627930
# outcomeMeasures 0.7200597 0.5152935 0.7534936 0.50571296   0.5573094 0.7602627
# panas           0.4129443 0.2222202 0.3908986 0.06843493   0.2544647 0.3367425
# laborLand       0.7358440 0.2240105 0.5816486 0.42102764   0.5796943 0.5356285
#                 expectations outcomeMeasures      panas laborLand
# intuitMov          0.5265144       0.7200597 0.41294433 0.7358440
# physEnv            0.5793594       0.5152935 0.22222019 0.2240105
# emotEnv            0.6313744       0.7534936 0.39089864 0.5816486
# fluidReal          0.3948105       0.5057130 0.06843493 0.4210276
# intensePres        0.4832478       0.5573094 0.25446468 0.5796943
# painExp            0.5627930       0.7602627 0.33674250 0.5356285
# expectations       1.0000000       0.5880393 0.33426585 0.3815260
# outcomeMeasures    0.5880393       1.0000000 0.40628292 0.6069820
# panas              0.3342658       0.4062829 1.00000000 0.3423835
# laborLand          0.3815260       0.6069820 0.34238347 1.0000000


library(ggplot2)
pdf("plots/scaleCorrelations.pdf")
#########################################################################
## First, compare each theme to laborLand
#########################################################################
## intimateSec
adf = grpcres[rownames(grpcres)=="intimateSec",]
adf$x = -15
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=intimateSec, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## RRR
adf = grpcres[rownames(grpcres)=="RRR",]
adf$x = -6
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=RRR, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## spaceTime
adf = grpcres[rownames(grpcres)=="spaceTime",]
adf$x = -20
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=spaceTime, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## emotEnv
adf = grpcres[rownames(grpcres)=="emotEnv",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=emotEnv, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## surrender
adf = grpcres[rownames(grpcres)=="surrender",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=surrender, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## painExp
adf = grpcres[rownames(grpcres)=="painExp",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=painExp, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## expectations
adf = grpcres[rownames(grpcres)=="expectations",]
adf$x = -3
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=expectations, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## outcomeMeasures
groupsDf$P016 = postDf$P016
adf = grpcres[rownames(grpcres)=="outcomeMeasures",]
adf$x = -10
adf$y = 5
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=laborLand, y=outcomeMeasures, color=P016))+geom_point()
		#geom_smooth(method=lm)+
		#geom_text(aes(x=x, y=y, label=label),data=adf)
## panas
adf = grpcres[rownames(grpcres)=="panas",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=panas, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)

#########################################################################
## Now compare each theme to outcome measures; r- and p-values are
## not correct for these graphs
#########################################################################
## intimateSec
adf = grpcres[rownames(grpcres)=="intimateSec",]
adf$x = -15
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=intimateSec, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## RRR
adf = grpcres[rownames(grpcres)=="RRR",]
adf$x = -6
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=RRR, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## spaceTime
adf = grpcres[rownames(grpcres)=="spaceTime",]
adf$x = -20
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=spaceTime, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## emotEnv
adf = grpcres[rownames(grpcres)=="emotEnv",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=emotEnv, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## surrender
adf = grpcres[rownames(grpcres)=="surrender",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=surrender, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## painExp
adf = grpcres[rownames(grpcres)=="painExp",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=painExp, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## expectations
adf = grpcres[rownames(grpcres)=="expectations",]
adf$x = -3
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=expectations, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## outcomeMeasures
adf = grpcres[rownames(grpcres)=="outcomeMeasures",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=outcomeMeasures, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## panas
adf = grpcres[rownames(grpcres)=="panas",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=panas, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)

dev.off()

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

save(postDf,newpostDf,origPostDf,descPostDf,postNumVarNames,file="rdata/postData.RData")
