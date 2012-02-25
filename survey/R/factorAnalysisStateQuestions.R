################################################################################
## factorAnalysisExample.R
##
## work out some details of a factor analysis as an example
## 
## Author: Haaland
###############################################################################
library(ggplot2)
library(psych)
library(GPArotation)
load("rdata/postData.RData")

names(postDf)

stateQsNames = c("P048","P049","P050","P057","P058","P059","P060",
		"P061","P062","P063","P064","P068","P069","P070","P071",
		"P072","P073","P074","P075","P076","P077","P078","P079",
		"P080","P082","P083","P085","P086","P087","P089","P090",
		"P092","P093","P094","P095","P096","P097","P098","P099",
		"P101","P102","P103","P104","P105","P106","P107","P108",
		"P109","P110","P111","P112","P113","P114","P115","P116",
		"P117","P118","P119","P120","P121","P123","P126","P127",
		"P130","P132","P135","P136","P137","P138","P139","P141",
		"P143","P144","P146","P147")
stateQsDf = postDf[,stateQsNames]
head(stateQsDf)
stateQsMat = as.matrix(stateQsDf)
stateQsMat = scale(stateQsMat)
head(stateQsMat)

VSS.scree(cor(stateQsDf))
# based on scree plot, choose 5 factors
faStateQs = fa(stateQsMat,nfactors=5,rotate="varimax")
faStateQs
names(faStateQs)
# names(faStateQs)
#  [1] "residual"     "dof"          "fit"          "fit.off"      "sd"          
#  [6] "crms"         "rms"          "factors"      "n.obs"        "objective"   
# [11] "criteria"     "STATISTIC"    "PVAL"         "Call"         "null.model"  
# [16] "null.dof"     "null.chisq"   "TLI"          "RMSEA"        "BIC"         
# [21] "r.scores"     "R2"           "valid"        "score.cor"    "weights"     
# [26] "communality"  "uniquenesses" "values"       "e.values"     "loadings"    
# [31] "fm"           "scores"       "fn"          
## these are the new factors
head(faStateQs$scores)
# head(faStateQs$scores)
#          MR1         MR4       MR5        MR3        MR2
# 2   4.148855   2.3152170  3.786710   1.177365  3.2361204
# 3   6.983633   3.9714199  7.235053  -7.172253 -2.7008853
# 4  -9.869244  -7.1631393 -8.880756  -2.937839 -4.2135603
# 5  -2.346034 -10.3019015 -1.357387  -6.892012  3.7141169
# 6   6.662303  -0.7461599  1.646485   3.667921 -6.7970963
# 7 -21.816834  -9.7411846 -1.027380 -18.021899  0.9545925

## add the new factors to the original matrix,
## I'm not going to do this permanently right now
faStateQsDf = cbind(postDf,faStateQs$scores)
dim(faStateQsDf)
# dim(faStateQsDf)
# [1]  35 177
names(faStateQsDf)

## what does the factor represent?
## pick the variables that have the largest absolute values
## MR2 represents, e.g., P030, P032, P036, P037, ...
## then you make up a story and a name
faStateQs$loadings
# faStateQs$loadings
#                  MR1   MR4   MR5   MR3   MR2
# SS loadings    8.324 8.314 7.785 5.694 5.662
# Proportion Var 0.111 0.111 0.104 0.076 0.075
# Cumulative Var 0.111 0.222 0.326 0.402 0.477

## this is another way to look at it, you could extract just these variables and look at them more closely
head(factor2cluster(faStateQs$loadings))
# head(factor2cluster(faStateQs$loadings))
#      MR1 MR4 MR5 MR3 MR2
# P048   0   0   1   0   0
# P049   0   1   0   0   0
# P050   1   0   0   0   0
# P057   0   0   0   0   1
# P058   1   0   0   0   0
# P059   0   0   0   0   1

## could I recall only the questions = 1 and make a table?
MR1 = factor2cluster(faStateQs$loadings)[,"MR1",drop=FALSE]
MR2 = factor2cluster(faStateQs$loadings)[,"MR2",drop=FALSE]
MR3 = factor2cluster(faStateQs$loadings)[,"MR3",drop=FALSE]
MR4 = factor2cluster(faStateQs$loadings)[,"MR4",drop=FALSE]
MR5 = factor2cluster(faStateQs$loadings)[,"MR5",drop=FALSE]


#########################################################################
## create correlation graphs between laborLand and each factor(MR1-11)
#########################################################################
library(ggplot2)
pdf("plots/faCorrelations.pdf")
#quartz()
## *** DAD, I couldn't figure out how to get the correlations between
## MR1 - MR11 and laborLand (and outcomeMeasures)...I want to include
## this information on the graphs
## this is what I want to do except it doesn't recognize MR1
# create the correlations
sQFaGroupsDf = data.frame(MR1 = MR1, MR2 = MR2, MR3 = MR3, MR4 = MR4,
		MR5 = MR5)
sQFaGrpcorrtest = corr.test(sQFaGroupsDf)
## this is obvious - the factors are chosen to correlated the least
## with one another, so doing this test doesn't tell me anything

------

# correlations, unadjusted p and adjusted p
llFaGrpcres = data.frame(round(llFaGrpcorrtest$r[,12],3),
		round(llFaGrpcorrtest$p[12,],4),
		round(llFaGrpcorrtest$p[,12],4))
names(llFaGrpcres) = c("cor.","p-raw","p-adjusted")
llFaGrpcres
# now plot it
adf = llFaGrpcres[rownames(llFaGrpcres)=="MR1",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(llFaGroupsDf,aes(x=MR1, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)

## this is what I've done instead (without correlations)
ggplot(fadf,aes(x=MR1, y=laborLand))+geom_point()+
		geom_smooth(method=lm)
ggplot(fadf,aes(x=MR2, y=laborLand))+geom_point()+
		geom_smooth(method=lm)
ggplot(fadf,aes(x=MR3, y=laborLand))+geom_point()+
		geom_smooth(method=lm)
ggplot(fadf,aes(x=MR4, y=laborLand))+geom_point()+
		geom_smooth(method=lm)
ggplot(fadf,aes(x=MR5, y=laborLand))+geom_point()+
		geom_smooth(method=lm)
ggplot(fadf,aes(x=MR6, y=laborLand))+geom_point()+
		geom_smooth(method=lm)
ggplot(fadf,aes(x=MR7, y=laborLand))+geom_point()+
		geom_smooth(method=lm)
ggplot(fadf,aes(x=MR8, y=laborLand))+geom_point()+
		geom_smooth(method=lm)
ggplot(fadf,aes(x=MR9, y=laborLand))+geom_point()+
		geom_smooth(method=lm)
ggplot(fadf,aes(x=MR10, y=laborLand))+geom_point()+
		geom_smooth(method=lm)
ggplot(fadf,aes(x=MR11, y=laborLand))+geom_point()+
		geom_smooth(method=lm)
dev.off()

