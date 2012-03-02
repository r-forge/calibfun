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
#########################################################################
## based on scree plot, choose 5 factors
#########################################################################
faStateQs5 = fa(stateQsMat,nfactors=5,rotate="varimax")
faStateQs5
# The degrees of freedom for the null model are  2775  and the objective function was  413.3 with Chi Square of  3375.31
# The degrees of freedom for the model are 2410  and the objective function was  79 
#
# The root mean square of the residuals is  0.08 
# The df corrected root mean square of the residuals is  0.11 
# The number of observations was  26  with Chi Square =  0  with prob <  1 
names(faStateQs5)
# names(faStateQs)
#  [1] "residual"     "dof"          "fit"          "fit.off"      "sd"          
#  [6] "crms"         "rms"          "factors"      "n.obs"        "objective"   
# [11] "criteria"     "STATISTIC"    "PVAL"         "Call"         "null.model"  
# [16] "null.dof"     "null.chisq"   "TLI"          "RMSEA"        "BIC"         
# [21] "r.scores"     "R2"           "valid"        "score.cor"    "weights"     
# [26] "communality"  "uniquenesses" "values"       "e.values"     "loadings"    
# [31] "fm"           "scores"       "fn"          
## these are the new factors
head(faStateQs5$scores)
# head(faStateQs5$scores)
#          MR4        MR3        MR5        MR2        MR1
# 2   1.636189   2.304578   2.609391  4.2781150   1.514141
# 3   4.065924  -6.608126   7.038655 -0.5406977   8.769526
# 4 -10.875009  -9.370953 -10.882372 -5.9698676  -4.715227
# 5 -11.811480  -7.187901  -2.861329  6.1051425   3.281700
# 6  -1.772833   4.802828   2.542439 -6.9914593   5.325620
# 7 -14.324523 -30.949387  -3.886985  3.1017261 -11.037747

## add the new factors to the original matrix,
## I'm not going to do this permanently right now
faStateQs5Df = cbind(postDf,faStateQs5$scores)
dim(faStateQs5Df)
# dim(faStateQs5Df)
# [1]  26 162
names(faStateQs5Df)

## what does the factor represent?
## pick the variables that have the largest absolute values
## MR2 represents, e.g., P030, P032, P036, P037, ...
## then you make up a story and a name
faStateQs5$loadings
# faStateQs5$loadings
#                  MR4   MR3   MR5   MR2   MR1
# SS loadings    9.705 8.334 8.119 7.590 6.299
# Proportion Var 0.129 0.111 0.108 0.101 0.084
# Cumulative Var 0.129 0.241 0.349 0.450 0.534

## this is another way to look at it, you could extract just these 
## variables and look at them more closely
head(factor2cluster(faStateQs5$loadings))
# head(factor2cluster(faStateQs5$loadings))
#      MR4 MR3 MR5 MR2 MR1
# P048   0   0   1   0   0
# P049   0  -1   0   0   0
# P050   0   0   0   0   1
# P057   0   0   0   1   0
# P058   0   0   0   0   1
# P059   0   0   0   1   0

# old clusters
# head(factor2cluster(faStateQs5$loadings))
#      MR1 MR4 MR5 MR3 MR2
# P048   0   0   1   0   0
# P049   0   1   0   0   0
# P050   1   0   0   0   0
# P057   0   0   0   0   1
# P058   1   0   0   0   0
# P059   0   0   0   0   1

## could I recall only the questions = 1 and make a table?
# These variables won't be accurate b/c they're being written
# over with each factor analysis done
MR1 = factor2cluster(faStateQs5$loadings)[,"MR1",drop=FALSE]
MR2 = factor2cluster(faStateQs5$loadings)[,"MR2",drop=FALSE]
MR3 = factor2cluster(faStateQs5$loadings)[,"MR3",drop=FALSE]
MR4 = factor2cluster(faStateQs5$loadings)[,"MR4",drop=FALSE]
MR5 = factor2cluster(faStateQs5$loadings)[,"MR5",drop=FALSE]

## copied from corrlations below to aid in recording factor ?s
## sQFa5GroupsDf should stay accurate as long as I don't re-run it
sQFa5GroupsDf = data.frame(MR1 = MR1, MR2 = MR2, MR3 = MR3, MR4 = MR4,
		MR5 = MR5)

#########################################################################
## now try fa with 6 factors
#########################################################################
faStateQs6 = fa(stateQsMat,nfactors=6,rotate="varimax")
faStateQs6
# The degrees of freedom for the null model are  2775  and the objective function was  413.3 with Chi Square of  3375.31
# The degrees of freedom for the model are 2340  and the objective function was  67.13 
#
# The root mean square of the residuals is  0.07 
# The df corrected root mean square of the residuals is  0.1 
# The number of observations was  26  with Chi Square =  0  with prob <  1 
names(faStateQs6)
# names(faStateQs6)
#  [1] "residual"     "dof"          "fit"          "fit.off"      "sd"          
#  [6] "crms"         "rms"          "factors"      "n.obs"        "objective"   
# [11] "criteria"     "STATISTIC"    "PVAL"         "Call"         "null.model"  
# [16] "null.dof"     "null.chisq"   "TLI"          "RMSEA"        "BIC"         
# [21] "r.scores"     "R2"           "valid"        "score.cor"    "weights"     
# [26] "communality"  "uniquenesses" "values"       "e.values"     "loadings"    
# [31] "fm"           "scores"       "fn"          
## these are the new factors
head(faStateQs6$scores)
# head(faStateQs6$scores)
#          MR1        MR5         MR4       MR2        MR3       MR6
# 2   3.254352   2.460821   0.6084983  3.429098   1.136420  2.392158
# 3   3.499817   8.179618  -2.3502857 -4.597317  -7.437335  7.771551
# 4 -11.597996 -10.725473  -7.9651391 -3.710908  -4.908971 -5.634302
# 5  -1.551279  -3.679030 -12.2895602  6.002191  -6.461578 -1.900194
# 6   5.914185   2.161215  -1.6382865 -6.447516   2.731517 -5.924113
# 7 -22.457680  -4.948335  -9.9493146  7.127682 -24.445879 -6.519786

## add the new factors to the original matrix,
## I'm not going to do this permanently right now
faStateQs6Df = cbind(postDf,faStateQs6$scores)
dim(faStateQs6Df)
# dim(faStateQs6Df)
# [1]  26 163
names(faStateQs6Df)

## what does the factor represent?
## pick the variables that have the largest absolute values
## MR2 represents, e.g., P030, P032, P036, P037, ...
## then you make up a story and a name
faStateQs6$loadings
# faStateQs6$loadings
#                  MR1   MR5   MR4   MR2   MR3   MR6
# SS loadings    8.383 8.075 8.019 7.660 6.771 5.082
# Proportion Var 0.112 0.108 0.107 0.102 0.090 0.068
# Cumulative Var 0.112 0.219 0.326 0.429 0.519 0.587

## this is another way to look at it, you could extract just these 
## variables and look at them more closely
head(factor2cluster(faStateQs6$loadings))
# head(factor2cluster(faStateQs6$loadings))
#      MR1 MR5 MR4 MR2 MR3 MR6
# P048   0   1   0   0   0   0
# P049   0   0   0   0  -1   0
# P050   1   0   0   0   0   0
# P057   0   0   0   0   0   1
# P058   1   0   0   0   0   0
# P059   0   0   0   1   0   0

MR1 = factor2cluster(faStateQs6$loadings)[,"MR1",drop=FALSE]
MR2 = factor2cluster(faStateQs6$loadings)[,"MR2",drop=FALSE]
MR3 = factor2cluster(faStateQs6$loadings)[,"MR3",drop=FALSE]
MR4 = factor2cluster(faStateQs6$loadings)[,"MR4",drop=FALSE]
MR5 = factor2cluster(faStateQs6$loadings)[,"MR5",drop=FALSE]
MR6 = factor2cluster(faStateQs6$loadings)[,"MR6",drop=FALSE]

## copied from corrlations below to aid in recording factor ?s
## sQFa6GroupsDf should stay accurate as long as I don't re-run it
sQFa6GroupsDf = data.frame(MR1 = MR1, MR2 = MR2, MR3 = MR3, MR4 = MR4,
		MR5 = MR5, MR6 = MR6)

#########################################################################
## Following code is mostly likely obsolete - keep it just in case
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
sQFa6GroupsDf = data.frame(MR1 = MR1, MR2 = MR2, MR3 = MR3, MR4 = MR4,
		MR5 = MR5, MR6 = MR6)
sQFa6Grpcorrtest = corr.test(sQFa6GroupsDf)
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

