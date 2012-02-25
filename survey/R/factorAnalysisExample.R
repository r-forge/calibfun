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

vnames = postNumVarNames[45:153]
adf = postDf[,vnames]
head(adf)
amat = as.matrix(adf)
amat = scale(amat)
head(amat)

VSS.scree(cor(adf))
## try 11 factors
fa1 = fa(amat,nfactors=9,rotate="varimax")
fa1
names(fa1)
#  [1] "residual"     "dof"          "fit"          "fit.off"      "sd"          
#  [6] "crms"         "rms"          "factors"      "n.obs"        "objective"   
# [11] "criteria"     "STATISTIC"    "PVAL"         "Call"         "null.model"  
# [16] "null.dof"     "null.chisq"   "TLI"          "RMSEA"        "BIC"         
# [21] "r.scores"     "R2"           "valid"        "score.cor"    "weights"     
# [26] "communality"  "uniquenesses" "values"       "e.values"     "loadings"    
# [31] "fm"           "Phi"          "scores"       "fn"          
## these are the new factors
head(fa1$scores)
# head(fa1$scores)
#          MR1         MR5        MR4        MR3       MR2       MR9       MR6
# 2   6.517042   1.6114181  1.7756405  1.2167409 -1.459299 -1.042572 -1.916654
# 3   8.850451   6.4823043  7.3670616 -4.2660821  1.312145  8.748369  6.434724
# 4 -22.189623 -10.0668784 -7.2429155 -4.5427400 -2.608104  5.636866 -3.987410
# 5 -16.242285  -8.2375659 -5.5731127  3.4011484  1.976425  9.693853 11.200146
# 6   9.726003   0.3945343 -0.2740542 -9.0314816 -4.116140 -3.532443 -3.847852
# 7 -39.529954 -14.5150076 -8.9962966  0.6646337 15.032098  1.120348  2.240037
#           MR7       MR8
# 2   0.8235385  2.241063
# 3  -3.4088099  3.242389
# 4  -2.2568528 -4.019609
# 5   2.7360655 -1.743900
# 6   0.1347610 -1.947237
# 7 -19.2914826  0.109450

## add the new factors to the original matrix,
## I'm not going to do this permanently right now
fadf = cbind(postDf,fa1$scores)
dim(fadf)
# [1]  35 181
names(fadf)

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
llFaGroupsDf = data.frame(MR1 = MR1, MR2 = MR2, MR3 = MR3, MR4 = MR4,
		MR5 = MR5, MR6 = MR6, MR7 = MR7, MR8 = MR8, MR9 = MR9,
		MR10 = MR10, MR11 = MR11, laborLand = laborLand)
llFaGrpcorrtest = corr.test(llFaGroupsDf)
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
## what does the factor represent?
## pick the variables that have the largest absolute values
## MR2 represents, e.g., P030, P032, P036, P037, ...
## then you make up a story and a name
fa1$loadings
# fa1$loadings
# 
#                   MR1   MR5   MR4   MR3   MR2   MR9   MR6   MR7   MR8
# SS loadings    16.312 8.582 8.474 6.476 6.365 5.593 5.441 5.028 5.020
# Proportion Var  0.150 0.079 0.078 0.059 0.058 0.051 0.050 0.046 0.046
# Cumulative Var  0.150 0.228 0.306 0.366 0.424 0.475 0.525 0.571 0.617



## this is another way to look at it, you could extract just these variables and look at them more closely
head(factor2cluster(fa1$loadings))
# head(factor2cluster(fa1$loadings))
#      MR1 MR5 MR4 MR3 MR2 MR9 MR6 MR7 MR8
# P048   0   0   1   0   0   0   0   0   0
# P049   0   0   0   0   0   0   0  -1   0
# P050   0   0   0   0   0   0   1   0   0
# P051   0   0   0   0   0   0   1   0   0
# P052   0   0   1   0   0   0   0   0   0
# P053   0   0   0   1   0   0   0   0   0

## could I recall only the questions = 1 and make a table?
factor2cluster(fa1$loadings)[,"MR1",drop=FALSE]
factor2cluster(fa1$loadings)[,"MR2",drop=FALSE]
factor2cluster(fa1$loadings)[,"MR3",drop=FALSE]
factor2cluster(fa1$loadings)[,"MR4",drop=FALSE]
factor2cluster(fa1$loadings)[,"MR5",drop=FALSE]
factor2cluster(fa1$loadings)[,"MR6",drop=FALSE]
factor2cluster(fa1$loadings)[,"MR7",drop=FALSE]
factor2cluster(fa1$loadings)[,"MR8",drop=FALSE]
factor2cluster(fa1$loadings)[,"MR9",drop=FALSE]

## MR1 = P061,P065,P076,P084,P087,P088,P097,P105,P109,P113,P114,P119,
##       P121,P122,P124,P125,P126,P131,P132,P133,P135,P138,P139,P141,
##		 P142,P145,P146,P147,P148,P149,P150,P151,P155,P156
## 		 (34 questions)
## MR5 = P062,P071,P080,P085,P086,P089,P093,P094,P100,P101,P103,P107,
##		 P108,P115,P127,P129,P136
## MR4 = P048,P052,P063,P066,P069,P070,P077,P082,P083,P092,P102
## MR3 = P053,P057,P059,P060,(-P074),P075,P078,P090,P095,P098,P112,(-P116)
## MR2 = P054,P055,P056,(-P099)
## MR9 = P068,P072,(-P117),P128,(-P134),P137,(-P140)
## MR6 = P050,P051,P058,(-P064),P073,(-P079),P104,(-P123),(-P130),P153
## MR7 = (-P049),(-P081),(-P091),P096,P106,P110,P111,(-P143),(-P144)
## MR8 = P067,P118,P120,(-P152),P154


