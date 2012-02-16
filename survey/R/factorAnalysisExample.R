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

## pick an arbitrary set to get started with
vnames = postNumVarNames[45:153]
adf = postDf[,vnames]
head(adf)
amat = as.matrix(adf)
amat = scale(amat)
head(amat)

VSS.scree(cor(adf))
## try 11 factors
fa1 = fa(amat,nfactors=11,rotate="varimax")
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
#          MR1        MR4        MR8        MR2        MR3       MR9        MR5
# 2   5.014329  2.6629876   1.416896 -1.5930822  0.4209971  4.170252 -0.7102057
# 3   6.955727  9.5054792   5.424834 -1.3125515  7.2248477 -7.784305  8.0236313
# 4 -25.403226 -8.3608009  -7.095082 -3.2729534  3.9311672 -4.869385 -5.1236742
# 5 -19.259708 -4.1224616  -6.838782 -0.6742797  0.6522129 -5.740370 10.8973924
# 6   5.630451  0.6375632   2.193712 -5.1911224 11.8468558  8.043981 -1.9591005
# 7 -45.605689 -6.2239807 -14.534606 11.0746464 -8.2841520 -6.020012  3.4038961
#         MR6       MR11         MR7       MR10
# 2  0.860406  3.5733990  -3.4079563 -0.6857798
# 3  5.626360  0.8268897  -1.9451418  3.8062569
# 4 -4.432112 -3.1447216  -3.2624254 -2.6452789
# 5 -6.440587  1.7310552  -0.7244511  4.2841007
# 6  3.386501  1.2217109  -4.0150683 -6.1222809
# 7  3.948384 -9.1575746 -13.1969195 -6.0090564

## add the new factors to the original matrix,
## I'm not going to do this permanently right now
fadf = cbind(postDf,fa1$scores)
dim(fadf)
# [1]  31 167
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
#MR1   MR4   MR8   MR2   MR3   MR9   MR5   MR6  MR11   MR7
#SS loadings    18.472 7.958 7.176 6.432 6.048 5.420 5.402 5.398 5.170 5.045
#Proportion Var  0.169 0.073 0.066 0.059 0.055 0.050 0.050 0.050 0.047 0.046
#Cumulative Var  0.169 0.242 0.308 0.367 0.423 0.473 0.522 0.572 0.619 0.665
#MR10
#SS loadings    4.885
#Proportion Var 0.045
#Cumulative Var 0.710


## this is another way to look at it, you could extract just these variables and look at them more closely
head(factor2cluster(fa1$loadings))
# head(factor2cluster(fa1$loadings))
#      MR1 MR4 MR8 MR2 MR3 MR9 MR5 MR6 MR11 MR7 MR10
# P048   0   1   0   0   0   0   0   0    0   0    0
# P049   0   0   0   0   0   0   0   0    0  -1    0
# P050   0   0   0   0   0   0   0   0    0   0    1
# P051   0   0   0   0   0   0   1   0    0   0    0
# P052   0   1   0   0   0   0   0   0    0   0    0
# P053   0   0   0   0  -1   0   0   0    0   0    0
factor2cluster(fa1$loadings)[,"MR1",drop=FALSE]


