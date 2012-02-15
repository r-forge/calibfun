################################################################################
## factorAnalysisExample.R
##
## work out some details of a factor analysis as an example
## 
## Author: Haaland
###############################################################################
library(ggplot2)
library(psych)
load("rdata/postData.RData")

names(postDf)

## pick an arbitrary set to get started with
vnames = names(postDf)[30:50]
adf = postDf[,vnames]
head(adf)
amat = as.matrix(adf)
head(amat)

fa1 = fa(amat,nfactors=2,rotate="varimax")
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
#          MR2         MR1
# 2  1.1449550  1.22422978
# 3 -1.8716872 -1.26462926
# 4 -1.0252414 -0.55995901
# 5 -0.2754741  0.06552535
# 6 -0.7199737 -0.11155597
# 7  0.1500117  2.07831354

## add the new factors to the original matrix,
## I'm not going to do this permanently right now
pdf = cbind(postDf,fa1$scores)
dim(pdf)
# [1]  23 167
names(pdf)

quartz()
ggplot(pdf,aes(x=MR1,y=laborLand)) + geom_point()
## this shows that one of the factors is "somewhat" correlated with laborLand
ggplot(pdf,aes(x=MR2,y=laborLand)) + geom_point() + geom_smooth(method=lm)

## what does the factor represent?
## pick the variables that have the largest absolute values
## MR2 represents, e.g., P030, P032, P036, P037, ...
## then you make up a story and a name
fa1$loadings
# 
# Loadings:
#      MR2    MR1   
# P029 -0.268  0.492
# P030  0.779       
# P031 -0.220  0.744
# P032  0.669 -0.188
# P033 -0.348  0.433
# P034 -0.307  0.790
# P035 -0.305  0.658
# P036  0.864 -0.251
# P037  0.769       
# P038  0.132  0.609
# P039  0.750       
# P040 -0.314  0.455
# P041  0.845 -0.258
# P042         0.727
# P043  0.584       
# P044  0.519       
# P045  0.101  0.680
# P046  0.754       
# P047         0.464
# P048  0.260       
# P049 -0.157  0.270
# 
#                  MR2   MR1
# SS loadings    5.508 4.088
# Proportion Var 0.262 0.195
# Cumulative Var 0.262 0.457

## this is another way to look at it, you could extract just these variables and look at them more closely
factor2cluster(fa1$loadings)
#      MR2 MR1
# P029   0   1
# P030   1   0
# P031   0   1
# P032   1   0
# P033   0   1
# P034   0   1
# P035   0   1
# P036   1   0
# P037   1   0
# P038   0   1
# P039   1   0
# P040   0   1
# P041   1   0
# P042   0   1
# P043   1   0
# P044   1   0
# P045   0   1
# P046   1   0
# P047   0   1
# P048   1   0
# P049   0   1
