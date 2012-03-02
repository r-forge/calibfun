################################################################################
## factorAnalysisExample.R
##
## run a factor analysis on the unassigned questions to see if there
## are other themes at play
## 
## Author: Haaland
###############################################################################
library(ggplot2)
library(psych)
library(GPArotation)
load("rdata/postData.RData")

names(postDf)

unassignedNames = c("P049","P057","P058","P062","P068","P069",
		"P073","P074","P075","P078","P080","P081","P082","P083",
		"P091","P104","P112","P113","P118","P120","P121","P129",
		"P131","P137")
unassignedDf = postDf[,unassignedNames]
head(unassignedDf)
unassignedMat = as.matrix(unassignedDf)
unassignedMat = scale(unassignedMat)
head(unassignedMat)

VSS.scree(cor(unassignedDf))
#########################################################################
## based on scree plot, choose 5 factors
#########################################################################
faUnassigned = fa(unassignedMat,nfactors=5,rotate="varimax")
faUnassigned
# The degrees of freedom for the null model are  276  and the objective function was  21.86 with Chi Square of  353.43
# The degrees of freedom for the model are 166  and the objective function was  14.21 
# 
# The root mean square of the residuals is  0.07 
# The df corrected root mean square of the residuals is  0.13 
# The number of observations was  26  with Chi Square =  182.41  with prob <  0.18 
names(faUnassigned)
# names(faUnassigned)
#  [1] "residual"     "dof"          "fit"          "fit.off"      "sd"          
#  [6] "crms"         "rms"          "factors"      "n.obs"        "objective"   
# [11] "criteria"     "STATISTIC"    "PVAL"         "Call"         "null.model"  
# [16] "null.dof"     "null.chisq"   "TLI"          "RMSEA"        "BIC"         
# [21] "r.scores"     "R2"           "valid"        "score.cor"    "weights"     
# [26] "communality"  "uniquenesses" "values"       "e.values"     "loadings"    
# [31] "fm"           "scores"       "fn"          
## these are the new factors
head(faUnassigned$scores)
# head(faUnassigned$scores)
#          MR3         MR5         MR1        MR2         MR4
# 2  0.2654729  0.40357956 -0.74856080  0.5659095  0.68387813
# 3  1.1761356  1.57782311 -0.04222596 -1.4374735  0.21444720
# 4  0.4816785 -0.01037843  0.27033732 -1.2034104  0.02431296
# 5  0.4629572  0.25725356 -0.14362322  0.8073276 -3.12447209
# 6  0.3670064 -1.19643137 -0.19067246 -2.1099421  0.17514043
# 7 -0.9189983  0.06441476 -1.12599654  0.1620317  0.43894096

## add the new factors to the original matrix,
## I'm not going to do this permanently right now
faUnassignedDf = cbind(postDf,faUnassigned$scores)
dim(faUnassignedDf)
# dim(faUnassignedDf)
# [1]  26 174
names(faUnassignedDf)

## what does the factor represent?
## pick the variables that have the largest absolute values
## MR2 represents, e.g., P030, P032, P036, P037, ...
## then you make up a story and a name
faUnassigned$loadings
# faUnassigned$loadings
#                  MR3   MR5   MR1   MR2   MR4
# SS loadings    2.642 2.236 2.199 2.097 1.790
# Proportion Var 0.110 0.093 0.092 0.087 0.075
# Cumulative Var 0.110 0.203 0.295 0.382 0.457

## this is another way to look at it, you could extract just these 
## variables and look at them more closely
head(factor2cluster(faUnassigned$loadings))
# head(factor2cluster(faUnassigned$loadings))
#      MR3 MR5 MR1 MR2 MR4
# P049   0   0   0  -1   0
# P057   0   1   0   0   0
# P058   0   0   0   0  -1
# P062   0   0   0   1   0
# P068   1   0   0   0   0
# P069   0   0   0   0   1

## could I recall only the questions = 1 and make a table?
# These variables won't be accurate b/c they're being written
# over with each factor analysis done
MR1 = factor2cluster(faUnassigned$loadings)[,"MR1",drop=FALSE]
MR2 = factor2cluster(faUnassigned$loadings)[,"MR2",drop=FALSE]
MR3 = factor2cluster(faUnassigned$loadings)[,"MR3",drop=FALSE]
MR4 = factor2cluster(faUnassigned$loadings)[,"MR4",drop=FALSE]
MR5 = factor2cluster(faUnassigned$loadings)[,"MR5",drop=FALSE]

## copied from corrlations below to aid in recording factor ?s
## sQFa5GroupsDf should stay accurate as long as I don't re-run it
unassignedFaGroupsDf = data.frame(MR1 = MR1, MR2 = MR2, MR3 = MR3, MR4 = MR4,
		MR5 = MR5)
unassignedFaGroupsDf

