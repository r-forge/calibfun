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

themeNames = c("P087","P100","P106","P112","P121","P048","P051",
		"P083","P102","P110","P063","P076","P092","P095","P097","P098",
		"P109","P113","P114","P138","P126","P132","P135","P141","P143",
		"P144","P146","P058","P071","P072","P105","P116","P119",
		"P120","P139","P147","P067","P081","P084","P088","P091",
		"P125","P128","P134","P140","P057","P099","P103")
themeDf = postDf[,themeNames]
head(themeDf)
themeMat = as.matrix(themeDf)
themeMat = scale(themeMat)
head(themeMat)

VSS.scree(cor(themeDf))
#########################################################################
## based on scree plot, choose 2 factors ?
#########################################################################
faTheme = fa(themeMat,nfactors=2,rotate="varimax")
faTheme 
#                  MR1  MR2
# SS loadings    11.45 4.70
# Proportion Var  0.24 0.10
# Cumulative Var  0.24 0.34
# 
# Test of the hypothesis that 2 factors are sufficient.
# 
# The degrees of freedom for the null model are  1128  and the objective function was  531.26 with Chi Square of  9119.99
# The degrees of freedom for the model are 1033  and the objective function was  NaN 
# 
# The root mean square of the residuals is  0.09 
# The df corrected root mean square of the residuals is  0.14 
# The number of observations was  35  with Chi Square =  NaN  with prob <  NaN 
# 

names(faTheme)
# names(faUnassigned)
#  [1] "residual"     "dof"          "fit"          "fit.off"      "sd"          
#  [6] "crms"         "rms"          "factors"      "n.obs"        "objective"   
# [11] "criteria"     "STATISTIC"    "PVAL"         "Call"         "null.model"  
# [16] "null.dof"     "null.chisq"   "TLI"          "RMSEA"        "BIC"         
# [21] "r.scores"     "R2"           "valid"        "score.cor"    "weights"     
# [26] "communality"  "uniquenesses" "values"       "e.values"     "loadings"    
# [31] "fm"           "scores"       "fn"          
## these are the new factors
head(faTheme$scores)
# head(faTheme$scores)
#           MR1        MR3        MR5        MR2       MR4       MR6
# 2   2.8438292  0.7587461   3.244906   3.319005 -1.848162  3.254639
# 3   1.9625234  4.8210927   4.310324  -1.805994  3.214959  3.624243
# 4  -9.7022773 -4.4928814  -5.327772  -2.469097  3.570236 -4.079281
# 5  -2.5214733 -2.3722987  -2.123335  -3.192580  3.552425 -1.760634
# 6  -0.2935592  1.3417107   7.037938   2.838687 -5.966997 -1.176163
# 7 -17.3281417 -3.5343114 -11.943978 -13.279851 -3.541905 -2.671103

## add the new factors to the original matrix,
## I'm not going to do this permanently right now
faThemeDf = cbind(postDf,faTheme$scores)
dim(faThemeDf)
# dim(faThemeDf)
# [1]  35 212
names(faThemeDf)

## what does the factor represent?
## pick the variables that have the largest absolute values
## MR2 represents, e.g., P030, P032, P036, P037, ...
## then you make up a story and a name
faTheme$loadings
# faTheme$loadings
#                   MR1   MR2
# SS loadings    11.451 4.696
# Proportion Var  0.239 0.098
# Cumulative Var  0.239 0.336


## this is another way to look at it, you could extract just these 
## variables and look at them more closely
head(factor2cluster(faTheme$loadings))
# head(factor2cluster(faTheme$loadings))
#      MR1 MR2
# P087   1   0
# P100   1   0
# P106   1   0
# P112   1   0
# P121   1   0
# P048   1   0


## could I recall only the questions = 1 and make a table?
# These variables won't be accurate b/c they're being written
# over with each factor analysis done
MR1 = factor2cluster(faTheme$loadings)[,"MR1",drop=FALSE]
MR2 = factor2cluster(faTheme$loadings)[,"MR2",drop=FALSE]
MR3 = factor2cluster(faTheme$loadings)[,"MR3",drop=FALSE]
MR4 = factor2cluster(faTheme$loadings)[,"MR4",drop=FALSE]
MR5 = factor2cluster(faTheme$loadings)[,"MR5",drop=FALSE]

## copied from corrlations below to aid in recording factor ?s
## sQFa5GroupsDf should stay accurate as long as I don't re-run it
unassignedFaGroupsDf = data.frame(MR1 = MR1, MR2 = MR2, MR3 = MR3, MR4 = MR4,
		MR5 = MR5)
unassignedFaGroupsDf

