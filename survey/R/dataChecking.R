###############################################################################
## dataChecking.R
##
## run basic summaries to verify that the data is as expected
## 
## Author: Haaland
###############################################################################

library(psych)

summary(preDf)

describe(preDf[,preNumVarNames])

describe(postDf[,postNumVarNames])
