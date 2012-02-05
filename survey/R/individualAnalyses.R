###############################################################################
## individualAnalyses.R
##
## TODO: Basic descriptive statistics and individual comparisons
##		 and analyses 
##
## Author: Haaland
###############################################################################

## do descriptive statistics for all questions
# first, create a data.frame with only numeric responses
numPreDf <- preDf[-c(1:3,5,8,17)]
#calculate mean and standard deviation
describe(numPreDf)
sapply(numPreDf, mean)
sapply(numPreDf, sd)

# first, create a data.frame with only numeric responses
numPostDf <- postDf[-c(1:3,5,8,17)]
#calculate mean and standard deviation
describe(numPostDf)
sapply(numPostDf, mean)
sapply(numPostDf, sd)


#########################################################################
## create a rating of how fully each woman entered laborland
#########################################################################


#########################################################################
## group questions into themes and create composite score
#########################################################################

