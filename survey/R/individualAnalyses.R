###############################################################################
## individualAnalyses.R
##
## Basic descriptive statistics and individual comparisons
##		 and analyses 
##
## Author: Haaland
###############################################################################

## do descriptive statistics for all questions
# first, create a data.frame with only numeric responses
numPreDf <- preDf[,preNumVarNames]
#calculate mean and standard deviation and standard error
sumFun = function(x) {
	data.frame(Mean=round(mean(x),2),
			SD = round(sd(x),2), 
			SE = round(sd(x)/sqrt(length(x)),2))
}


sumPreDf = sapply(numPreDf,sumFun)
head(sumPreDf)
sumPreDf = t(sumPreDf)
head(sumPreDf)
# head(sumPreDf)
#      Mean SD   SE  
# N001 1.67 0.78 0.22
# N003 1    0    0   
# N004 3    1.04 0.3 
# N006 5    1.6  0.46
# N007 1    0    0   
# N008 4.83 3.88 1.12

# first, create a data.frame with only numeric responses
numPostDf <- postDf[,postNumVarNames]
#calculate mean and standard deviation
sumPostDf = t(sapply(numPostDf,sumFun))
head(sumPostDf)
# head(sumPostDf)
#      Mean SD   SE  
# P001 2.09 1.38 0.29
# P003 1    0    0   
# P004 2.48 0.9  0.19
# P006 4.7  1.33 0.28
# P007 1    0    0   
# P008 5.61 3.63 0.76

## look at the data and determine the questions that have no 
## standard deviation
# P003, P007, P024

#########################################################################
## create a rating of how fully each woman entered laborland
#########################################################################


#########################################################################
## group questions into themes and create composite score
#########################################################################

