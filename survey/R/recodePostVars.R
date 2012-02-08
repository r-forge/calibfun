###############################################################################
## recodePostVars.R
##
## recode all of the post partum variables that were reversed in the original survey
## 
## Author: Haaland
###############################################################################

## you don't need to rerun the original file unless there was a problem of some sort
load("rdata/postData.RData")

#####################################################################################
## correct the ones that are scored in reverse
#####################################################################################
## this is the list of variables based on old names that need to be reverse
## this is for the list of variables that are 1-5
## TODO: get the full list of switch variables for 1-5
## -- run this block of code 
## -- update pastes
## -- copy and paste for the 1-3 variables, or any other groups
## -- when you are convinced it is right, save the revised data
## -- make a new file for recodePreVars.R and do the same
## -- be sure to rerun the plotHistograms.R and individualAnalyses.R
revPostOldNames = c("Q37.1","Q47_3","Q47_9")
## check to be sure they are all actually legal names
all(revPostOldNames %in% descPostDf$oldName)
# all(revPostOldNames %in% descPostDf$oldName)
# [1] TRUE
## now get the new names
selNames = descPostDf$oldName %in% revPostOldNames
descPostDf[selNames,c("varName","oldName")]
# descPostDf[selNames,c("varName","oldName")]
#      varName oldName
# P052    P052   Q37.1
# P064    P064   Q47_3
# P070    P070   Q47_9

revPostNames = as.character(descPostDf$varName[selNames])
revPostNames
# revPostNames
# [1] "P052" "P064" "P070"

## keep this just in case you need it
adf = postDf
## safe point
## postDf = adf

## now switch the values
for(i in revPostNames) {
	postDf[,i] = 6-postDf[,i]
}
## just check to be sure this makes sense
head(adf[,revPostNames])
# head(adf[,revPostNames])
#   P052 P064 P070
# 2    5    4    5
# 3    5    1    5
# 4    5    3    3
# 5    5    2    5
# 6    5    1    4
# 7    5    1    5


head(postDf[,revPostNames])
# head(postDf[,revPostNames])
#   P052 P064 P070
# 2    1    2    1
# 3    1    5    1
# 4    1    3    3
# 5    1    4    1
# 6    1    5    2
# 7    1    5    1


#########################################################################
## save what we have just done
#########################################################################
## this overwrites the original, so if you make a big mistake
## you'll have to run the code to remake the dataframe
save(postDf,descPostDf,postNumVarNames,file="rdata/postData.RData")


