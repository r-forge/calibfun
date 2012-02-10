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
revPostOldNames = c("Q37.1","Q47_3","Q47_9","Q48_3","Q48_6","Q48_8",
		"Q48_9","Q50_3","Q51_6","Q51_8","Q52_5","Q52_6","Q52_9",
		"Q52_10","Q53_3","Q53_6","Q53_8","Q53_10","Q54_3","Q54_5",
		"Q54_6","Q54_9","Q55_4","Q55_8","Q56_1","Q56_7","Q57.1")
## check to be sure they are all actually legal names
all(revPostOldNames %in% descPostDf$oldName)
# all(revPostOldNames %in% descPostDf$oldName)
# [1] TRUE
## now get the new names
selNames = descPostDf$oldName %in% revPostOldNames
descPostDf[selNames,c("varName","oldName")]
# descPostDf[selNames,c("varName","oldName")]
# 		varName oldName
# P052    P052   Q37.1
# P064    P064   Q47_3
# P070    P070   Q47_9
# P074    P074   Q48_3
# P077    P077   Q48_6
# P079    P079   Q48_8
# P080    P080   Q48_9
# P084    P084   Q50_3
# P097    P097   Q51_6
# P099    P099   Q51_8
# P106    P106   Q52_5
# P107    P107   Q52_6
# P110    P110   Q52_9
# P111    P111  Q52_10
# P114    P114   Q53_3
# P117    P117   Q53_6
# P119    P119   Q53_8
# P121    P121  Q53_10
# P124    P124   Q54_3
# P126    P126   Q54_5
# P127    P127   Q54_6
# P130    P130   Q54_9
# P135    P135   Q55_4
# P139    P139   Q55_8
# P142    P142   Q56_1
# P148    P148   Q56_7
# P149    P149   Q57.1

revPostNames = as.character(descPostDf$varName[selNames])
revPostNames
# revPostNames
#  [1] "P052" "P064" "P070" "P074" "P077" "P079" "P080" "P084" "P097" "P099"
# [11] "P106" "P107" "P110" "P111" "P114" "P117" "P119" "P121" "P124" "P126"
# [21] "P127" "P130" "P135" "P139" "P142" "P148" "P149"


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
#   P052 P064 P070 P074 P077 P079 P080 P084 P097 P099 P106 P107 P110 P111 P114
# 2    5    4    5    2    4    2    3    5    5    4    5    4    4    4    5
# 3    5    1    5    3    5    1    3    3    5    3    2    3    3    4    5
# 4    5    3    3    4    4    3    3    2    4    4    3    4    4    5    4
# 5    5    2    5    2    5    1    2    4    4    3    4    4    4    4    3
# 6    5    1    4    3    5    4    4    3    5    5    4    3    5    5    5
# 7    5    1    5    2    5    1    3    1    3    2    1    4    2    2    1
#   P117 P119 P121 P124 P126 P127 P130 P135 P139 P142 P148 P149
# 2    3    4    4    4    3    2    3    4    4    5    3    4
# 3    5    5    3    4    5    4    2    4    2    4    3    3
# 4    2    2    3    2    4    2    2    5    2    3    2    3
# 5    2    3    4    1    4    2    2    4    2    3    2    3
# 6    4    4    5    4    5    3    3    5    4    4    3    4
# 7    1    1    1    2    4    2    2    2    1    1    2    1

head(postDf[,revPostNames])
# head(postDf[,revPostNames])
#   P052 P064 P070 P074 P077 P079 P080 P084 P097 P099 P106 P107 P110 P111 P114
# 2    1    2    1    4    2    4    3    1    1    2    1    2    2    2    1
# 3    1    5    1    3    1    5    3    3    1    3    4    3    3    2    1
# 4    1    3    3    2    2    3    3    4    2    2    3    2    2    1    2
# 5    1    4    1    4    1    5    4    2    2    3    2    2    2    2    3
# 6    1    5    2    3    1    2    2    3    1    1    2    3    1    1    1
# 7    1    5    1    4    1    5    3    5    3    4    5    2    4    4    5
#   P117 P119 P121 P124 P126 P127 P130 P135 P139 P142 P148 P149
# 2    3    2    2    2    3    4    3    2    2    1    3    2
# 3    1    1    3    2    1    2    4    2    4    2    3    3
# 4    4    4    3    4    2    4    4    1    4    3    4    3
# 5    4    3    2    5    2    4    4    2    4    3    4    3
# 6    2    2    1    2    1    3    3    1    2    2    3    2
# 7    5    5    5    4    2    4    4    4    5    5    4    5


#########################################################################
## save what we have just done
#########################################################################
## this overwrites the original, so if you make a big mistake
## you'll have to run the code to remake the dataframe
save(postDf,descPostDf,postNumVarNames,file="rdata/postData.RData")

#########################################################################
## now that all the varialbes go in the same direction, reverse them
## so that higher is always better
#########################################################################
# two-level scales
twoLevelVars=c("P051","P054")
sixLevelVars=c("P053","P055","P154","P155","P156")
eightLevelVars=c("P056")
timeVars=c("P152","P153")
(1:ncol(postDf))[names(postDf) %in% c("P048","P156")]
# (1:ncol(postDf))[names(postDf) %in% c("P048","P156")]
# [1]  49 157
changeDirVars=names(postDf)[49:157]
fiveLevelVars=changeDirVars[!(changeDirVars %in% c(twoLevelVars,
							sixLevelVars,eightLevelVars,timeVars))]
fiveLevelVars
# fiveLevelVars
#  [1] "P048" "P049" "P050" "P052" "P057" "P058" "P059" "P060" "P061" "P062"
# [11] "P063" "P064" "P065" "P066" "P067" "P068" "P069" "P070" "P071" "P072"
# [21] "P073" "P074" "P075" "P076" "P077" "P078" "P079" "P080" "P081" "P082"
# [31] "P083" "P084" "P085" "P086" "P087" "P088" "P089" "P090" "P091" "P092"
# [41] "P093" "P094" "P095" "P096" "P097" "P098" "P099" "P100" "P101" "P102"
# [51] "P103" "P104" "P105" "P106" "P107" "P108" "P109" "P110" "P111" "P112"
# [61] "P113" "P114" "P115" "P116" "P117" "P118" "P119" "P120" "P121" "P122"
# [71] "P123" "P124" "P125" "P126" "P127" "P128" "P129" "P130" "P131" "P132"
# [81] "P133" "P134" "P135" "P136" "P137" "P138" "P139" "P140" "P141" "P142"
# [91] "P143" "P144" "P145" "P146" "P147" "P148" "P149" "P150" "P151"

## now actually change the directions (but not for timeVars)
for (i in twoLevelVars){
	postDf[,i]=2-postDf[,i]+1
}
for (i in sixLevelVars){
	postDf[,i]=6-postDf[,i]+1
}
for (i in eightLevelVars){
	postDf[,i]=8-postDf[,i]+1
}
for (i in fiveLevelVars){
	postDf[,i]=5-postDf[,i]+1
}

save(postDf,descPostDf,postNumVarNames,file="rdata/postData.RData")
