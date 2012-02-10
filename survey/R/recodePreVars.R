###############################################################################
## recodePreVars.R
##
## recode all of the prenatal variables that were reversed in the original survey
## 
## Author: Haaland
###############################################################################

## you don't need to rerun the original file unless there was a problem of some sort
load("rdata/preData.RData")

#####################################################################################
## correct the ones that are scored in reverse
#####################################################################################
## this is the list of variables based on old names that need to be reverse
## this is for the list of variables that are 1-5
revPreOldNames = c("")
## check to be sure they are all actually legal names
all(revPreOldNames %in% descPreDf$oldName)
# all(revPostOldNames %in% descPostDf$oldName)
# [1] TRUE
## now get the new names
selNamesPre = descPreDf$oldName %in% revPreOldNames
descPreDf[selNamesPre,c("varName","oldName")]
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

revPreNames = as.character(descPreDf$varName[selNamesPre])
revPreNames
# revPostNames
#  [1] "P052" "P064" "P070" "P074" "P077" "P079" "P080" "P084" "P097" "P099"
# [11] "P106" "P107" "P110" "P111" "P114" "P117" "P119" "P121" "P124" "P126"
# [21] "P127" "P130" "P135" "P139" "P142" "P148" "P149"


## keep this just in case you need it
adf = preDf
## safe point
## preDf = adf

## now switch the values
for(i in revPreNames) {
	preDf[,i] = 6-preDf[,i]
}
## just check to be sure this makes sense
head(adf[,revPreNames])
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

head(preDf[,revPreNames])
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
save(preDf,descPreDf,preNumVarNames,file="rdata/preData.RData")




