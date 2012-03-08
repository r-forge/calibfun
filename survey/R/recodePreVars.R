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
revPreOldNames = c("Q16_2","Q16_4","Q16_6","Q16_7","Q16_8","Q16_11",
		"Q16_13","Q16_15","Q16_18","Q16_20","Q17_1","Q17_2","Q17_5",
		"Q17_6","Q17_9","Q17_10","Q18_1","Q19_7","Q19_8","Q19_9",
		"Q32_5","Q32_6","Q32_7","Q32_8","Q33_1","Q33_3","Q33_7",
		"Q33_8","Q34_6")
## check to be sure they are all actually legal names
all(revPreOldNames %in% descPreDf$oldName)
# all(revPostOldNames %in% descPostDf$oldName)
# [1] TRUE
## now get the new names
selNamesPre = descPreDf$oldName %in% revPreOldNames
descPreDf[selNamesPre,c("varName","oldName")]
# descPreDf[selNamesPre,c("varName","oldName")]
#      varName oldName
# N016    N016   Q16_2
# N018    N018   Q16_4
# N020    N020   Q16_6
# N021    N021   Q16_7
# N022    N022   Q16_8
# N025    N025  Q16_11
# N027    N027  Q16_13
# N029    N029  Q16_15
# N032    N032  Q16_18
# N034    N034  Q16_20
# N035    N035   Q17_1
# N036    N036   Q17_2
# N039    N039   Q17_5
# N040    N040   Q17_6
# N043    N043   Q17_9
# N044    N044  Q17_10
# N045    N045   Q18_1
# N061    N061   Q19_7
# N062    N062   Q19_8
# N063    N063   Q19_9
# N080    N080   Q32_5
# N081    N081   Q32_6
# N082    N082   Q32_7
# N083    N083   Q32_8
# N086    N086   Q33_1
# N088    N088   Q33_3
# N092    N092   Q33_7
# N093    N093   Q33_8
# N099    N099   Q34_6

revPreNames = as.character(descPreDf$varName[selNamesPre])
revPreNames
# revPreNames
#  [1] "N016" "N018" "N020" "N021" "N022" "N025" "N027" "N029" "N032" "N034"
# [11] "N035" "N036" "N039" "N040" "N043" "N044" "N045" "N061" "N062" "N063"
# [21] "N080" "N081" "N082" "N083" "N086" "N088" "N092" "N093" "N099"


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
# head(adf[,revPreNames])
#   N016 N018 N020 N021 N022 N025 N027 N029 N032 N034 N035 N036 N039 N040 N043
# 2    2    2    1    1    1    2    1    3    2    1    4    4    2    2    4
# 3    2    1    1    3    1    2    1    3    1    2    4    4    3    2    4
# 4    2    2    1    1    1    2    1    2    1    1    5    5    5    4    5
# 5    2    2    2    2    1    3    2    2    1    2    5    4    4    2    4
# 6    1    2    2    2    1    2    2    1    1    2    4    2    2    3    3
# 7    2    2    1    2    1    3    1    2    1    1    5    5    4    2    3
#   N044 N045 N061 N062 N063 N080 N081 N082 N083 N086 N088 N092 N093 N099
# 2    4    3    4    4    3    4    5    4    4    3    4    4    4    4
# 3    4    2    5    5    4    4    5    4    3    2    4    4    3    4
# 4    5    4    5    5    5    4    5    5    4    4    2    4    5    4
# 5    4    3    4    4    5    3    4    5    4    3    4    5    4    5
# 6    2    2    4    3    3    4    4    3    3    2    3    3    4    2
# 7    3    3    5    5    5    3    4    4    4    4    2    4    5    4

head(preDf[,revPreNames])
# head(preDf[,revPreNames])
#   N016 N018 N020 N021 N022 N025 N027 N029 N032 N034 N035 N036 N039 N040 N043
# 2    4    4    5    5    5    4    5    3    4    5    2    2    4    4    2
# 3    4    5    5    3    5    4    5    3    5    4    2    2    3    4    2
# 4    4    4    5    5    5    4    5    4    5    5    1    1    1    2    1
# 5    4    4    4    4    5    3    4    4    5    4    1    2    2    4    2
# 6    5    4    4    4    5    4    4    5    5    4    2    4    4    3    3
# 7    4    4    5    4    5    3    5    4    5    5    1    1    2    4    3
#   N044 N045 N061 N062 N063 N080 N081 N082 N083 N086 N088 N092 N093 N099
# 2    2    3    2    2    3    2    1    2    2    3    2    2    2    2
# 3    2    4    1    1    2    2    1    2    3    4    2    2    3    2
# 4    1    2    1    1    1    2    1    1    2    2    4    2    1    2
# 5    2    3    2    2    1    3    2    1    2    3    2    1    2    1
# 6    4    4    2    3    3    2    2    3    3    4    3    3    2    4
# 7    3    3    1    1    1    3    2    2    2    2    4    2    1    2


#########################################################################
## save what we have just done
#########################################################################
## this overwrites the original, so if you make a big mistake
## you'll have to run the code to remake the dataframe
save(preDf,descPreDf,preNumVarNames,file="rdata/preData.RData")


#########################################################################
## now that all the variables go in the same direction, reverse them
## so that higher is always better
#########################################################################
# two-level scales
twoLevelVars=c("N067","N072")
threeLevelVars=c("N065","N066")
fourLevelVars=c("N068")
(1:ncol(preDf))[names(preDf) %in% c("N035","N101")]
# (1:ncol(preDf))[names(preDf) %in% c("N035","N101")]
# [1]  36 102
changeDirVars=names(preDf)[36:102]
fiveLevelVars=changeDirVars[!(changeDirVars %in% c(twoLevelVars,
							threeLevelVars,fourLevelVars))]
fiveLevelVars
# fiveLevelVars
#  [1] "N035" "N036" "N037" "N038" "N039" "N040" "N041" "N042" "N043" "N044"
# [11] "N045" "N046" "N047" "N048" "N049" "N050" "N051" "N052" "N053" "N054"
# [21] "N055" "N056" "N057" "N058" "N059" "N060" "N061" "N062" "N063" "N064"
# [31] "N069" "N070" "N071" "N073" "N074" "N075" "N076" "N077" "N078" "N079"
# [41] "N080" "N081" "N082" "N083" "N084" "N085" "N086" "N087" "N088" "N089"
# [51] "N090" "N091" "N092" "N093" "N094" "N095" "N096" "N097" "N098" "N099"
# [61] "N100" "N101"


## now actually change the directions
for (i in twoLevelVars){
	preDf[,i]=2-preDf[,i]+1
}
for (i in threeLevelVars){
	preDf[,i]=3-preDf[,i]+1
}
for (i in fourLevelVars){
	preDf[,i]=4-preDf[,i]+1
}
for (i in fiveLevelVars){
	preDf[,i]=5-preDf[,i]+1
}

save(preDf,descPreDf,preNumVarNames,file="rdata/preData.RData")

