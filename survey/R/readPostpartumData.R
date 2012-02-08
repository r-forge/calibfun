##############################################################################
## readPostpartumData.R
##
## read the data table (csv) and make a data frame for the postpartum survey
## this should be nearly the same as the prenatal suvey
## 
## Author: Haaland
###############################################################################

## read the csv file as exported from the survey software
## postfile is set in the driver.R file

postDf = read.csv(postfile,header=TRUE,as.is=TRUE)
## this shows how many rows (subjects) and columns (questions)
dim(postDf)
# dim(postDf)
# [1]  24 166

names(postDf)
#   [1] "X...V1" "V2"     "V3"     "V4"     "V5"     "Q27"    "Q29"    "Q31"   
#   [9] "Q33"    "Q35"    "Q37"    "Q39"    "Q41"    "Q43"    "Q45"    "Q47"   
#  [17] "Q49"    "Q51"    "Q53"    "Q55"    "Q57"    "Q59"    "Q19"    "Q20"   
#  [25] "Q21"    "Q22"    "Q23"    "Q24"    "Q25"    "Q26"    "Q27.1"  "Q28"   
#  [33] "Q29.1"  "Q30"    "Q31.1"  "Q61_1"  "Q61_2"  "Q61_3"  "Q61_4"  "Q61_5" 
#  [41] "Q61_6"  "Q61_7"  "Q61_8"  "Q61_9"  "Q61_10" "Q61_11" "Q61_12" "Q61_13"
#  [49] "Q61_14" "Q61_15" "Q61_16" "Q61_17" "Q61_18" "Q61_19" "Q61_20" "Q32"   
#  [57] "Q33.1"  "Q34"    "Q35.1"  "Q36"    "Q37.1"  "Q38"    "Q39.1"  "Q40"   
#  [65] "Q41.1"  "Q42"    "Q43.1"  "Q44"    "Q45.1"  "Q46"    "Q47_1"  "Q47_2" 
#  [73] "Q47_3"  "Q47_4"  "Q47_5"  "Q47_6"  "Q47_7"  "Q47_8"  "Q47_9"  "Q47_10"
#  [81] "Q48_1"  "Q48_2"  "Q48_3"  "Q48_4"  "Q48_5"  "Q48_6"  "Q48_7"  "Q48_8" 
#  [89] "Q48_9"  "Q48_10" "Q50_1"  "Q50_2"  "Q50_3"  "Q50_4"  "Q50_5"  "Q50_6" 
#  [97] "Q50_7"  "Q50_8"  "Q50_9"  "Q50_10" "Q51_1"  "Q51_2"  "Q51_3"  "Q51_4" 
# [105] "Q51_5"  "Q51_6"  "Q51_7"  "Q51_8"  "Q51_9"  "Q51_10" "Q52_1"  "Q52_2" 
# [113] "Q52_3"  "Q52_4"  "Q52_5"  "Q52_6"  "Q52_7"  "Q52_8"  "Q52_9"  "Q52_10"
# [121] "Q53_1"  "Q53_2"  "Q53_3"  "Q53_4"  "Q53_5"  "Q53_6"  "Q53_7"  "Q53_8" 
# [129] "Q53_9"  "Q53_10" "Q54_1"  "Q54_2"  "Q54_3"  "Q54_4"  "Q54_5"  "Q54_6" 
# [137] "Q54_7"  "Q54_8"  "Q54_9"  "Q54_10" "Q55_1"  "Q55_2"  "Q55_3"  "Q55_4" 
# [145] "Q55_5"  "Q55_6"  "Q55_7"  "Q55_8"  "Q55_9"  "Q55_10" "Q56_1"  "Q56_2" 
# [153] "Q56_3"  "Q56_4"  "Q56_5"  "Q56_6"  "Q56_7"  "Q57.1"  "Q58"    "Q59.1" 
# [161] "Q60"    "Q61"    "Q62"    "Q63"    "Q64"    "Q66"   
## not sure why the first name is funny
names(postDf)[1]="V1"



#####################################################################################
## a few of these aren't going to be analyzed
#####################################################################################
postDf = postDf[,!(names(postDf) %in% c("V2","V3","V4","V5","Q27","Q29","Q31","Q32","Q66"))]
names(postDf)
dim(postDf)		
# dim(postDf)
# [1]  24 157


				
#####################################################################################
## the variables names don't have any particular meaning, so
## I'm just going to set them sequentially
## Note: the first 5 columns are going to get separate names
#####################################################################################
nvars = ncol(postDf)-1
nvars
# nvars
# [1] 156


## we are going to start with 1,
nstart = 1
nstart
# nstart
# [1] 1


newPostNames = c(paste("P00",1:9,sep=""),paste("P0",10:99,sep=""),paste("P",100:nvars,sep=""))
newPostNames
# newPostNames
#   [1] "P001" "P002" "P003" "P004" "P005" "P006" "P007" "P008" "P009" "P010"
#  [11] "P011" "P012" "P013" "P014" "P015" "P016" "P017" "P018" "P019" "P020"
#  [21] "P021" "P022" "P023" "P024" "P025" "P026" "P027" "P028" "P029" "P030"
#  [31] "P031" "P032" "P033" "P034" "P035" "P036" "P037" "P038" "P039" "P040"
#  [41] "P041" "P042" "P043" "P044" "P045" "P046" "P047" "P048" "P049" "P050"
#  [51] "P051" "P052" "P053" "P054" "P055" "P056" "P057" "P058" "P059" "P060"
#  [61] "P061" "P062" "P063" "P064" "P065" "P066" "P067" "P068" "P069" "P070"
#  [71] "P071" "P072" "P073" "P074" "P075" "P076" "P077" "P078" "P079" "P080"
#  [81] "P081" "P082" "P083" "P084" "P085" "P086" "P087" "P088" "P089" "P090"
#  [91] "P091" "P092" "P093" "P094" "P095" "P096" "P097" "P098" "P099" "P100"
# [101] "P101" "P102" "P103" "P104" "P105" "P106" "P107" "P108" "P109" "P110"
# [111] "P111" "P112" "P113" "P114" "P115" "P116" "P117" "P118" "P119" "P120"
# [121] "P121" "P122" "P123" "P124" "P125" "P126" "P127" "P128" "P129" "P130"
# [131] "P131" "P132" "P133" "P134" "P135" "P136" "P137" "P138" "P139" "P140"
# [141] "P141" "P142" "P143" "P144" "P145" "P146" "P147" "P148" "P149" "P150"
# [151] "P151" "P152" "P153" "P154" "P155" "P156"


oldPostNames = names(postDf)[-1]
names(postDf)[-1] = newPostNames
names(postDf)[1] = "ResponseId"
postDf[1,1:3]
names(postDf)
# names(postDf)
#   [1] "ResponseId" "P001"       "P002"       "P003"       "P004"      
#   [6] "P005"       "P006"       "P007"       "P008"       "P009"      
#  [11] "P010"       "P011"       "P012"       "P013"       "P014"      
#  [16] "P015"       "P016"       "P017"       "P018"       "P019"      
#  [21] "P020"       "P021"       "P022"       "P023"       "P024"      
#  [26] "P025"       "P026"       "P027"       "P028"       "P029"      
#  [31] "P030"       "P031"       "P032"       "P033"       "P034"      
#  [36] "P035"       "P036"       "P037"       "P038"       "P039"      
#  [41] "P040"       "P041"       "P042"       "P043"       "P044"      
#  [46] "P045"       "P046"       "P047"       "P048"       "P049"      
#  [51] "P050"       "P051"       "P052"       "P053"       "P054"      
#  [56] "P055"       "P056"       "P057"       "P058"       "P059"      
#  [61] "P060"       "P061"       "P062"       "P063"       "P064"      
#  [66] "P065"       "P066"       "P067"       "P068"       "P069"      
#  [71] "P070"       "P071"       "P072"       "P073"       "P074"      
#  [76] "P075"       "P076"       "P077"       "P078"       "P079"      
#  [81] "P080"       "P081"       "P082"       "P083"       "P084"      
#  [86] "P085"       "P086"       "P087"       "P088"       "P089"      
#  [91] "P090"       "P091"       "P092"       "P093"       "P094"      
#  [96] "P095"       "P096"       "P097"       "P098"       "P099"      
# [101] "P100"       "P101"       "P102"       "P103"       "P104"      
# [106] "P105"       "P106"       "P107"       "P108"       "P109"      
# [111] "P110"       "P111"       "P112"       "P113"       "P114"      
# [116] "P115"       "P116"       "P117"       "P118"       "P119"      
# [121] "P120"       "P121"       "P122"       "P123"       "P124"      
# [126] "P125"       "P126"       "P127"       "P128"       "P129"      
# [131] "P130"       "P131"       "P132"       "P133"       "P134"      
# [136] "P135"       "P136"       "P137"       "P138"       "P139"      
# [141] "P140"       "P141"       "P142"       "P143"       "P144"      
# [146] "P145"       "P146"       "P147"       "P148"       "P149"      
# [151] "P150"       "P151"       "P152"       "P153"       "P154"      
# [156] "P155"       "P156"      


#####################################################################################
## I'm going to use vdesc to get reasonable descriptions later
## it needs to be removed now so the dataframe can be fixed up
#####################################################################################
postdesc = unlist(postDf[1,])
## this is the actual 88th question
postdesc[88]
# postdesc[88]
#                                                                                                                                                     Q187 
# "Please rate how strongly you agree or disagree with each of the following statements by selecting th...-My birth did not go according to my birth plan" 
# > cat("Synch1324866261257190000\n");

## now delete this information from the data frame which should have
## only data now
postDf = postDf[-1,]
head(postDf)

#####################################################################################
## identify the character vs numeric variables and change as necessary
#####################################################################################
## these original variables are ones that aren't numeric
## "Q35","Q41","Q59"
c1 = newPostNames[oldPostNames=="Q35"]
c1
# c1
# [1] "P002"

c2 = newPostNames[oldPostNames=="Q41"]
c2
# c2
# [1] "P005"


c3 =newPostNames[oldPostNames=="Q59"]
c3
# c3
# [1] "P014"


## it is better not to hard code this
postCharVarNames  = c("ResponseId",c1,c2,c3)

postDf[,postCharVarNames]
## these are the variables that are numeric
postNumVarNames = names(postDf)[!(names(postDf) %in% postCharVarNames)]
postNumVarNames

## convert all of the numeric responses from character to numeric
## that's just the way R read them the first time
for(i in postNumVarNames){
	postDf[,i] = as.numeric(postDf[,i])
}

#####################################################################################
## Mostly we are going to have to accept the question identification provided
## by the survey instrument -- here we change a couple of names that make
## sense then save the descriptions with the coded names
#####################################################################################

## the rest of the descriptions need to be parsed so that 
## repetitive information is discarded and we have just the information
## that positively identifies the question and can be used for 
## labels, etc.
postdesc = postdesc[-1]
postdesc = strsplit(postdesc,split="...-",fixed=TRUE)

## from visual inspection, I see that if there are two entries, we can discard
## the first one
postdesc[90]
# postdesc[90]
# $P090
# [1] "Please rate how strongly you agree or disagree with each of the following statements by selecting th"
# [2] "My partner and I felt intimate"                                                                      


for(i in 1:length(postdesc)){
	if(length(postdesc[[i]])==2){
		postdesc[[i]] = postdesc[[i]][2]
	}
}
## look to be sure
postdesc =unlist(postdesc)
postdesc
descPostDf = data.frame(varName=newPostNames,oldName=oldPostNames,varDesc=postdesc)

head(descPostDf)

#####################################################################################
## correct the ones that are scored in reverse
#####################################################################################
## this is the list of variables based on old names that need to be reverse
## this is for the list of variables that are 1-5
## TODO: get the full list of switch variables
## -- run all of the code up to here
## -- run this block of code 
## -- update pastes
## -- copy and paste for the 1-3 variables, or any other groups
## -- 
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

## now switch the values
for(i in revPostNames) {
	postDf[,i] = 6-postDf[,i]
}
## just check to be sure this makes sense
adf[,revPostNames]
# adf[,revPostNames]
#    P052 P064 P070
# 2     5    4    5
# 3     5    1    5
# 4     5    3    3
# 5     5    2    5
# 6     5    1    4
# 7     5    1    5
# 8     2    2    2
# 9     5    3    5
# 10    5    3    5
# 11    1    2    2
# 12    4    4    5
# 13    5    3    4
# 14    3    3    5
# 15    4    1    2
# 16    4    2    5
# 17    4    4    5
# 18    2    2    5
# 19    5    3    5
# 20    3    2    5
# 21    5    2    2
# 22    5    5    5
# 23    5    2    5
# 24    4    2    4
# > cat("Synch1328671829478672000\n");

postDf[,revPostNames]
# postDf[,revPostNames]
#    P052 P064 P070
# 2     1    2    1
# 3     1    5    1
# 4     1    3    3
# 5     1    4    1
# 6     1    5    2
# 7     1    5    1
# 8     4    4    4
# 9     1    3    1
# 10    1    3    1
# 11    5    4    4
# 12    2    2    1
# 13    1    3    2
# 14    3    3    1
# 15    2    5    4
# 16    2    4    1
# 17    2    2    1
# 18    4    4    1
# 19    1    3    1
# 20    3    4    1
# 21    1    4    4
# 22    1    1    1
# 23    1    4    1
# 24    2    4    2
# > cat("Synch1328671840380053000\n");




#########################################################################
## save what we have just done
#########################################################################

save(postDf,descPostDf,postNumVarNames,file="rdata/postData.RData")

#########################################################################
## save a csv file with the mappings of questions
## this is in the dataframe descPostDf
#########################################################################
## comment this out because we are going to edit the file and don't want
## to write over it by accident
#write.csv(descPostDf,file="output/descPostDf.csv",row.names=FALSE)
