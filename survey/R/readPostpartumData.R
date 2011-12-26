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
# [1]   5 166

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

postDf[1,1:5]
#####################################################################################
## I'm going to use vdesc to get reasonable descriptions later
## it needs to be removed now so the dataframe can be fixed up
#####################################################################################
postdesc = unlist(postDf[1,])
## this is the actual 88th question
postdesc[88]
# postdesc[88]
#                                                                                                                                                                          Q48_8 
# "Please rate how strongly you agree or disagree with each of the following statements by selecting th...-I approached my labor through conscious reasoning and/or rationality" 

## now delete this information from the data frame which should have
## only data now
postDf = postDf[-1,]


#####################################################################################
## identify the character vs numeric variables and change as necessary
#####################################################################################
## for reasons I'm unclear of the first name is messed up so fix it
names(postDf)[1]
# [1] "X...V1"
names(postDf)[1] = "V1"
## these variables are ones that aren't numeric
postCharVarNames  = c("V1","V2","V3","V4","Q35","Q41","Q59")
postDf[,postCharVarNames]
## these are the variables that are numeric
numVarNames = names(postDf)[!(names(postDf) %in% postCharVarNames)]
numVarNames

## convert all of the numeric responses from character to numeric
## that's just the way R read them the first time
for(i in numVarNames){
	postDf[,i] = as.numeric(postDf[,i])
}

#####################################################################################
## Mostly we are going to have to accept the question identification provided
## by the survey instrument -- here we change a couple of names that make
## sense then save the descriptions with the coded names
#####################################################################################
## Note that the first 5 columns of df have sensible names
names(postDf)[1:5] =postdesc[1:5]
head(postDf)

## the rest of the descriptions need to be parsed so that 
## repetitive information is discarded and we have just the information
## that positively identifies the question and can be used for 
## labels, etc.
postdesc = postdesc[-c(1:5)]
postdesc = strsplit(postdesc,split="...-",fixed=TRUE)

## from visual inspection, I see that if there are two entries, we can discard
## the first one
postdesc[90]
# vdesc1[90]
# $Q50_5
# [1] "Please rate how strongly you agree or disagree with each of the following statements by selecting th"
# [2] "My behavior was intuitive rather than rational"                                                      


for(i in 1:length(postdesc)){
	if(length(postdesc[[i]])==2){
		postdesc[[i]] = postdesc[[i]][2]
	}
}
## look to be sure
postdesc =unlist(postdesc)
postdesc
descPostDf = data.frame(varName=names(postDf)[-(1:5)],varDesc=postdesc)
descPostDf

save(postDf,descPostDf,file="rdata/postData.RData")
