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
postDf = postDf[,!(names(postDf) %in% c("V2","V5","Q27","Q29","Q31"))]
names(postDf)
				
				
#####################################################################################
## the variables names don't have any particular meaning, so
## I'm just going to set them sequentially
## Note: the first 5 columns are going to get separate names
#####################################################################################
nvars = ncol(postDf)-3
nvars
# nvars
# [1] 158
## we are going to start after the pre variables
nstart = ncol(preDf)-3
nstart
# nstart
# [1] 102
names(preDf)[ncol(preDf)]
# names(preDf)[ncol(preDf)]
# [1] "Q102"

newPostNames = paste("Q",(nstart+1):(nstart+nvars),sep="")
newPostNames
# newPostNames
#   [1] "Q102" "Q103" "Q104" "Q105" "Q106" "Q107" "Q108" "Q109" "Q110" "Q111"
#  [11] "Q112" "Q113" "Q114" "Q115" "Q116" "Q117" "Q118" "Q119" "Q120" "Q121"
#  [21] "Q122" "Q123" "Q124" "Q125" "Q126" "Q127" "Q128" "Q129" "Q130" "Q131"
#  [31] "Q132" "Q133" "Q134" "Q135" "Q136" "Q137" "Q138" "Q139" "Q140" "Q141"
#  [41] "Q142" "Q143" "Q144" "Q145" "Q146" "Q147" "Q148" "Q149" "Q150" "Q151"
#  [51] "Q152" "Q153" "Q154" "Q155" "Q156" "Q157" "Q158" "Q159" "Q160" "Q161"
#  [61] "Q162" "Q163" "Q164" "Q165" "Q166" "Q167" "Q168" "Q169" "Q170" "Q171"
#  [71] "Q172" "Q173" "Q174" "Q175" "Q176" "Q177" "Q178" "Q179" "Q180" "Q181"
#  [81] "Q182" "Q183" "Q184" "Q185" "Q186" "Q187" "Q188" "Q189" "Q190" "Q191"
#  [91] "Q192" "Q193" "Q194" "Q195" "Q196" "Q197" "Q198" "Q199" "Q200" "Q201"
# [101] "Q202" "Q203" "Q204" "Q205" "Q206" "Q207" "Q208" "Q209" "Q210" "Q211"
# [111] "Q212" "Q213" "Q214" "Q215" "Q216" "Q217" "Q218" "Q219" "Q220" "Q221"
# [121] "Q222" "Q223" "Q224" "Q225" "Q226" "Q227" "Q228" "Q229" "Q230" "Q231"
# [131] "Q232" "Q233" "Q234" "Q235" "Q236" "Q237" "Q238" "Q239" "Q240" "Q241"
# [141] "Q242" "Q243" "Q244" "Q245" "Q246" "Q247" "Q248" "Q249" "Q250" "Q251"
# [151] "Q252" "Q253" "Q254" "Q255" "Q256" "Q257" "Q258" "Q259"


oldPostNames = names(postDf)[-c(1:3)]
names(postDf)[-c(1:3)] = newPostNames
postDf[1,1:3]
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


#####################################################################################
## identify the character vs numeric variables and change as necessary
#####################################################################################
## these original variables are ones that aren't numeric
## "Q35","Q41","Q59"
c1 = newPostNames[oldPostNames=="Q35"]
# newPostNames[oldPostNames=="Q35"]
# [1] "Q103"

c2 = newPostNames[oldPostNames=="Q41"]
# newPostNames[oldPostNames=="Q41"]
# [1] "Q106"

c3 =newPostNames[oldPostNames=="Q59"]
# newPostNames[oldPostNames=="Q59"]
# [1] "Q115"

## it is better not to hard code this
#postCharVarNames  = c("V1","V3","V4","Q103","Q106","Q115")
postCharVarNames  = c("V1","V3","V4",c1,c2,c3)

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
## Note that the first 3 columns of df have sensible names
names(postDf)[1:3] =postdesc[1:3]
head(postDf)

## the rest of the descriptions need to be parsed so that 
## repetitive information is discarded and we have just the information
## that positively identifies the question and can be used for 
## labels, etc.
postdesc = postdesc[-c(1:3)]
postdesc = strsplit(postdesc,split="...-",fixed=TRUE)

## from visual inspection, I see that if there are two entries, we can discard
## the first one
postdesc[90]
# postdesc[90]
# $Q192
# [1] "Please rate how strongly you agree or disagree with each of the following statements by selecting th"
# [2] "I relinquished rational control of myself and listened to my body"                                   


for(i in 1:length(postdesc)){
	if(length(postdesc[[i]])==2){
		postdesc[[i]] = postdesc[[i]][2]
	}
}
## look to be sure
postdesc =unlist(postdesc)
postdesc
descPostDf = data.frame(varName=newPostNames,oldName=oldPostNames,varDesc=postdesc)

descPostDf

#########################################################################
## delete the last question
#########################################################################
descPostDf=descPostDf[-nrow(descPostDf),]
postDf=postDf[,-ncol(postDf)]
postNumVarNames = postNumVarNames[-length(postNumVarNames)]

save(postDf,descPostDf,postNumVarNames,file="rdata/postData.RData")

#########################################################################
## save a csv file with the mappings of questions
## this is in the dataframe descPostDf
#########################################################################

#write.csv(descPostDf,file="output/descPostDf.csv",row.names=FALSE)
