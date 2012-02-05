##############################################################################
## readPrenatalData.R
##
## read the data table (csv) and make a data frame for the prenatal survey
## 
## Author: Haaland
###############################################################################

## read the csv file as exported from the survey software
## prefile is set in the driver.R file
preDf = read.csv(prefile,header=TRUE,as.is=TRUE)
## this shows how many rows (subjects) and columns (questions)
dim(df)
# [1]   5 110

names(preDf)
# names(df)
#   [1] "V1"     "V2"     "V3"     "V4"     "V5"     "Q36"    "Q37"    "Q35"   
#   [9] "Q1"     "Q2"     "Q3"     "Q4"     "Q5"     "Q6"     "Q7"     "Q8"    
#  [17] "Q9"     "Q10"    "Q11"    "Q12"    "Q13"    "Q14"    "Q16_1"  "Q16_2" 
#  [25] "Q16_3"  "Q16_4"  "Q16_5"  "Q16_6"  "Q16_7"  "Q16_8"  "Q16_9"  "Q16_10"
#  [33] "Q16_11" "Q16_12" "Q16_13" "Q16_14" "Q16_15" "Q16_16" "Q16_17" "Q16_18"
#  [41] "Q16_19" "Q16_20" "Q17_1"  "Q17_2"  "Q17_3"  "Q17_4"  "Q17_5"  "Q17_6" 
#  [49] "Q17_7"  "Q17_8"  "Q17_9"  "Q17_10" "Q18_1"  "Q18_2"  "Q18_3"  "Q18_4" 
#  [57] "Q18_5"  "Q18_6"  "Q18_7"  "Q18_8"  "Q18_9"  "Q18_10" "Q19_1"  "Q19_2" 
#  [65] "Q19_3"  "Q19_4"  "Q19_5"  "Q19_6"  "Q19_7"  "Q19_8"  "Q19_9"  "Q20"   
#  [73] "Q21"    "Q22"    "Q23"    "Q25"    "Q26"    "Q27"    "Q28"    "Q24"   
#  [81] "Q29"    "Q30"    "Q31"    "Q32_1"  "Q32_2"  "Q32_3"  "Q32_4"  "Q32_5" 
#  [89] "Q32_6"  "Q32_7"  "Q32_8"  "Q32_9"  "Q32_10" "Q33_1"  "Q33_2"  "Q33_3" 
#  [97] "Q33_4"  "Q33_5"  "Q33_6"  "Q33_7"  "Q33_8"  "Q34_1"  "Q34_2"  "Q34_3" 
# [105] "Q34_4"  "Q34_5"  "Q34_6"  "Q34_7"  "Q34_8"  "Q41"   

#####################################################################################
## a few of these aren't going to be analyzed
#####################################################################################
#V1	V2	V3	V4	V5	Q36	Q37	Q35
#ResponseID	ResponseSet	StartDate	EndDate	Finished	CONSENT FORM FOR ONLINE SURVEYS /  /  TITLE OF RESEARCH: Maternal Perceptions of Natural Chilbirth Exper...	 Do you wish to participate in this study?	Are you 18 or older?
#		R_d0QoYgGDL6DF8BC	Default Response Set	Dec 10, 2011 9:10 PM	Dec 10, 2011 9:18 PM	1			1
#R_5pCLMdvULGaB2bW	Default Response Set	Dec 11, 2011 11:46 AM	Dec 11, 2011 12:00 PM	1	1	1	1
#R_9AXBNcQ1fcpNtru	Default Response Set	Dec 11, 2011 6:42 PM	Dec 11, 2011 7:17 PM	1	1	1	1
#R_1ZzxGWSu5jraeyM	Default Response Set	Dec 21, 2011 6:09 PM	Dec 21, 2011 6:17 PM	1	1	1	1
preDf = preDf[,!(names(preDf) %in% c("V2","V3","V4","V5","Q36","Q37","Q35"))]
names(preDf)
#####################################################################################
## the variables names don't have any particular meaning, so
## I'm just going to set them sequentially
## Note: the first 5 columns are going to get separate names
#####################################################################################
nvars = ncol(preDf)-1
newPreNames = c(paste("Q00",1:9,sep=""),paste("Q0",10:99,sep=""),paste("Q",100:nvars,sep=""))
newPreNames
# newPreNames
#   [1] "Q001" "Q002" "Q003" "Q004" "Q005" "Q006" "Q007" "Q008" "Q009" "Q010"
#  [11] "Q011" "Q012" "Q013" "Q014" "Q015" "Q016" "Q017" "Q018" "Q019" "Q020"
#  [21] "Q021" "Q022" "Q023" "Q024" "Q025" "Q026" "Q027" "Q028" "Q029" "Q030"
#  [31] "Q031" "Q032" "Q033" "Q034" "Q035" "Q036" "Q037" "Q038" "Q039" "Q040"
#  [41] "Q041" "Q042" "Q043" "Q044" "Q045" "Q046" "Q047" "Q048" "Q049" "Q050"
#  [51] "Q051" "Q052" "Q053" "Q054" "Q055" "Q056" "Q057" "Q058" "Q059" "Q060"
#  [61] "Q061" "Q062" "Q063" "Q064" "Q065" "Q066" "Q067" "Q068" "Q069" "Q070"
#  [71] "Q071" "Q072" "Q073" "Q074" "Q075" "Q076" "Q077" "Q078" "Q079" "Q080"
#  [81] "Q081" "Q082" "Q083" "Q084" "Q085" "Q086" "Q087" "Q088" "Q089" "Q090"
#  [91] "Q091" "Q092" "Q093" "Q094" "Q095" "Q096" "Q097" "Q098" "Q099" "Q100"
# [101] "Q101" "Q102"

oldPreNames = names(preDf)[-c(1)]
names(preDf)[-c(1)] = newPreNames

preDf[1,1:10]
#####################################################################################
## I'm going to use vdesc to get reasonable descriptions later
## it needs to be removed now so the dataframe can be fixed up
#####################################################################################
predesc = unlist(preDf[1,])
## this is the actual 87th question
predesc[87]
# vdesc[87]
#                                                                                                                                                                     Q32_5 
# "Please rate how strongly you agree or disagree with each of the following statements by selecting th...-My partner fears that people will talk too much during my labor" 

## now delete this information from the data frame which should have
## only data now
preDf = preDf[-1,]


#####################################################################################
## identify the character vs numeric variables and change as necessary
#####################################################################################
## these variables are ones that aren't numeric
## Q2, Q5, and Q14 are the original names
newPreNames[oldPreNames=="Q2"]
# newPreNames[oldPreNames=="Q2"]
# [1] "Q002"
newPreNames[oldPreNames=="Q5"]
# newPreNames[oldPreNames=="Q5"]
# [1] "Q005"
newPreNames[oldPreNames=="Q14"]
# newPreNames[oldPreNames=="Q14"]
# [1] "Q014"

preCharVarNames  = c("V1","Q002","Q005","Q014")
preDf[,preCharVarNames]
## these are the variables that are numeric
preNumVarNames = names(preDf)[!(names(preDf) %in% preCharVarNames)]
preNumVarNames

## convert all of the numeric responses from character to numeric
## that's just the way R read them the first time
for(i in preNumVarNames){
	preDf[,i] = as.numeric(preDf[,i])
}
#####################################################################################
## Mostly we are going to have to accept the question identification provided
## by the survey instrument -- here we change a couple of names that make
## sense then save the descriptions with the coded names
#####################################################################################
## Note that the first 5 columns of df have sensible names
names(preDf)[1] =as.character(predesc[1])
head(preDf)

## the rest of the descriptions need to be parsed so that 
## repetitive information is discarded and we have just the information
## that positively identifies the question and can be used for 
## labels, etc.
predesc = predesc[-c(1)]
predesc = strsplit(predesc,split="...-",fixed=TRUE)

## from visual inspection, I see that if there are two entries, we can discard
## the first one
predesc[90]
# predesc[90]
# $Q090
# [1] "Please rate how strongly you agree or disagree with each of the following statements by selecting th"
# [2] "My partner feels I have prepared mentally for the childbirth experience"                             

for(i in 1:length(predesc)){
	if(length(predesc[[i]])==2){
		predesc[[i]] = predesc[[i]][2]
	}
}
## look to be sure
predesc =unlist(predesc)
predesc
descPreDf = data.frame(varName=newPreNames,oldName=oldPreNames,varDesc=predesc)
descPreDf

#########################################################################
## delete the last question
#########################################################################
descPreDf=descPreDf[-nrow(descPreDf),]
preDf=preDf[,-ncol(preDf)]

save(preDf,descPreDf,preNumVarNames,file="rdata/preData.RData")

#########################################################################
## save a csv file with the mappings of questions
## this is in the dataframe desPreDf
#########################################################################

#write.csv(descPreDf,file="output/descPreDf.csv",row.names=FALSE)
