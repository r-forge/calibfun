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
dim(preDf)
# dim(preDf)
# [1]  13 110


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
preDf = preDf[,!(names(preDf) %in% c("V2","V3","V4","V5","Q36","Q37","Q35","Q41"))]
names(preDf)
#####################################################################################
## the variables names don't have any particular meaning, so
## I'm just going to set them sequentially
## Note: the first 5 columns are going to get separate names
#####################################################################################
nvars = ncol(preDf)-1
newPreNames = c(paste("N00",1:9,sep=""),paste("N0",10:99,sep=""),paste("N",100:nvars,sep=""))
newPreNames
# newPreNames
#   [1] "N001" "N002" "N003" "N004" "N005" "N006" "N007" "N008" "N009" "N010"
#  [11] "N011" "N012" "N013" "N014" "N015" "N016" "N017" "N018" "N019" "N020"
#  [21] "N021" "N022" "N023" "N024" "N025" "N026" "N027" "N028" "N029" "N030"
#  [31] "N031" "N032" "N033" "N034" "N035" "N036" "N037" "N038" "N039" "N040"
#  [41] "N041" "N042" "N043" "N044" "N045" "N046" "N047" "N048" "N049" "N050"
#  [51] "N051" "N052" "N053" "N054" "N055" "N056" "N057" "N058" "N059" "N060"
#  [61] "N061" "N062" "N063" "N064" "N065" "N066" "N067" "N068" "N069" "N070"
#  [71] "N071" "N072" "N073" "N074" "N075" "N076" "N077" "N078" "N079" "N080"
#  [81] "N081" "N082" "N083" "N084" "N085" "N086" "N087" "N088" "N089" "N090"
#  [91] "N091" "N092" "N093" "N094" "N095" "N096" "N097" "N098" "N099" "N100"
# [101] "N101"

oldPreNames = names(preDf)[-c(1)]
names(preDf)[-c(1)] = newPreNames
names(preDf)[1] = "ResponseId"

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
c1 = newPreNames[oldPreNames=="Q2"]
# newPreNames[oldPreNames=="Q2"]
# [1] "N002"

c2 = newPreNames[oldPreNames=="Q5"]
# newPreNames[oldPreNames=="Q5"]
# [1] "N005"

c3 =newPreNames[oldPreNames=="Q14"]
# newPreNames[oldPreNames=="Q14"]
# [1] "N014"

preCharVarNames  = c("ResponseId",c1,c2,c3)
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
# $N090
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
## save the results
#########################################################################

save(preDf,descPreDf,preNumVarNames,file="rdata/preData.RData")

#########################################################################
## save a csv file with the mappings of questions
## this is in the dataframe desPreDf
#########################################################################
## comment this out so that you don't over write it by mistake
#write.csv(descPreDf,file="output/descPreDf.csv",row.names=FALSE)


