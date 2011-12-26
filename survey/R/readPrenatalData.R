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

preDf[1,1:5]
#####################################################################################
## I'm going to use vdesc to get reasonable descriptions later
## it needs to be removed now so the dataframe can be fixed up
#####################################################################################
predesc = unlist(preDf[1,])
## this is the actual 88th question
predesc[88]
# vdesc[88]
#                                                                                                                                                                     Q32_5 
# "Please rate how strongly you agree or disagree with each of the following statements by selecting th...-My partner fears that people will talk too much during my labor" 

## now delete this information from the data frame which should have
## only data now
preDf = preDf[-1,]


#####################################################################################
## identify the character vs numeric variables and change as necessary
#####################################################################################
## these variables are ones that aren't numeric
preCharVarNames  = c("V1","V2","V3","V4","Q2","Q5","Q14")
preDf[,preCharVarNames]
## these are the variables that are numeric
numVarNames = names(preDf)[!(names(preDf) %in% preCharVarNames)]
numVarNames

## convert all of the numeric responses from character to numeric
## that's just the way R read them the first time
for(i in numVarNames){
	preDf[,i] = as.numeric(preDf[,i])
}

#####################################################################################
## Mostly we are going to have to accept the question identification provided
## by the survey instrument -- here we change a couple of names that make
## sense then save the descriptions with the coded names
#####################################################################################
## Note that the first 5 columns of df have sensible names
names(preDf)[1:5] =predesc[1:5]
head(preDf)

## the rest of the descriptions need to be parsed so that 
## repetitive information is discarded and we have just the information
## that positively identifies the question and can be used for 
## labels, etc.
predesc = predesc[-c(1:5)]
vdesc1 = strsplit(predesc,split="...-",fixed=TRUE)

## from visual inspection, I see that if there are two entries, we can discard
## the first one
vdesc1[90]
# vdesc1[90]
# $Q33_2
# [1] "Please rate how strongly you agree or disagree with each of the following statements by selecting th"
# [2] "My partner expects me to use no pain medications during labor"                                       

for(i in 1:length(vdesc1)){
	if(length(vdesc1[[i]])==2){
		vdesc1[[i]] = vdesc1[[i]][2]
	}
}
## look to be sure
vdesc1 =unlist(vdesc1)
vdesc1
descPreDf = data.frame(varName=names(preDf)[-(1:5)],varDesc=vdesc1)
descPreDf

save(preDf,descPreDf,file="rdata/preData.RData")
