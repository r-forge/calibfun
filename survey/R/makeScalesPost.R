###############################################################################
## makeScalesPost.R
##
## make attribute scales
## 
## Author: Haaland
###############################################################################
load("rdata/postData.RData")

#########################################################################
## make laborLand score
#########################################################################
laborLandNames = c("P071","P072","P073","P079","P080","P085","P086",
		"P089","P093","P094","P108","P115","P132","P146")
# 11 of these 14 overlap with other themes
laborLandDf = postDf[,laborLandNames]
laborLandDf_scale = scale(laborLandDf)
describe(laborLandDf_scale)
laborLand = apply(laborLandDf_scale,1,sum)
laborLandDf_scale = data.frame(laborLandDf_scale, laborLand = laborLand)
laborLandDf_scale
cor(laborLandDf_scale)
postDf$laborLand=laborLand
#########################################################################
## make intuitive movements score (theme 1)
#########################################################################
intuitMovNames = c("P057","P078","P086","P087","P089","P093","P094"
				,"P099","P100","P101","P103","P106","P107","P108",
				"P112","P115","P121","P127","P130","P136")
# P086,P089,P093,P094,P108, and P115 overlap with laborLand score
intuitMovDf = postDf[,intuitMovNames]
intuitMovDf_scale = scale(intuitMovDf)
describe(intuitMovDf_scale)
intuitMov = apply(intuitMovDf_scale,1,sum)
intuitMovDf_scale = data.frame(intuitMovDf_scale, intuitMov = intuitMov)
intuitMovDf_scale
cor(intuitMovDf_scale)
postDf$intuitMov=intuitMov
#########################################################################
## make physical environment score (theme 2)
#########################################################################
physEnvNames = c("P048","P049","P051","P053","P054","P055","P056",
				"P083","P102","P110")
physEnvDf = postDf[,physEnvNames]
physEnvDf_scale = scale(physEnvDf)
describe(physEnvDf_scale)
physEnv = apply(physEnvDf_scale,1,sum)
physEnvDf_scale = data.frame(physEnvDf_scale, physEnv = physEnv)
physEnvDf_scale
cor(physEnvDf_scale)
postDf$physEnv=physEnv
#########################################################################
## make emotional environment score (theme 3)
#########################################################################
emotEnvNames = c("P052","P062","P063","P064","P070","P074",
				"P075","P076","P090","P092","P095","P097","P098",
				"P109","P111","P113","P114","P117","P118","P123",
				"P138")
emotEnvDf = postDf[,emotEnvNames]
emotEnvDf_scale = scale(emotEnvDf)
describe(emotEnvDf_scale)
emotEnv = apply(emotEnvDf_scale,1,sum)
emotEnvDf_scale = data.frame(emotEnvDf_scale, emotEnv = emotEnv)
emotEnvDf_scale
cor(emotEnvDf_scale)
postDf$emotEnv=emotEnv
#########################################################################
## make fluid reality score (theme 4)
#########################################################################
fluidRealNames = c("P077","P104","P125","P126","P128","P132","P134",
					"P135","P137","P140","P141","P143","P144","P146")
# P132 and P146 overlap with laborLand score
fluidRealDf = postDf[,fluidRealNames]
fluidRealDf_scale = scale(fluidRealDf)
describe(fluidRealDf_scale)
fluidReal = apply(fluidRealDf_scale,1,sum)
fluidRealDf_scale = data.frame(fluidRealDf_scale, fluidReal = fluidReal)
fluidRealDf_scale
cor(fluidRealDf_scale)
postDf$fluidReal=fluidReal
#########################################################################
## make intense presence score (theme 5)
#########################################################################
intensePresNames = c("P058","P059","P060","P061","P068","P069","P071",
					"P072","P073","P082","P096","P105","P116","P119",
					"P120","P139","P147")
# P071,P072,and P073 overlap with laborLand score
intensePresDf = postDf[,intensePresNames]
intensePresDf_scale = scale(intensePresDf)
describe(intensePresDf_scale)
intensePres = apply(intensePresDf_scale,1,sum)
intensePresDf_scale = data.frame(intensePresDf_scale, intensePres = intensePres)
intensePresDf_scale
cor(intensePresDf_scale)
postDf$intensePres=intensePres
#########################################################################
## make pain experience score (theme 6)
#########################################################################
painExpNames = c("P050","P124","P129","P131","P133","P142","P145",
				"P148","P154","P155","P156")
painExpDf = postDf[,painExpNames]
painExpDf_scale = scale(painExpDf)
describe(painExpDf_scale)
painExp = apply(painExpDf_scale,1,sum)
painExpDf_scale = data.frame(painExpDf_scale, painExp = painExp)
painExpDf_scale
cor(painExpDf_scale)
postDf$painExp=painExp
#########################################################################
## make expectations score (theme 7)
#########################################################################
expectationsNames = c("P067","P081","P084","P088","P091")
expectationsDf = postDf[,expectationsNames]
expectationsDf_scale = scale(expectationsDf)
describe(expectationsDf_scale)
expectations = apply(expectationsDf_scale,1,sum)
expectationsDf_scale = data.frame(expectationsDf_scale, expectations = expectations)
expectationsDf_scale
cor(expectationsDf_scale)
postDf$expectations=expectations
#########################################################################
## make outcome measures score (theme 8)
#########################################################################
outcomeMeasuresNames = c("P065","P066","P122","P149","P150","P151")
outcomeMeasuresDf = postDf[,outcomeMeasuresNames]
outcomeMeasuresDf_scale = scale(outcomeMeasuresDf)
describe(outcomeMeasuresDf_scale)
outcomeMeasures = apply(outcomeMeasuresDf_scale,1,sum)
outcomeMeasuresDf_scale = data.frame(outcomeMeasuresDf_scale, outcomeMeasures = outcomeMeasures)
outcomeMeasuresDf_scale
cor(outcomeMeasuresDf_scale)
postDf$outcomeMeasures=outcomeMeasures


#########################################################################
## plot correlations (move to individualAnalyses?)
#########################################################################
groupsDf = data.frame(physEnv = physEnv, laborLand = laborLand)
cor(groupsDf)
library(ggplot2)
ggplot(groupsDf,aes(x=physEnv, y=laborLand))+geom_point()

#########################################################################
## creating difference scores for actual vs. perceived time in active
## labor and pushing
#########################################################################

## 0-2 hours | 2-4 hours | 4-6 hours | 6-8 hours | 8-10 hours | 
## 10-12 hours | More than 12 hours
tValsAct = c(1,3,5,7,9,11,13)
## values for active labor
tActualAct = tValsAct[postDf[,c("P022")]]
tPerceivedAct = tValsAct[postDf[,c("P152")]]
tDiffAct = tActualAct - tPerceivedAct
## add these variables to the dataframe
postDf$tActualAct=tActualAct
postDf$tPerceivedAct=tPerceivedAct
postDf$tDiffAct=tDiffAct

## 0-19 minutes | 20-39 minutes | 40-59 minutes | 60-79 minutes | 
## 80-99 minutes | 100-120 minutes | more than 2 hours
tValsPush = c(10,30,50,70,90,110,130)
##values for pushing
tActualPush = tValsPush[postDf[,c("P023")]]
tPerceivedPush = tValsPush[postDf[,c("P153")]]
tDiffPush = tActualPush - tPerceivedPush
## add these variables to the dataframe
postDf$tActualPush=tActualPush
postDf$tPerceivedPush=tPerceivedPush
postDf$tDiffPush=tDiffPush

save(postDf,descPostDf,postNumVarNames,file="rdata/postData.RData")