###############################################################################
## makeScalesPost.R
##
## make attribute scales
## 
## Author: Haaland
###############################################################################
load("rdata/postData.RData")
# I'm going to save this b/c I'm going to experiment and I want to 
# be able to get this back
# newpostDf = postDf
# postDf = origPostDf
#########################################################################
## make laborLand composite score
#########################################################################
laborLandNames = c("P080","P085","P086","P089","P093",
		"P094","P101","P107","P108","P115","P123","P127","P130",
		"P136")
# should I include (inverse of) P073?
# laborLandNames = c("P071","P072","P073","P079","P080","P085","P086",
#		"P089","P093","P094","P108","P115","P132","P146")
laborLandDf = postDf[,laborLandNames]
laborLandDf_scale = scale(laborLandDf)
describe(laborLandDf_scale)
laborLand = apply(laborLandDf_scale,1,sum)
laborLandDf_scale = data.frame(laborLandDf_scale, laborLand = laborLand)
laborLandDf_scale
llcorrtest = corr.test(laborLandDf_scale)
# correlations, unadjusted p and adjusted p
llcres = data.frame(round(llcorrtest$r[,15],3),
		round(llcorrtest$p[15,],4),
		round(llcorrtest$p[,15],4))
names(llcres) = c("cor.","p-raw","p-adjusted")
llcres
# llcres
#            cor. p-raw p-adjusted
# P080      0.536 9e-04     0.0686
# P085      0.706 0e+00     0.0002
# P086      0.683 0e+00     0.0006
# P089      0.787 0e+00     0.0000
# P093      0.724 0e+00     0.0001
# P094      0.889 0e+00     0.0000
# P101      0.766 0e+00     0.0000
# P107      0.650 0e+00     0.0022
# P108      0.622 1e-04     0.0060
# P115      0.548 7e-04     0.0504
# P123      0.557 5e-04     0.0397
# P127      0.624 1e-04     0.0055
# P130      0.648 0e+00     0.0024
# P136      0.609 1e-04     0.0090
# laborLand 1.000 0e+00     0.0000

postDf$laborLand=laborLand

#########################################################################
## make intuitive movements composite score (theme 1)
#########################################################################
intuitMovNames = c("P087","P100","P106","P112","P121")
intuitMovDf = postDf[,intuitMovNames]
intuitMovDf_scale = scale(intuitMovDf)
describe(intuitMovDf_scale)
intuitMov = apply(intuitMovDf_scale,1,sum)
intuitMovDf_scale = data.frame(intuitMovDf_scale, intuitMov = intuitMov)
intuitMovDf_scale
imcorrtest = corr.test(intuitMovDf_scale)
# correlations, unadjusted p and adjusted p
imcres = data.frame(round(imcorrtest$r[,6],3),
		round(imcorrtest$p[6,],4),
		round(imcorrtest$p[,6],4))
names(imcres) = c("cor.","p-raw","p-adjusted")
imcres
# imcres
#            cor.  p-raw p-adjusted
# P087      0.681 0.0000     0.0001
# P100      0.728 0.0000     0.0000
# P106      0.652 0.0000     0.0003
# P112      0.503 0.0021     0.0229
# P121      0.638 0.0000     0.0004
# intuitMov 1.000 0.0000     0.0000

postDf$intuitMov=intuitMov

#########################################################################
## make physical environment composite score (theme 2)
#########################################################################
physEnvNames = c("P048","P051","P083","P102","P110")
# P053-P056 deleted for quality control
physEnvDf = postDf[,physEnvNames]
physEnvDf_scale = scale(physEnvDf)
describe(physEnvDf_scale)
physEnv = apply(physEnvDf_scale,1,sum)
physEnvDf_scale = data.frame(physEnvDf_scale, physEnv = physEnv)
physEnvDf_scale
pecorrtest = corr.test(physEnvDf_scale)
# correlations, unadjusted p and adjusted p
pecres = data.frame(round(pecorrtest$r[,6],3),
		round(pecorrtest$p[6,],4),
		round(pecorrtest$p[,6],4))
names(pecres) = c("cor.","p-raw","p-adjusted")
pecres
# pecres
#          cor.  p-raw p-adjusted
# P048    0.702 0.0000     0.0000
# P051    0.455 0.0061     0.0608
# P083    0.712 0.0000     0.0000
# P102    0.850 0.0000     0.0000
# P110    0.438 0.0086     0.0771
# physEnv 1.000 0.0000     0.0000

postDf$physEnv=physEnv

#########################################################################
## make emotional environment composite score (theme 3)
#########################################################################
emotEnvNames = c("P063","P076","P092","P095","P097","P098",
		"P109","P113","P114","P138")
emotEnvDf = postDf[,emotEnvNames]
emotEnvDf_scale = scale(emotEnvDf)
describe(emotEnvDf_scale)
emotEnv = apply(emotEnvDf_scale,1,sum)
emotEnvDf_scale = data.frame(emotEnvDf_scale, emotEnv = emotEnv)
emotEnvDf_scale
eecorrtest = corr.test(emotEnvDf_scale)
# correlations, unadjusted p and adjusted p
eecres = data.frame(round(eecorrtest$r[,11],3),
		round(eecorrtest$p[11,],4),
		round(eecorrtest$p[,11],4))
names(eecres) = c("cor.","p-raw","p-adjusted")
eecres
# eecres
#          cor. p-raw p-adjusted
# P063    0.551 6e-04     0.0255
# P076    0.591 2e-04     0.0088
# P092    0.567 4e-04     0.0173
# P095    0.721 0e+00     0.0001
# P097    0.647 0e+00     0.0013
# P098    0.614 1e-04     0.0042
# P109    0.780 0e+00     0.0000
# P113    0.748 0e+00     0.0000
# P114    0.554 6e-04     0.0239
# P138    0.644 0e+00     0.0015
# emotEnv 1.000 0e+00     0.0000

postDf$emotEnv=emotEnv

#########################################################################
## make fluid reality composite score (theme 4)
#########################################################################
fluidRealNames = c("P126","P132","P135","P141","P143","P144","P146")
# P128,P125,P134,P140 removed to form new memory scale
fluidRealDf = postDf[,fluidRealNames]
fluidRealDf_scale = scale(fluidRealDf)
describe(fluidRealDf_scale)
fluidReal = apply(fluidRealDf_scale,1,sum)
fluidRealDf_scale = data.frame(fluidRealDf_scale, fluidReal = fluidReal)
fluidRealDf_scale
frcorrtest = corr.test(fluidRealDf_scale)
# correlations, unadjusted p and adjusted p
frcres = data.frame(round(frcorrtest$r[,8],3),
		round(frcorrtest$p[8,],4),
		round(frcorrtest$p[,8],4))
names(frcres) = c("cor.","p-raw","p-adjusted")
frcres
# frcres
#            cor. p-raw p-adjusted
# P126      0.563 4e-04     0.0082
# P132      0.668 0e+00     0.0003
# P135      0.760 0e+00     0.0000
# P141      0.570 3e-04     0.0073
# P143      0.775 0e+00     0.0000
# P144      0.592 2e-04     0.0041
# P146      0.735 0e+00     0.0000
# fluidReal 1.000 0e+00     0.0000

postDf$fluidReal=fluidReal

#########################################################################
## make intense presence composite score (theme 5)
#########################################################################
intensePresNames = c("P058","P071","P072","P105","P116","P119",
		"P120","P139","P147")
intensePresDf = postDf[,intensePresNames]
intensePresDf_scale = scale(intensePresDf)
describe(intensePresDf_scale)
intensePres = apply(intensePresDf_scale,1,sum)
intensePresDf_scale = data.frame(intensePresDf_scale, intensePres = intensePres)
intensePresDf_scale
ipcorrtest = corr.test(intensePresDf_scale)
# correlations, unadjusted p and adjusted p
ipcres = data.frame(round(ipcorrtest$r[,10],3),
		round(ipcorrtest$p[10,],4),
		round(ipcorrtest$p[,10],4))
names(ipcres) = c("cor.","p-raw","p-adjusted")
ipcres
# ipcres
#              cor.  p-raw p-adjusted
# P058        0.555 0.0005     0.0192
# P071        0.678 0.0000     0.0003
# P072        0.569 0.0004     0.0134
# P105        0.677 0.0000     0.0003
# P116        0.626 0.0001     0.0023
# P119        0.604 0.0001     0.0047
# P120        0.494 0.0026     0.0878
# P139        0.759 0.0000     0.0000
# P147        0.655 0.0000     0.0008
# intensePres 1.000 0.0000     0.0000

postDf$intensePres=intensePres

#########################################################################
## make pain experience composite score (theme 6)
#########################################################################
painExpNames = c("P124","P131","P133","P142","P145",
		"P148","P155","P156")

#painExpNames = c("P124","P131","P133","P142","P145","P148","P155",
#		"P156")
# removed P133 (matches other file)
painExpDf = postDf[,painExpNames]
painExpDf_scale = scale(painExpDf)
describe(painExpDf_scale)
painExp = apply(painExpDf_scale,1,sum)
painExpDf_scale = data.frame(painExpDf_scale, painExp = painExp)
painExpDf_scale
pexcorrtest = corr.test(painExpDf_scale)
# correlations, unadjusted p and adjusted p
pexcres = data.frame(round(pexcorrtest$r[,9],3),
		round(pexcorrtest$p[9,],4),
		round(pexcorrtest$p[,9],4))
names(pexcres) = c("cor.","p-raw","p-adjusted")
pexcres
# pexcres
#          cor.  p-raw p-adjusted
# P124    0.880 0.0000     0.0000
# P131    0.492 0.0027     0.0433
# P133    0.889 0.0000     0.0000
# P142    0.751 0.0000     0.0000
# P145    0.663 0.0000     0.0004
# P148    0.769 0.0000     0.0000
# P155    0.602 0.0001     0.0030
# P156    0.696 0.0000     0.0001
# painExp 1.000 0.0000     0.0000

postDf$painExp=painExp

#########################################################################
## make expectations composite score (theme 7)
#########################################################################
expectationsNames = c("P067","P084","P088")
#expectationsNames = c("P067","P084","P088")
expectationsDf = postDf[,expectationsNames]
expectationsDf_scale = scale(expectationsDf)
describe(expectationsDf_scale)
expectations = apply(expectationsDf_scale,1,sum)
expectationsDf_scale = data.frame(expectationsDf_scale, expectations = expectations)
expectationsDf_scale
excorrtest = corr.test(expectationsDf_scale)
# correlations, unadjusted p and adjusted p
excres = data.frame(round(excorrtest$r[,4],3),
		round(excorrtest$p[4,],4),
		round(excorrtest$p[,4],4))
names(excres) = c("cor.","p-raw","p-adjusted")
excres
# excres
#               cor. p-raw p-adjusted
# P067         0.888     0          0
# P084         0.815     0          0
# P088         0.774     0          0
# expectations 1.000     0          0

postDf$expectations=expectations

#########################################################################
## make memory composite score
#########################################################################
memoryNames = c("P125","P128","P134","P140")
memoryDf = postDf[,memoryNames]
memoryDf_scale = scale(memoryDf)
describe(memoryDf_scale)
memory = apply(memoryDf_scale,1,sum)
memoryDf_scale = data.frame(memoryDf_scale, memory = memory)
memoryDf_scale
mecorrtest = corr.test(memoryDf_scale)
# correlations, unadjusted p and adjusted p
mecres = data.frame(round(mecorrtest$r[,5],3),
		round(mecorrtest$p[5,],4),
		round(mecorrtest$p[,5],4))
names(mecres) = c("cor.","p-raw","p-adjusted")
mecres
# mecres
#         cor. p-raw p-adjusted
# P125   0.617 1e-04      3e-04
# P128   0.803 0e+00      0e+00
# P134   0.863 0e+00      0e+00
# P140   0.853 0e+00      0e+00
# memory 1.000 0e+00      0e+00

postDf$memory=memory

#########################################################################
## make vocals composite score (theme 7)
#########################################################################
vocalsNames = c("P057","P099","P103")
vocalsDf = postDf[,vocalsNames]
vocalsDf_scale = scale(vocalsDf)
describe(vocalsDf_scale)
vocals = apply(vocalsDf_scale,1,sum)
vocalsDf_scale = data.frame(vocalsDf_scale, vocals = vocals)
vocalsDf_scale
vccorrtest = corr.test(vocalsDf_scale)
# correlations, unadjusted p and adjusted p
vccres = data.frame(round(vccorrtest$r[,4],3),
		round(vccorrtest$p[4,],4),
		round(vccorrtest$p[,4],4))
names(vccres) = c("cor.","p-raw","p-adjusted")
vccres
# vccres
#         cor.  p-raw p-adjusted
# P057   0.713 0.0000     0.0000
# P099   0.510 0.0017     0.0052
# P103   0.812 0.0000     0.0000
# vocals 1.000 0.0000     0.0000

postDf$vocals=vocals

#########################################################################
## make outcome measures composite score (theme 8)
#########################################################################
outcomeMeasuresNames = c("P065","P066","P122","P149","P150","P151")
outcomeMeasuresDf = postDf[,outcomeMeasuresNames]
outcomeMeasuresDf_scale = scale(outcomeMeasuresDf)
describe(outcomeMeasuresDf_scale)
outcomeMeasures = apply(outcomeMeasuresDf_scale,1,sum)
outcomeMeasuresDf_scale = data.frame(outcomeMeasuresDf_scale, outcomeMeasures = outcomeMeasures)
outcomeMeasuresDf_scale
omcorrtest = corr.test(outcomeMeasuresDf_scale)
# correlations, unadjusted p and adjusted p
omcres = data.frame(round(omcorrtest$r[,7],3),
		round(omcorrtest$p[7,],4),
		round(omcorrtest$p[,7],4))
names(omcres) = c("cor.","p-raw","p-adjusted")
omcres
# omcres
#                  cor. p-raw p-adjusted
# P065            0.737 0e+00     0.0000
# P066            0.602 1e-04     0.0016
# P122            0.683 0e+00     0.0001
# P149            0.869 0e+00     0.0000
# P150            0.798 0e+00     0.0000
# P151            0.695 0e+00     0.0001
# outcomeMeasures 1.000 0e+00     0.0000

postDf$outcomeMeasures=outcomeMeasures

#########################################################################
## take all 'state' questions and see how well they correlate
#########################################################################
stateQsNames = c("P048","P049","P050","P057","P058","P059","P060",
		"P061","P062","P063","P064","P068","P069","P070","P071",
		"P072","P073","P074","P075","P076","P077","P078","P079",
		"P080","P082","P083","P085","P086","P087","P089","P090",
		"P092","P093","P094","P095","P096","P097","P098","P099",
		"P101","P102","P103","P104","P105","P106","P107","P108",
		"P109","P110","P111","P112","P113","P114","P115","P116",
		"P117","P118","P119","P120","P121","P123","P126","P127",
		"P130","P132","P135","P136","P137","P138","P139","P141",
		"P143","P144","P146","P147")
stateQsDf = postDf[,stateQsNames]
stateQsDf_scale = scale(stateQsDf)
describe(stateQsDf_scale)
stateQs = apply(stateQsDf_scale,1,sum)
stateQsDf_scale = data.frame(stateQsDf_scale, stateQs = stateQs)
stateQsDf_scale
sqcorrtest = corr.test(stateQsDf_scale)
# correlations, unadjusted p and adjusted p
sqcres = data.frame(round(sqcorrtest$r[,76],3),
		round(sqcorrtest$p[76,],4),
		round(sqcorrtest$p[,76],4))
names(sqcres) = c("cor.","p-raw","p-adjusted")
sqcres
# sqcres
#           cor.  p-raw p-adjusted
# P048     0.338 0.0471     1.0000
# P049    -0.075 0.6679     1.0000
# P050     0.324 0.0573     1.0000
# P057     0.054 0.7598     1.0000
# P058     0.311 0.0687     1.0000
# P059    -0.170 0.3291     1.0000
# P060    -0.032 0.8540     1.0000
# P061     0.491 0.0027     1.0000
# P062     0.414 0.0133     1.0000
# P063     0.487 0.0030     1.0000
# P064     0.162 0.3525     1.0000
# P068     0.098 0.5766     1.0000
# P069     0.312 0.0682     1.0000
# P070     0.430 0.0099     1.0000
# P071     0.528 0.0011     1.0000
# P072     0.208 0.2311     1.0000
# P073    -0.049 0.7791     1.0000
# P074     0.263 0.1271     1.0000
# P075    -0.085 0.6283     1.0000
# P076     0.540 0.0008     1.0000
# P077     0.483 0.0033     1.0000
# P078    -0.025 0.8880     1.0000
# P079    -0.104 0.5533     1.0000
# P080     0.265 0.1244     1.0000
# P082     0.394 0.0192     1.0000
# P083     0.521 0.0013     1.0000
# P085     0.580 0.0003     0.7248
# P086     0.450 0.0067     1.0000
# P087     0.638 0.0000     0.1043
# P089     0.612 0.0001     0.2637
# P090     0.238 0.1690     1.0000
# P092     0.569 0.0004     0.9988
# P093     0.643 0.0000     0.0892
# P094     0.703 0.0000     0.0070
# P095     0.671 0.0000     0.0294
# P096     0.197 0.2569     1.0000
# P097     0.486 0.0031     1.0000
# P098     0.506 0.0019     1.0000
# P099     0.128 0.4627     1.0000
# P101     0.708 0.0000     0.0056
# P102     0.547 0.0007     1.0000
# P103     0.294 0.0862     1.0000
# P104    -0.048 0.7853     1.0000
# P105     0.448 0.0070     1.0000
# P106     0.343 0.0438     1.0000
# P107     0.560 0.0005     1.0000
# P108     0.545 0.0007     1.0000
# P109     0.769 0.0000     0.0002
# P110     0.367 0.0302     1.0000
# P111     0.302 0.0782     1.0000
# P112     0.235 0.1745     1.0000
# P113     0.630 0.0001     0.1414
# P114     0.545 0.0007     1.0000
# P115     0.441 0.0081     1.0000
# P116     0.336 0.0488     1.0000
# P117     0.214 0.2161     1.0000
# P118     0.192 0.2697     1.0000
# P119     0.475 0.0040     1.0000
# P120     0.315 0.0656     1.0000
# P121     0.529 0.0011     1.0000
# P123     0.401 0.0171     1.0000
# P126     0.391 0.0203     1.0000
# P127     0.398 0.0178     1.0000
# P130     0.500 0.0022     1.0000
# P132     0.702 0.0000     0.0073
# P135     0.540 0.0008     1.0000
# P136     0.534 0.0010     1.0000
# P137     0.218 0.2080     1.0000
# P138     0.588 0.0002     0.5680
# P139     0.669 0.0000     0.0311
# P141     0.575 0.0003     0.8552
# P143     0.378 0.0253     1.0000
# P144     0.351 0.0387     1.0000
# P146     0.455 0.0061     1.0000
# P147     0.592 0.0002     0.5115
# stateQs  1.000 0.0000     0.0000

postDf$stateQs=stateQs

#########################################################################
## create composite score of PANAS questions
#########################################################################
panasNames = paste("P0",28:47,sep="")
# get rid of these questions b/c there's not enough variation
panasdrop = c("P035","P040","P045")
panasNames = setdiff(panasNames,panasdrop)
panasNames
# panasNames
#  [1] "P028" "P029" "P030" "P031" "P032" "P033" "P034" "P036" "P037" "P038"
# [11] "P039" "P041" "P042" "P043" "P044" "P046" "P047"
panasDf = postDf[,panasNames]
panasDf_scale = scale(panasDf)
describe(panasDf_scale)
panas = apply(panasDf_scale,1,sum)
panasDf_scale = data.frame(panasDf_scale, panas = panas)
panasDf_scale
panascorrtest = corr.test(panasDf_scale)
# correlations, unadjusted p and adjusted p
panascres = data.frame(round(panascorrtest$r[,18],3),
		round(panascorrtest$p[18,],4),
		round(panascorrtest$p[,18],4))
names(panascres) = c("cor.","p-raw","p-adjusted")
panascres
# panascres
#        cor.  p-raw p-adjusted
# P028  0.587 0.0002     0.0274
# P029  0.521 0.0013     0.1570
# P030  0.733 0.0000     0.0001
# P031  0.567 0.0004     0.0486
# P032  0.673 0.0000     0.0014
# P033  0.533 0.0010     0.1218
# P034  0.528 0.0011     0.1354
# P036  0.804 0.0000     0.0000
# P037  0.666 0.0000     0.0018
# P038  0.275 0.1101     1.0000
# P039  0.585 0.0002     0.0289
# P041  0.763 0.0000     0.0000
# P042  0.401 0.0169     1.0000
# P043  0.527 0.0011     0.1372
# P044  0.521 0.0013     0.1588
# P046  0.696 0.0000     0.0005
# P047  0.211 0.2227     1.0000
# panas 1.000 0.0000     0.0000

postDf$panas=panas

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

#########################################################################
## code scales for my 4 factors (other than laborLand)
#########################################################################
M1Names = c("P087","P105","P109",
		"P113","P114","P116","P119","P132","P135","P138","P139",
		"P141","P147")
M1Df = postDf[,M1Names]
M1Df_scale = scale(M1Df)
describe(M1Df_scale)
M1ref = apply(M1Df_scale,1,sum)
M1Df_scale = data.frame(M1Df_scale, M1ref = M1ref)
M1Df_scale
M1corrtest = corr.test(M1Df_scale)
# correlations, unadjusted p and adjusted p
M1cres = data.frame(round(M1corrtest$r[,14],3),
		round(M1corrtest$p[14,],4),
		round(M1corrtest$p[,14],4))
names(M1cres) = c("cor.","p-raw","p-adjusted")
M1cres
# M1cres
#        cor. p-raw p-adjusted
# P087  0.685 0e+00     0.0005
# P105  0.632 0e+00     0.0035
# P109  0.766 0e+00     0.0000
# P113  0.681 0e+00     0.0005
# P114  0.615 1e-04     0.0062
# P116  0.568 4e-04     0.0246
# P119  0.565 4e-04     0.0262
# P132  0.713 0e+00     0.0001
# P135  0.577 3e-04     0.0197
# P138  0.691 0e+00     0.0004
# P139  0.878 0e+00     0.0000
# P141  0.732 0e+00     0.0001
# P147  0.746 0e+00     0.0000
# M1ref 1.000 0e+00     0.0000

# M1cres
#       cor.  p-raw p-adjusted
# P050 0.488 0.0029     0.3437
# P058 0.411 0.0142     1.0000
# P072 0.385 0.0223     1.0000
# P087 0.644 0.0000     0.0042
# P096 0.364 0.0316     1.0000
# P105 0.650 0.0000     0.0034
# P109 0.741 0.0000     0.0001
# P113 0.637 0.0000     0.0053
# P114 0.580 0.0003     0.0342
# P116 0.587 0.0002     0.0282
# P119 0.555 0.0005     0.0694
# P132 0.695 0.0000     0.0005
# P135 0.540 0.0008     0.1010
# P138 0.695 0.0000     0.0005
# P139 0.865 0.0000     0.0000
# P141 0.706 0.0000     0.0003
# P147 0.732 0.0000     0.0001
# M1   1.000 0.0000     0.0000

M2Names = c("P057","P059","P060","P068","P074","P075","P078",
		"P090","P098","P118")
M2Df = postDf[,M2Names]
M2Df$P074 = 6-M2Df$P074
M2Df$P118 = 6-M2Df$P118
M2Df_scale = scale(M2Df)
describe(M2Df_scale)
M2ref = apply(M2Df_scale,1,sum)
M2Df_scale = data.frame(M2Df_scale, M2ref = M2ref)
M2Df_scale
M2corrtest = corr.test(M2Df_scale)
# correlations, unadjusted p and adjusted p
M2cres = data.frame(round(M2corrtest$r[,11],3),
		round(M2corrtest$p[11,],4),
		round(M2corrtest$p[,11],4))
names(M2cres) = c("cor.","p-raw","p-adjusted")
M2cres
# M2cres
#        cor. p-raw p-adjusted
# P057  0.534 1e-03     0.0422
# P059  0.549 6e-04     0.0290
# P060  0.560 5e-04     0.0226
# P068  0.614 1e-04     0.0044
# P074  0.713 0e+00     0.0001
# P075  0.811 0e+00     0.0000
# P078  0.615 1e-04     0.0044
# P090  0.655 0e+00     0.0010
# P098  0.577 3e-04     0.0141
# P118  0.543 8e-04     0.0339
# M2ref 1.000 0e+00     0.0000


# M2cres
#       cor.  p-raw p-adjusted
# P057 0.606 0.0001     0.0086
# P059 0.517 0.0015     0.0982
# P060 0.539 0.0008     0.0582
# P061 0.402 0.0167     0.9001
# P068 0.569 0.0004     0.0260
# P074 0.637 0.0000     0.0029
# P075 0.787 0.0000     0.0000
# P078 0.588 0.0002     0.0146
# P090 0.653 0.0000     0.0017
# P098 0.601 0.0001     0.0099
# P103 0.331 0.0518     1.0000
# P118 0.513 0.0016     0.1058
# M2   1.000 0.0000     0.0000

M3Names = c("P064","P097","P099","P106","P110","P111","P121","P137")
M3Df = postDf[,M3Names]
#M3Df$P071 = 6-M3Df$P071
#M3Df$P104 = 6-M3Df$P104
#M3Df$P120 = 6-M3Df$P120
M3Df$P137 = 6-M3Df$P137
M3Df_scale = scale(M3Df)
describe(M3Df_scale)
M3ref = apply(M3Df_scale,1,sum)
M3Df_scale = data.frame(M3Df_scale, M3ref = M3ref)
M3Df_scale
M3corrtest = corr.test(M3Df_scale)
# correlations, unadjusted p and adjusted p
M3cres = data.frame(round(M3corrtest$r[,9],3),
		round(M3corrtest$p[9,],4),
		round(M3corrtest$p[,9],4))
names(M3cres) = c("cor.","p-raw","p-adjusted")
M3cres
# M3cres
#        cor.  p-raw p-adjusted
# P064  0.547 0.0007     0.0167
# P097  0.570 0.0004     0.0099
# P099  0.696 0.0000     0.0001
# P106  0.733 0.0000     0.0000
# P110  0.775 0.0000     0.0000
# P111  0.733 0.0000     0.0000
# P121  0.691 0.0000     0.0001
# P137  0.512 0.0017     0.0380
# M3ref 1.000 0.0000     0.0000


# M3cres
#       cor.  p-raw p-adjusted
# P064 0.631 0.0000     0.0036
# P071 0.468 0.0045     0.2812
# P097 0.513 0.0016     0.1034
# P099 0.595 0.0002     0.0115
# P104 0.415 0.0131     0.7619
# P106 0.624 0.0001     0.0045
# P110 0.714 0.0000     0.0001
# P111 0.736 0.0000     0.0000
# P117 0.477 0.0037     0.2354
# P120 0.254 0.1415     1.0000
# P121 0.588 0.0002     0.0141
# P137 0.555 0.0005     0.0362
# M3   1.000 0.0000     0.0000

M5Names = c("P048","P062","P063","P070","P076","P077",
		"P083","P092","P095","P102","P126","P143")
M5Df = postDf[,M5Names]
#M5Df$P079 = 6-M5Df$P079
M5Df_scale = scale(M5Df)
describe(M5Df_scale)
M5ref = apply(M5Df_scale,1,sum)
M5Df_scale = data.frame(M5Df_scale, M5ref = M5ref)
M5Df_scale
M5corrtest = corr.test(M5Df_scale)
# correlations, unadjusted p and adjusted p
M5cres = data.frame(round(M5corrtest$r[,13],3),
		round(M5corrtest$p[13,],4),
		round(M5corrtest$p[,13],4))
names(M5cres) = c("cor.","p-raw","p-adjusted")
M5cres
# M5cres
#        cor. p-raw p-adjusted
# P048  0.554 6e-04     0.0330
# P062  0.571 3e-04     0.0211
# P063  0.636 0e+00     0.0028
# P070  0.648 0e+00     0.0018
# P076  0.629 1e-04     0.0035
# P077  0.731 0e+00     0.0000
# P083  0.730 0e+00     0.0000
# P092  0.783 0e+00     0.0000
# P095  0.637 0e+00     0.0028
# P102  0.790 0e+00     0.0000
# P126  0.579 3e-04     0.0169
# P143  0.531 1e-03     0.0606
# M5ref 1.000 0e+00     0.0000


# M5cres
#       cor.  p-raw p-adjusted
# P048 0.542 0.0008     0.1023
# P062 0.550 0.0006     0.0849
# P063 0.608 0.0001     0.0156
# P070 0.623 0.0001     0.0094
# P076 0.644 0.0000     0.0044
# P077 0.707 0.0000     0.0003
# P079 0.438 0.0085     0.9464
# P082 0.465 0.0049     0.5742
# P083 0.705 0.0000     0.0003
# P092 0.779 0.0000     0.0000
# P095 0.616 0.0001     0.0118
# P102 0.787 0.0000     0.0000
# P112 0.383 0.0230     1.0000
# P126 0.526 0.0012     0.1536
# P143 0.559 0.0005     0.0660
# P144 0.488 0.0029     0.3558
# P146 0.511 0.0017     0.2194
# M5   1.000 0.0000     0.0000


postDf$M1=M1
postDf$M2=M2
postDf$M3=M3
postDf$M5=M5
postDf$M1ref=M1ref
postDf$M2ref=M2ref
postDf$M3ref=M3ref
postDf$M5ref=M5ref


save(postDf,newpostDf,origPostDf,descPostDf,postNumVarNames,file="rdata/postData.RData")
