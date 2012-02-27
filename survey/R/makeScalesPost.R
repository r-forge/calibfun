###############################################################################
## makeScalesPost.R
##
## make attribute scales
## 
## Author: Haaland
###############################################################################
load("rdata/postData.RData")

#########################################################################
## make laborLand composite score
#########################################################################
laborLandNames = c("P071","P080","P085","P086","P089","P093","P094",
		"P108","P115","P132","P146")
laborLandDf = postDf[,laborLandNames]
laborLandDf_scale = scale(laborLandDf)
describe(laborLandDf_scale)
laborLand = apply(laborLandDf_scale,1,sum)
laborLandDf_scale = data.frame(laborLandDf_scale, laborLand = laborLand)
laborLandDf_scale
llcorrtest = corr.test(laborLandDf_scale)
# correlations, unadjusted p and adjusted p
llcres = data.frame(round(llcorrtest$r[,12],3),
		round(llcorrtest$p[12,],4),
		round(llcorrtest$p[,12],4))
names(llcres) = c("cor.","p-raw","p-adjusted")
llcres
# llcres
#            cor. p-raw p-adjusted
# P071      0.694 0e+00     0.0002
# P080      0.561 5e-04     0.0210
# P085      0.764 0e+00     0.0000
# P086      0.697 0e+00     0.0002
# P089      0.792 0e+00     0.0000
# P093      0.681 0e+00     0.0004
# P094      0.846 0e+00     0.0000
# P108      0.662 0e+00     0.0008
# P115      0.564 4e-04     0.0198
# P132      0.636 0e+00     0.0022
# P146      0.537 9e-04     0.0366
# laborLand 1.000 0e+00     0.0000


postDf$laborLand=laborLand

#########################################################################
## make intuitive movements composite score (theme 1)
#########################################################################
intuitMovNames = c("P087","P100","P101","P107","P121","P127","P130",
		"P136")
intuitMovDf = postDf[,intuitMovNames]
intuitMovDf_scale = scale(intuitMovDf)
describe(intuitMovDf_scale)
intuitMov = apply(intuitMovDf_scale,1,sum)
intuitMovDf_scale = data.frame(intuitMovDf_scale, intuitMov = intuitMov)
intuitMovDf_scale
imcorrtest = corr.test(intuitMovDf_scale)
# correlations, unadjusted p and adjusted p
imcres = data.frame(round(imcorrtest$r[,9],3),
		round(imcorrtest$p[9,],4),
		round(imcorrtest$p[,9],4))
names(imcres) = c("cor.","p-raw","p-adjusted")
imcres
# imcres
#            cor. p-raw p-adjusted
# P087      0.715 0e+00     0.0000
# P100      0.661 0e+00     0.0005
# P101      0.799 0e+00     0.0000
# P107      0.621 1e-04     0.0021
# P121      0.536 9e-04     0.0228
# P127      0.619 1e-04     0.0022
# P130      0.712 0e+00     0.0001
# P136      0.773 0e+00     0.0000
# intuitMov 1.000 0e+00     0.0000


postDf$intuitMov=intuitMov
#########################################################################
## make physical environment composite score (theme 2)
#########################################################################
physEnvNames = c("P048","P083","P102")
physEnvDf = postDf[,physEnvNames]
physEnvDf_scale = scale(physEnvDf)
describe(physEnvDf_scale)
physEnv = apply(physEnvDf_scale,1,sum)
physEnvDf_scale = data.frame(physEnvDf_scale, physEnv = physEnv)
physEnvDf_scale
pecorrtest = corr.test(physEnvDf_scale)
# correlations, unadjusted p and adjusted p
pecres = data.frame(round(pecorrtest$r[,4],3),
		round(pecorrtest$p[4,],4),
		round(pecorrtest$p[,4],4))
names(pecres) = c("cor.","p-raw","p-adjusted")
pecres
# pecres
#          cor. p-raw p-adjusted
# P048    0.751     0          0
# P083    0.806     0          0
# P102    0.860     0          0
# physEnv 1.000     0          0

postDf$physEnv=physEnv
#########################################################################
## make emotional environment composite score (theme 3)
#########################################################################
emotEnvNames = c("P063","P076","P092","P095","P097","P098","P109",
		"P113","P114","P123","P138")
emotEnvDf = postDf[,emotEnvNames]
emotEnvDf_scale = scale(emotEnvDf)
describe(emotEnvDf_scale)
emotEnv = apply(emotEnvDf_scale,1,sum)
emotEnvDf_scale = data.frame(emotEnvDf_scale, emotEnv = emotEnv)
emotEnvDf_scale
eecorrtest = corr.test(emotEnvDf_scale)
# correlations, unadjusted p and adjusted p
eecres = data.frame(round(eecorrtest$r[,12],3),
		round(eecorrtest$p[12,],4),
		round(eecorrtest$p[,12],4))
names(eecres) = c("cor.","p-raw","p-adjusted")
eecres
# eecres
#          cor. p-raw p-adjusted
# P063    0.569 4e-04     0.0207
# P076    0.560 5e-04     0.0250
# P092    0.549 6e-04     0.0341
# P095    0.717 0e+00     0.0001
# P097    0.654 0e+00     0.0013
# P098    0.626 1e-04     0.0034
# P109    0.755 0e+00     0.0000
# P113    0.726 0e+00     0.0001
# P114    0.564 4e-04     0.0232
# P123    0.538 9e-04     0.0443
# P138    0.641 0e+00     0.0020
# emotEnv 1.000 0e+00     0.0000


postDf$emotEnv=emotEnv
#########################################################################
## make fluid reality composite score (theme 4)
#########################################################################
fluidRealNames = c("P125","P126","P135","P141","P143","P144")
# P132 and P146 overlap with laborLand score
fluidRealDf = postDf[,fluidRealNames]
fluidRealDf_scale = scale(fluidRealDf)
describe(fluidRealDf_scale)
fluidReal = apply(fluidRealDf_scale,1,sum)
fluidRealDf_scale = data.frame(fluidRealDf_scale, fluidReal = fluidReal)
fluidRealDf_scale
frcorrtest = corr.test(fluidRealDf_scale)
# correlations, unadjusted p and adjusted p
frcres = data.frame(round(frcorrtest$r[,7],3),
		round(frcorrtest$p[7,],4),
		round(frcorrtest$p[,7],4))
names(frcres) = c("cor.","p-raw","p-adjusted")
frcres
# frcres
#            cor. p-raw p-adjusted
# P125      0.650 0e+00     0.0004
# P126      0.680 0e+00     0.0001
# P135      0.757 0e+00     0.0000
# P141      0.620 1e-04     0.0012
# P143      0.750 0e+00     0.0000
# P144      0.565 4e-04     0.0060
# fluidReal 1.000 0e+00     0.0000


postDf$fluidReal=fluidReal
#########################################################################
## make intense presence composite score (theme 5)
#########################################################################
intensePresNames = c("P058",
		"P072","P105","P116","P119",
		"P139","P147")
intensePresDf = postDf[,intensePresNames]
intensePresDf_scale = scale(intensePresDf)
describe(intensePresDf_scale)
intensePres = apply(intensePresDf_scale,1,sum)
intensePresDf_scale = data.frame(intensePresDf_scale, intensePres = intensePres)
intensePresDf_scale
ipcorrtest = corr.test(intensePresDf_scale)
# correlations, unadjusted p and adjusted p
ipcres = data.frame(round(ipcorrtest$r[,8],3),
		round(ipcorrtest$p[8,],4),
		round(ipcorrtest$p[,8],4))
names(ipcres) = c("cor.","p-raw","p-adjusted")
ipcres
# ipcres
#              cor.  p-raw p-adjusted
# P058        0.497 0.0024     0.0455
# P072        0.561 0.0005     0.0097
# P105        0.725 0.0000     0.0000
# P116        0.686 0.0000     0.0001
# P119        0.614 0.0001     0.0020
# P139        0.771 0.0000     0.0000
# P147        0.749 0.0000     0.0000
# intensePres 1.000 0.0000     0.0000


postDf$intensePres=intensePres
#########################################################################
## make pain experience composite score (theme 6)
#########################################################################
painExpNames = c("P124","P131","P133","P142","P145","P148","P155",
		"P156")
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


#########################################################################
## create composite score of PANAS questions
#########################################################################
panasNames = paste("P0",28:47,sep="")
# get rid of these questions b/c there's not enough variation
panasdrop = c("P035","P040","P045","P047")
panasNames = setdiff(panasNames,panasdrop)
panasNames
# panasNames
#  [1] "P028" "P029" "P030" "P031" "P032" "P033" "P034" "P036" "P037" "P038"
# [11] "P039" "P041" "P042" "P043" "P044" "P046"
panasDf = postDf[,panasNames]
panasDf_scale = scale(panasDf)
describe(panasDf_scale)
panas = apply(panasDf_scale,1,sum)
panasDf_scale = data.frame(panasDf_scale, panas = panas)
panasDf_scale
panascorrtest = corr.test(panasDf_scale)
# correlations, unadjusted p and adjusted p
panascres = data.frame(round(panascorrtest$r[,17],3),
		round(panascorrtest$p[17,],4),
		round(panascorrtest$p[,17],4))
names(panascres) = c("cor.","p-raw","p-adjusted")
panascres
# panascres
#        cor.  p-raw p-adjusted
# P028  0.619 0.0001     0.0088
# P029  0.530 0.0011     0.1112
# P030  0.754 0.0000     0.0000
# P031  0.569 0.0004     0.0404
# P032  0.685 0.0000     0.0007
# P033  0.536 0.0009     0.0952
# P034  0.474 0.0040     0.3830
# P036  0.817 0.0000     0.0000
# P037  0.678 0.0000     0.0010
# P038  0.263 0.1276     1.0000
# P039  0.586 0.0002     0.0248
# P041  0.775 0.0000     0.0000
# P042  0.354 0.0372     1.0000
# P043  0.553 0.0006     0.0610
# P044  0.521 0.0013     0.1376
# P046  0.717 0.0000     0.0002
# panas 1.000 0.0000     0.0000

## old panas correlations (all questions)
# panascres
#        cor.  p-raw p-adjusted
# P028  0.530 0.0011     0.1838
# P029  0.577 0.0003     0.0506
# P030  0.693 0.0000     0.0008
# P031  0.614 0.0001     0.0167
# P032  0.653 0.0000     0.0042
# P033  0.559 0.0005     0.0863
# P034  0.578 0.0003     0.0494
# P035  0.617 0.0001     0.0153
# P036  0.764 0.0000     0.0000
# P037  0.637 0.0000     0.0075
# P038  0.298 0.0817     1.0000
# P039  0.522 0.0013     0.2233
# P040  0.603 0.0001     0.0236
# P041  0.723 0.0000     0.0002
# P042  0.442 0.0078     1.0000
# P043  0.471 0.0043     0.6843
# P044  0.510 0.0017     0.2914
# P045  0.368 0.0294     1.0000
# P046  0.655 0.0000     0.0039
# P047  0.238 0.1682     1.0000
# panas 1.000 0.0000     0.0000









#########################################################################
## plot correlations (move to individualAnalyses?)
#########################################################################
groupsDf = data.frame(intuitMov = intuitMov, physEnv = physEnv,
		emotEnv = emotEnv, fluidReal = fluidReal, intensePres = intensePres,
		painExp = painExp, expectations = expectations, outcomeMeasures = outcomeMeasures,
		laborLand = laborLand)
grpcorrtest = corr.test(groupsDf)
# correlations, unadjusted p and adjusted p
grpcres = data.frame(round(grpcorrtest$r[,9],3),
		round(grpcorrtest$p[9,],4),
		round(grpcorrtest$p[,9],4))
names(grpcres) = c("cor.","p-raw","p-adjusted")
grpcres
# grpcres
#                  cor.  p-raw p-adjusted
# intuitMov       0.736 0.0000     0.0000
# physEnv         0.177 0.3102     1.0000
# emotEnv         0.582 0.0002     0.0062
# fluidReal       0.421 0.0118     0.1414
# intensePres     0.580 0.0003     0.0063
# painExp         0.536 0.0009     0.0183
# expectations    0.382 0.0237     0.2373
# outcomeMeasures 0.607 0.0001     0.0031
# laborLand       1.000 0.0000     0.0000

# to get the whole correlation matrix
grpcorrtest$r
# grpcorrtest$r
#                 intuitMov   physEnv   emotEnv fluidReal intensePres   painExp
# intuitMov       1.0000000 0.2048264 0.6824775 0.3729995   0.4460095 0.5881909
# physEnv         0.2048264 1.0000000 0.2364817 0.2884915   0.0658481 0.2027949
# emotEnv         0.6824775 0.2364817 1.0000000 0.6689764   0.4582295 0.6554013
# fluidReal       0.3729995 0.2884915 0.6689764 1.0000000   0.3609485 0.5124390
# intensePres     0.4460095 0.0658481 0.4582295 0.3609485   1.0000000 0.5556691
# painExp         0.5881909 0.2027949 0.6554013 0.5124390   0.5556691 1.0000000
# expectations    0.5265144 0.4683503 0.6313744 0.3948105   0.4832478 0.5627930
# outcomeMeasures 0.7200597 0.0545819 0.7534936 0.5057130   0.5573094 0.7602627
# laborLand       0.7358440 0.1765905 0.5816486 0.4210276   0.5796943 0.5356285
#                 expectations outcomeMeasures laborLand
# intuitMov          0.5265144       0.7200597 0.7358440
# physEnv            0.4683503       0.0545819 0.1765905
# emotEnv            0.6313744       0.7534936 0.5816486
# fluidReal          0.3948105       0.5057130 0.4210276
# intensePres        0.4832478       0.5573094 0.5796943
# painExp            0.5627930       0.7602627 0.5356285
# expectations       1.0000000       0.5880393 0.3815260
# outcomeMeasures    0.5880393       1.0000000 0.6069820
# laborLand          0.3815260       0.6069820 1.0000000


library(ggplot2)
pdf("plots/scaleCorrelations.pdf")
#########################################################################
## First, compare each theme to laborLand
#########################################################################
## intuitMov
adf = grpcres[rownames(grpcres)=="intuitMov",]
adf$x = -15
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=intuitMov, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## physEnv
adf = grpcres[rownames(grpcres)=="physEnv",]
adf$x = -6
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=physEnv, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## emotEnv
adf = grpcres[rownames(grpcres)=="emotEnv",]
adf$x = -20
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=emotEnv, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## fluidReal
adf = grpcres[rownames(grpcres)=="fluidReal",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=fluidReal, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## intensePres
adf = grpcres[rownames(grpcres)=="intensePres",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=intensePres, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## painExp
adf = grpcres[rownames(grpcres)=="painExp",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=painExp, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## expectations
adf = grpcres[rownames(grpcres)=="expectations",]
adf$x = -3
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=expectations, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## outcomeMeasures
adf = grpcres[rownames(grpcres)=="outcomeMeasures",]
adf$x = -10
adf$y = 10
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=outcomeMeasures, y=laborLand))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)

#########################################################################
## Now compare each theme to outcome measures
#########################################################################
## intuitMov
adf = grpcres[rownames(grpcres)=="intuitMov",]
adf$x = -10
adf$y = 6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=intuitMov, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## physEnv
adf = grpcres[rownames(grpcres)=="physEnv",]
adf$x = -6
adf$y = 6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=physEnv, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## emotEnv
adf = grpcres[rownames(grpcres)=="emotEnv",]
adf$x = -20
adf$y = 6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=emotEnv, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## fluidReal
adf = grpcres[rownames(grpcres)=="fluidReal",]
adf$x = -10
adf$y = 6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=fluidReal, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## intensePres
adf = grpcres[rownames(grpcres)=="intensePres",]
adf$x = -10
adf$y = 6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=intensePres, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## painExp
adf = grpcres[rownames(grpcres)=="painExp",]
adf$x = -10
adf$y = 6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=painExp, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)
## expectations
adf = grpcres[rownames(grpcres)=="expectations",]
adf$x = -3
adf$y = 6
adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		"\np-adjusted=",adf$'p-adjusted')
ggplot(groupsDf,aes(x=expectations, y=outcomeMeasures))+geom_point()+
		geom_smooth(method=lm)+
		geom_text(aes(x=x, y=y, label=label),data=adf)

dev.off()
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