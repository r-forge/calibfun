###############################################################################
## altThemeMakeScalesPost.R
##
## scales for my alternate themes - data is not up to date with full
## data set (was last run for dropped data set)
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
laborLandNames = c("P049","P069","P080","P085","P086","P089","P093",
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
llcres = data.frame(round(llcorrtest$r[,17],3),
		round(llcorrtest$p[17,],4),
		round(llcorrtest$p[,17],4))
names(llcres) = c("cor.","p-raw","p-adjusted")
llcres
# llcres
#            cor.  p-raw p-adjusted
# P049      0.252 0.1443     1.0000
# P069      0.448 0.0070     0.6165
# P080      0.561 0.0005     0.0501
# P085      0.697 0.0000     0.0004
# P086      0.691 0.0000     0.0006
# P089      0.767 0.0000     0.0000
# P093      0.709 0.0000     0.0002
# P094      0.867 0.0000     0.0000
# P101      0.768 0.0000     0.0000
# P107      0.636 0.0000     0.0049
# P108      0.641 0.0000     0.0041
# P115      0.520 0.0014     0.1449
# P123      0.568 0.0004     0.0414
# P127      0.620 0.0001     0.0086
# P130      0.640 0.0000     0.0044
# P136      0.569 0.0004     0.0411
# laborLand 1.000 0.0000     0.0000

postDf$laborLand=laborLand

#########################################################################
## make Intimacy-Trust-Security (intimateSec) composite score
#########################################################################
intimateSecNames = c("P048","P063","P076",
		"P090","P092","P095","P098","P102")
intimateSecDf = postDf[,intimateSecNames]
intimateSecDf_scale = scale(intimateSecDf)
describe(intimateSecDf_scale)
intimateSec = apply(intimateSecDf_scale,1,sum)
intimateSecDf_scale = data.frame(intimateSecDf_scale, intimateSec = intimateSec)
intimateSecDf_scale
iscorrtest = corr.test(intimateSecDf_scale)
# correlations, unadjusted p and adjusted p
iscres = data.frame(round(iscorrtest$r[,9],3),
		round(iscorrtest$p[9,],4),
		round(iscorrtest$p[,9],4))
names(iscres) = c("cor.","p-raw","p-adjusted")
iscres
# iscres
#              cor. p-raw p-adjusted
# P048        0.705 1e-04     0.0018
# P063        0.651 3e-04     0.0089
# P076        0.629 6e-04     0.0151
# P090        0.652 3e-04     0.0089
# P092        0.733 0e+00     0.0007
# P095        0.737 0e+00     0.0006
# P098        0.710 0e+00     0.0015
# P102        0.753 0e+00     0.0003
# intimateSec 1.000 0e+00     0.0000

postDf$intimateSec=intimateSec

#########################################################################
## make rhythm-relaxation-ritual (RRR) composite score (theme 1)
#########################################################################
RRRNames = c("P050","P071","P072","P096","P105","P116","P119",
		"P139","P147")
RRRDf = postDf[,RRRNames]
RRRDf_scale = scale(RRRDf)
describe(RRRDf_scale)
RRR = apply(RRRDf_scale,1,sum)
RRRDf_scale = data.frame(RRRDf_scale, RRR = RRR)
RRRDf_scale
rrrcorrtest = corr.test(RRRDf_scale)
# correlations, unadjusted p and adjusted p
rrrcres = data.frame(round(rrrcorrtest$r[,10],3),
		round(rrrcorrtest$p[10,],4),
		round(rrrcorrtest$p[,10],4))
names(rrrcres) = c("cor.","p-raw","p-adjusted")
rrrcres
# rrrcres
#       cor. p-raw p-adjusted
# P050 0.696 1e-04     0.0033
# P071 0.610 9e-04     0.0347
# P072 0.661 2e-04     0.0094
# P096 0.606 1e-03     0.0376
# P105 0.712 0e+00     0.0019
# P116 0.733 0e+00     0.0009
# P119 0.629 6e-04     0.0228
# P139 0.769 0e+00     0.0002
# P147 0.685 1e-04     0.0046
# RRR  1.000 0e+00     0.0000

postDf$RRR=RRR

#########################################################################
## make Space and Time (spaceTime) composite score (theme 4)
#########################################################################
spaceTimeNames = c("P070","P077",
		"P126","P132","P135","P143","P144","P146")
spaceTimeDf = postDf[,spaceTimeNames]
spaceTimeDf_scale = scale(spaceTimeDf)
describe(spaceTimeDf_scale)
spaceTime = apply(spaceTimeDf_scale,1,sum)
spaceTimeDf_scale = data.frame(spaceTimeDf_scale, spaceTime = spaceTime)
spaceTimeDf_scale
stcorrtest = corr.test(spaceTimeDf_scale)
# correlations, unadjusted p and adjusted p
stcres = data.frame(round(stcorrtest$r[,9],3),
		round(stcorrtest$p[9,],4),
		round(stcorrtest$p[,9],4))
names(stcres) = c("cor.","p-raw","p-adjusted")
stcres
# stcres
#            cor.  p-raw p-adjusted
# P070      0.666 0.0002     0.0063
# P077      0.683 0.0001     0.0038
# P126      0.604 0.0011     0.0293
# P132      0.707 0.0001     0.0018
# P135      0.721 0.0000     0.0011
# P143      0.784 0.0000     0.0001
# P144      0.567 0.0025     0.0602
# P146      0.743 0.0000     0.0005
# spaceTime 1.000 0.0000     0.0000

postDf$spaceTime=spaceTime

#########################################################################
## make emotional environment composite score (theme 3)
#########################################################################
emotEnvNames = c("P097","P100","P109","P110","P111","P114","P138",
		"P141")
emotEnvDf = postDf[,emotEnvNames]
emotEnvDf_scale = scale(emotEnvDf)
describe(emotEnvDf_scale)
emotEnv = apply(emotEnvDf_scale,1,sum)
emotEnvDf_scale = data.frame(emotEnvDf_scale, emotEnv = emotEnv)
emotEnvDf_scale
eecorrtest = corr.test(emotEnvDf_scale)
# correlations, unadjusted p and adjusted p
eecres = data.frame(round(eecorrtest$r[,9],3),
		round(eecorrtest$p[9,],4),
		round(eecorrtest$p[,9],4))
names(eecres) = c("cor.","p-raw","p-adjusted")
eecres
# eecres
#          cor.  p-raw p-adjusted
# P097    0.817 0.0000     0.0000
# P100    0.668 0.0002     0.0050
# P109    0.819 0.0000     0.0000
# P110    0.603 0.0011     0.0270
# P111    0.690 0.0001     0.0028
# P114    0.770 0.0000     0.0001
# P138    0.689 0.0001     0.0028
# P141    0.773 0.0000     0.0001
# emotEnv 1.000 0.0000     0.0000

postDf$emotEnv=emotEnv

#########################################################################
## make surrender composite score (theme 5)
#########################################################################
surrenderNames = c("P064","P079","P099","P106","P117")
surrenderDf = postDf[,surrenderNames]
surrenderDf_scale = scale(surrenderDf)
describe(surrenderDf_scale)
surrender = apply(surrenderDf_scale,1,sum)
surrenderDf_scale = data.frame(surrenderDf_scale, surrender = surrender)
surrenderDf_scale
surcorrtest = corr.test(surrenderDf_scale)
# correlations, unadjusted p and adjusted p
surcres = data.frame(round(surcorrtest$r[,6],3),
		round(surcorrtest$p[6,],4),
		round(surcorrtest$p[,6],4))
names(surcres) = c("cor.","p-raw","p-adjusted")
surcres
# surcres
#            cor. p-raw p-adjusted
# P064      0.619 8e-04     0.0076
# P079      0.632 5e-04     0.0058
# P099      0.757 0e+00     0.0001
# P106      0.731 0e+00     0.0003
# P117      0.669 2e-04     0.0023
# surrender 1.000 0e+00     0.0000

postDf$surrender=surrender

#########################################################################
## make Sensory Orientation (sensOr) composite score
#########################################################################
sensOrNames = c("P058","P078","P112")
sensOrDf = postDf[,sensOrNames]
sensOrDf_scale = scale(sensOrDf)
describe(sensOrDf_scale)
sensOr = apply(sensOrDf_scale,1,sum)
sensOrDf_scale = data.frame(sensOrDf_scale, sensOr = sensOr)
sensOrDf_scale
socorrtest = corr.test(sensOrDf_scale)
# correlations, unadjusted p and adjusted p
socres = data.frame(round(socorrtest$r[,4],3),
		round(socorrtest$p[4,],4),
		round(socorrtest$p[,4],4))
names(socres) = c("cor.","p-raw","p-adjusted")
socres
# socres
#         cor. p-raw p-adjusted
# P058   0.634 5e-04      0.002
# P078   0.816 0e+00      0.000
# P112   0.779 0e+00      0.000
# sensOr 1.000 0e+00      0.000


sensOrNames = c("P082","P104","P137")
sensOrDf = postDf[,sensOrNames]
sensOrDf_scale = scale(sensOrDf)
describe(sensOrDf_scale)
sensOr = apply(sensOrDf_scale,1,sum)
sensOrDf_scale = data.frame(sensOrDf_scale, sensOr = sensOr)
sensOrDf_scale
socorrtest = corr.test(sensOrDf_scale)
# correlations, unadjusted p and adjusted p
socres = data.frame(round(socorrtest$r[,4],3),
		round(socorrtest$p[4,],4),
		round(socorrtest$p[,4],4))
names(socres) = c("cor.","p-raw","p-adjusted")
socres
# socres
#         cor. p-raw p-adjusted
# P082   0.699 1e-04      3e-04
# P104   0.787 0e+00      0e+00
# P137   0.769 0e+00      0e+00
# sensOr 1.000 0e+00      0e+00

## haven't decided on this theme yet --> haven't added it yet
# postDf$sensOr=sensOr

#########################################################################
## make pain experience composite score (theme 6)
#########################################################################
painExpNames = c("P124","P131","P133","P142","P145","P148",
		"P155","P156")
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
# P124    0.827 0.0000     0.0000
# P131    0.545 0.0040     0.0962
# P133    0.859 0.0000     0.0000
# P142    0.681 0.0001     0.0042
# P145    0.567 0.0025     0.0707
# P148    0.678 0.0001     0.0044
# P155    0.478 0.0134     0.2817
# P156    0.661 0.0002     0.0071
# painExp 1.000 0.0000     0.0000

postDf$painExp=painExp
# left P155 in because it's so important to the theme

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
# P067         0.847     0      0e+00
# P084         0.737     0      1e-04
# P088         0.845     0      0e+00
# expectations 1.000     0      0e+00

postDf$expectations=expectations

#########################################################################
## make memory composite score (theme 7)
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
# P125   0.639 4e-04     0.0027
# P128   0.772 0e+00     0.0000
# P134   0.874 0e+00     0.0000
# P140   0.844 0e+00     0.0000
# memory 1.000 0e+00     0.0000

postDf$memory=memory

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
# P065            0.795 0e+00     0.0000
# P066            0.666 2e-04     0.0022
# P122            0.783 0e+00     0.0000
# P149            0.893 0e+00     0.0000
# P150            0.791 0e+00     0.0000
# P151            0.806 0e+00     0.0000
# outcomeMeasures 1.000 0e+00     0.0000

postDf$outcomeMeasures=outcomeMeasures

#########################################################################
## make unassigned composite score
#########################################################################
unassignedNames = c("P049","P057","P058","P062",
		"P068","P069","P073","P074","P075","P078","P080","P081","P082",
		"P083","P091","P104","P112","P113","P118","P120","P121","P129",
		"P131","P137")
# not including P059,P060,P061
unassignedDf = postDf[,unassignedNames]
unassignedDf_scale = scale(unassignedDf)
describe(unassignedDf_scale)
unassigned = apply(unassignedDf_scale,1,sum)
unassignedDf_scale = data.frame(unassignedDf_scale, unassigned = unassigned)
unassignedDf_scale
uncorrtest = corr.test(unassignedDf_scale)
# correlations, unadjusted p and adjusted p
uncres = data.frame(round(uncorrtest$r[,25],3),
		round(uncorrtest$p[25,],4),
		round(uncorrtest$p[,25],4))
names(uncres) = c("cor.","p-raw","p-adjusted")
uncres
# uncres
#              cor.  p-raw p-adjusted
# P049        0.083 0.6859          1
# P057        0.364 0.0673          1
# P058        0.222 0.2759          1
# P062        0.427 0.0298          1
# P068        0.293 0.1467          1
# P069        0.312 0.1211          1
# P073       -0.219 0.2831          1
# P074       -0.016 0.9383          1
# P075        0.124 0.5448          1
# P078        0.168 0.4130          1
# P080        0.160 0.4337          1
# P081       -0.216 0.2886          1
# P082        0.322 0.1083          1
# P083        0.332 0.0977          1
# P091        0.162 0.4291          1
# P104        0.281 0.1646          1
# P112        0.191 0.3496          1
# P113        0.489 0.0112          1
# P118       -0.071 0.7294          1
# P120        0.195 0.3408          1
# P121        0.138 0.5018          1
# P129        0.508 0.0081          1
# P131        0.418 0.0336          1
# P137        0.354 0.0762          1
# unassigned  1.000 0.0000          0

postDf$unassigned=unassigned



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
# P048     0.467 0.0162     1.0000
# P049    -0.183 0.3697     1.0000
# P050     0.449 0.0213     1.0000
# P057     0.146 0.4778     1.0000
# P058     0.095 0.6446     1.0000
# P059    -0.128 0.5321     1.0000
# P060     0.160 0.4360     1.0000
# P061     0.421 0.0322     1.0000
# P062     0.419 0.0333     1.0000
# P063     0.554 0.0033     1.0000
# P064     0.079 0.6999     1.0000
# P068     0.195 0.3398     1.0000
# P069     0.336 0.0934     1.0000
# P070     0.453 0.0201     1.0000
# P071     0.434 0.0267     1.0000
# P072     0.280 0.1662     1.0000
# P073    -0.114 0.5790     1.0000
# P074     0.132 0.5218     1.0000
# P075     0.000 0.9985     1.0000
# P076     0.482 0.0126     1.0000
# P077     0.414 0.0355     1.0000
# P078     0.019 0.9278     1.0000
# P079     0.027 0.8961     1.0000
# P080     0.172 0.4012     1.0000
# P082     0.252 0.2134     1.0000
# P083     0.268 0.1850     1.0000
# P085     0.495 0.0101     1.0000
# P086     0.340 0.0893     1.0000
# P087     0.635 0.0005     1.0000
# P089     0.713 0.0000     0.1246
# P090     0.343 0.0867     1.0000
# P092     0.596 0.0013     1.0000
# P093     0.696 0.0001     0.2195
# P094     0.661 0.0002     0.6685
# P095     0.671 0.0002     0.4914
# P096     0.458 0.0186     1.0000
# P097     0.527 0.0057     1.0000
# P098     0.490 0.0110     1.0000
# P099     0.256 0.2072     1.0000
# P101     0.655 0.0003     0.7916
# P102     0.461 0.0179     1.0000
# P103     0.372 0.0613     1.0000
# P104     0.008 0.9694     1.0000
# P105     0.419 0.0333     1.0000
# P106     0.384 0.0530     1.0000
# P107     0.451 0.0209     1.0000
# P108     0.464 0.0169     1.0000
# P109     0.676 0.0002     0.4285
# P110     0.368 0.0644     1.0000
# P111     0.397 0.0444     1.0000
# P112     0.192 0.3464     1.0000
# P113     0.618 0.0008     1.0000
# P114     0.574 0.0022     1.0000
# P115     0.387 0.0506     1.0000
# P116     0.369 0.0637     1.0000
# P117     0.231 0.2569     1.0000
# P118     0.078 0.7051     1.0000
# P119     0.498 0.0096     1.0000
# P120    -0.058 0.7770     1.0000
# P121     0.473 0.0147     1.0000
# P123     0.338 0.0909     1.0000
# P126     0.411 0.0372     1.0000
# P127     0.402 0.0416     1.0000
# P130     0.597 0.0013     1.0000
# P132     0.676 0.0002     0.4247
# P135     0.536 0.0048     1.0000
# P136     0.593 0.0014     1.0000
# P137     0.070 0.7326     1.0000
# P138     0.658 0.0003     0.7289
# P139     0.619 0.0007     1.0000
# P141     0.620 0.0007     1.0000
# P143     0.358 0.0723     1.0000
# P144     0.313 0.1196     1.0000
# P146     0.424 0.0311     1.0000
# P147     0.658 0.0003     0.7370
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
# P028  0.577 0.0020     0.2514
# P029  0.369 0.0636     1.0000
# P030  0.713 0.0000     0.0064
# P031  0.525 0.0059     0.6879
# P032  0.638 0.0005     0.0598
# P033  0.518 0.0068     0.7637
# P034  0.470 0.0155     1.0000
# P036  0.824 0.0000     0.0000
# P037  0.668 0.0002     0.0273
# P038  0.547 0.0038     0.4623
# P039  0.728 0.0000     0.0037
# P041  0.872 0.0000     0.0000
# P042  0.296 0.1423     1.0000
# P043  0.402 0.0420     1.0000
# P044  0.542 0.0042     0.5045
# P046  0.614 0.0008     0.1079
# P047  0.231 0.2554     1.0000
# panas 1.000 0.0000     0.0000

postDf$panas=panas
 #########################################################################
 ## plot correlation scales for these altThemes
 #########################################################################
 groupsDf = data.frame(intimateSec = intimateSec, RRR = RRR,
		 spaceTime = spaceTime, emotEnv = emotEnv, surrender = surrender, 
		 painExp = painExp, expectations = expectations, memory = memory,
		 outcomeMeasures = outcomeMeasures, panas = panas,
		 laborLand = laborLand)
 grpcorrtest = corr.test(groupsDf)
# correlations, unadjusted p and adjusted p
 grpcres = data.frame(round(grpcorrtest$r[,11],3),
		 round(grpcorrtest$p[11,],4),
		 round(grpcorrtest$p[,11],4))
 names(grpcres) = c("cor.","p-raw","p-adjusted")
 grpcres
# grpcres
#                   cor.  p-raw p-adjusted
# intimateSec      0.366 0.0657     1.0000
# RRR              0.432 0.0275     0.9071
# spaceTime        0.339 0.0898     1.0000
# emotEnv          0.470 0.0154     0.6001
# surrender        0.243 0.2320     1.0000
# painExp          0.405 0.0402     1.0000
# expectations     0.375 0.0590     1.0000
# memory          -0.141 0.4908     1.0000
# outcomeMeasures  0.572 0.0023     0.1095
# panas            0.410 0.0377     1.0000
# laborLand        1.000 0.0000     0.0000
 
 ## to get the whole correlation matrix
 grpcorrtest$r
# grpcorrtest$r
#                 intimateSec       RRR  spaceTime   emotEnv   surrender
# intimateSec      1.00000000 0.2454642  0.6523591 0.4791270  0.07571398
# RRR              0.24546421 1.0000000  0.4459724 0.5480445  0.00824280
# spaceTime        0.65235913 0.4459724  1.0000000 0.2535162 -0.02588060
# emotEnv          0.47912701 0.5480445  0.2535162 1.0000000  0.44447703
# surrender        0.07571398 0.0082428 -0.0258806 0.4444770  1.00000000
# painExp          0.33710623 0.4604491  0.2366106 0.6049429  0.30894921
# expectations     0.54760045 0.5706746  0.2801225 0.6392933  0.17781550
# memory           0.15475381 0.1074732  0.2492438 0.1658386 -0.23032947
# outcomeMeasures  0.39924964 0.5823228  0.4321095 0.7192045  0.48339497
# panas            0.37804045 0.4533526  0.1924924 0.4553272  0.18430693
# laborLand        0.36630389 0.4319762  0.3394228 0.4700587  0.24280619
#                    painExp expectations      memory outcomeMeasures      panas
# intimateSec     0.33710623    0.5476004  0.15475381      0.39924964 0.37804045
# RRR             0.46044912    0.5706746  0.10747317      0.58232280 0.45335256
# spaceTime       0.23661057    0.2801225  0.24924384      0.43210953 0.19249238
# emotEnv         0.60494285    0.6392933  0.16583859      0.71920452 0.45532721
# surrender       0.30894921    0.1778155 -0.23032947      0.48339497 0.18430693
# painExp         1.00000000    0.5821059  0.05880164      0.71474497 0.50879899
# expectations    0.58210591    1.0000000  0.19796676      0.54051818 0.36652964
# memory          0.05880164    0.1979668  1.00000000      0.04288652 0.09607528
# outcomeMeasures 0.71474497    0.5405182  0.04288652      1.00000000 0.47361407
# panas           0.50879899    0.3665296  0.09607528      0.47361407 1.00000000
# laborLand       0.40486065    0.3750484 -0.14141489      0.57166053 0.40955837
#                  laborLand
# intimateSec      0.3663039
# RRR              0.4319762
# spaceTime        0.3394228
# emotEnv          0.4700587
# surrender        0.2428062
# painExp          0.4048606
# expectations     0.3750484
# memory          -0.1414149
# outcomeMeasures  0.5716605
# panas            0.4095584
# laborLand        1.0000000
 
# old correlations with old themes
# grpcorrtest$r
#                 intuitMov   physEnv   emotEnv  fluidReal intensePres   painExp
# intuitMov       1.0000000 0.3451254 0.6824775 0.37299954   0.4460095 0.5881909
# physEnv         0.3451254 1.0000000 0.5975637 0.51913543   0.2404237 0.4293421
# emotEnv         0.6824775 0.5975637 1.0000000 0.66897639   0.4582295 0.6554013
# fluidReal       0.3729995 0.5191354 0.6689764 1.00000000   0.3609485 0.5124390
# intensePres     0.4460095 0.2404237 0.4582295 0.36094851   1.0000000 0.5556691
# painExp         0.5881909 0.4293421 0.6554013 0.51243903   0.5556691 1.0000000
# expectations    0.5265144 0.5793594 0.6313744 0.39481053   0.4832478 0.5627930
# outcomeMeasures 0.7200597 0.5152935 0.7534936 0.50571296   0.5573094 0.7602627
# panas           0.4129443 0.2222202 0.3908986 0.06843493   0.2544647 0.3367425
# laborLand       0.7358440 0.2240105 0.5816486 0.42102764   0.5796943 0.5356285
#                 expectations outcomeMeasures      panas laborLand
# intuitMov          0.5265144       0.7200597 0.41294433 0.7358440
# physEnv            0.5793594       0.5152935 0.22222019 0.2240105
# emotEnv            0.6313744       0.7534936 0.39089864 0.5816486
# fluidReal          0.3948105       0.5057130 0.06843493 0.4210276
# intensePres        0.4832478       0.5573094 0.25446468 0.5796943
# painExp            0.5627930       0.7602627 0.33674250 0.5356285
# expectations       1.0000000       0.5880393 0.33426585 0.3815260
# outcomeMeasures    0.5880393       1.0000000 0.40628292 0.6069820
# panas              0.3342658       0.4062829 1.00000000 0.3423835
# laborLand          0.3815260       0.6069820 0.34238347 1.0000000
 
 
 library(ggplot2)
 pdf("plots/scaleCorrelations.pdf")
 #########################################################################
 ## First, compare each theme to laborLand
 #########################################################################
 ## intimateSec
 adf = grpcres[rownames(grpcres)=="intimateSec",]
 adf$x = -15
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=intimateSec, y=laborLand))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## RRR
 adf = grpcres[rownames(grpcres)=="RRR",]
 adf$x = -6
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=RRR, y=laborLand))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## spaceTime
 adf = grpcres[rownames(grpcres)=="spaceTime",]
 adf$x = -20
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=spaceTime, y=laborLand))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## emotEnv
 adf = grpcres[rownames(grpcres)=="emotEnv",]
 adf$x = -10
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=emotEnv, y=laborLand))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## surrender
 adf = grpcres[rownames(grpcres)=="surrender",]
 adf$x = -10
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=surrender, y=laborLand))+geom_point()+
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
 groupsDf$P016 = postDf$P016
 adf = grpcres[rownames(grpcres)=="outcomeMeasures",]
 adf$x = -10
 adf$y = 5
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=laborLand, y=outcomeMeasures, color=P016))+geom_point()
 #geom_smooth(method=lm)+
 #geom_text(aes(x=x, y=y, label=label),data=adf)
 ## panas
 adf = grpcres[rownames(grpcres)=="panas",]
 adf$x = -10
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=panas, y=laborLand))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 
 #########################################################################
 ## Now compare each theme to outcome measures; r- and p-values are
 ## not correct for these graphs
 #########################################################################
 ## intimateSec
 adf = grpcres[rownames(grpcres)=="intimateSec",]
 adf$x = -15
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=intimateSec, y=outcomeMeasures))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## RRR
 adf = grpcres[rownames(grpcres)=="RRR",]
 adf$x = -6
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=RRR, y=outcomeMeasures))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## spaceTime
 adf = grpcres[rownames(grpcres)=="spaceTime",]
 adf$x = -20
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=spaceTime, y=outcomeMeasures))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## emotEnv
 adf = grpcres[rownames(grpcres)=="emotEnv",]
 adf$x = -10
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=emotEnv, y=outcomeMeasures))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## surrender
 adf = grpcres[rownames(grpcres)=="surrender",]
 adf$x = -10
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=surrender, y=outcomeMeasures))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## painExp
 adf = grpcres[rownames(grpcres)=="painExp",]
 adf$x = -10
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=painExp, y=outcomeMeasures))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## expectations
 adf = grpcres[rownames(grpcres)=="expectations",]
 adf$x = -3
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=expectations, y=outcomeMeasures))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## outcomeMeasures
 adf = grpcres[rownames(grpcres)=="outcomeMeasures",]
 adf$x = -10
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=outcomeMeasures, y=outcomeMeasures))+geom_point()+
		 geom_smooth(method=lm)+
		 geom_text(aes(x=x, y=y, label=label),data=adf)
 ## panas
 adf = grpcres[rownames(grpcres)=="panas",]
 adf$x = -10
 adf$y = 10
 adf$label = paste("cor=",adf$cor.,"\np-raw=",adf$'p-raw',
		 "\np-adjusted=",adf$'p-adjusted')
 ggplot(groupsDf,aes(x=panas, y=outcomeMeasures))+geom_point()+
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
 
# don't actually save these themes
#save(postDf,newpostDf,origPostDf,descPostDf,postNumVarNames,file="rdata/postData.RData")
