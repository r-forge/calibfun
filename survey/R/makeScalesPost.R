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
laborLandNames = c("P085","P086","P087","P089","P093","P094","P101",
		"P103","P107","P108","P115","P123","P127","P130","P136")
# this new score messes up my other themes. I should go back and
# redo all theme grouping around this laborLand score
laborLandDf = postDf[,laborLandNames]
laborLandDf_scale = scale(laborLandDf)
describe(laborLandDf_scale)
laborLand = apply(laborLandDf_scale,1,sum)
laborLandDf_scale = data.frame(laborLandDf_scale, laborLand = laborLand)
laborLandDf_scale
llcorrtest = corr.test(laborLandDf_scale)
# correlations, unadjusted p and adjusted p
llcres = data.frame(round(llcorrtest$r[,16],3),
		round(llcorrtest$p[16,],4),
		round(llcorrtest$p[,16],4))
names(llcres) = c("cor.","p-raw","p-adjusted")
llcres
# llcres
#            cor.  p-raw p-adjusted
# P085      0.765 0.0000     0.0006
# P086      0.670 0.0002     0.0188
# P087      0.666 0.0002     0.0215
# P089      0.843 0.0000     0.0000
# P093      0.793 0.0000     0.0002
# P094      0.904 0.0000     0.0000
# P101      0.728 0.0000     0.0028
# P103      0.358 0.0724     1.0000
# P107      0.642 0.0004     0.0404
# P108      0.573 0.0022     0.1964
# P115      0.520 0.0064     0.4936
# P123      0.555 0.0033     0.2765
# P127      0.640 0.0004     0.0429
# P130      0.702 0.0001     0.0071
# P136      0.674 0.0002     0.0171
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


save(postDf,newpostDf,origPostDf,descPostDf,postNumVarNames,file="rdata/postData.RData")
