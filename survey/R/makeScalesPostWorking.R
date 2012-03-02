###############################################################################
## makeScalesPost.R
##
## make attribute scales
## 
## Author: Haaland
###############################################################################
load("rdata/postData.RData")

#########################################################################
## in this document, I'm reverting to my original theme judgments
## and, based on my new laborLand, cleaning them up again from
## scratch (original data from Appendix A)
#########################################################################

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
## make intuitive movements composite score (theme 1)
#########################################################################
intuitMovNames = c("P078","P100","P106","P112","P121")
# take out P086,P087,P089,P093,P094,P101,P103,P107,P108,P115,P127,
# P130,P136
# removed P057
# removed P099 to add to vocals
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
# P078      0.612 0.0009     0.0124
# P100      0.567 0.0025     0.0276
# P106      0.806 0.0000     0.0000
# P112      0.610 0.0009     0.0124
# P121      0.531 0.0053     0.0495
# intuitMov 1.000 0.0000     0.0000

postDf$intuitMov=intuitMov

#########################################################################
## make physical environment composite score (theme 2)
#########################################################################
physEnvNames = c("P048","P051","P083","P102")
# P053-P056 deleted for quality control
# removed P049,P110
physEnvDf = postDf[,physEnvNames]
physEnvDf_scale = scale(physEnvDf)
describe(physEnvDf_scale)
physEnv = apply(physEnvDf_scale,1,sum)
physEnvDf_scale = data.frame(physEnvDf_scale, physEnv = physEnv)
physEnvDf_scale
pecorrtest = corr.test(physEnvDf_scale)
# correlations, unadjusted p and adjusted p
pecres = data.frame(round(pecorrtest$r[,5],3),
		round(pecorrtest$p[5,],4),
		round(pecorrtest$p[,5],4))
names(pecres) = c("cor.","p-raw","p-adjusted")
pecres
# pecres
#          cor. p-raw p-adjusted
# P048    0.826 0e+00     0.0000
# P051    0.610 9e-04     0.0066
# P083    0.648 3e-04     0.0028
# P102    0.784 0e+00     0.0000
# physEnv 1.000 0e+00     0.0000

postDf$physEnv=physEnv

#########################################################################
## make emotional environment composite score (theme 3)
#########################################################################
emotEnv2Names = c("P076","P090","P095","P097","P098",
		"P109","P113","P138")
# removed P052,P074,P064,P075,P117,P118,P070,P062,P092,P111,P063,P114
emotEnv2Df = postDf[,emotEnv2Names]
emotEnv2Df_scale = scale(emotEnv2Df)
describe(emotEnv2Df_scale)
emotEnv2 = apply(emotEnv2Df_scale,1,sum)
emotEnv2Df_scale = data.frame(emotEnv2Df_scale, emotEnv2 = emotEnv2)
emotEnv2Df_scale
ee2corrtest = corr.test(emotEnv2Df_scale)
# correlations, unadjusted p and adjusted p
ee2cres = data.frame(round(ee2corrtest$r[,9],3),
		round(ee2corrtest$p[9,],4),
		round(ee2corrtest$p[,9],4))
names(ee2cres) = c("cor.","p-raw","p-adjusted")
ee2cres
# ee2cres
#           cor. p-raw p-adjusted
# P076     0.622 7e-04     0.0179
# P090     0.681 1e-04     0.0040
# P095     0.707 1e-04     0.0018
# P097     0.716 0e+00     0.0013
# P098     0.682 1e-04     0.0040
# P109     0.680 1e-04     0.0040
# P113     0.782 0e+00     0.0001
# P138     0.754 0e+00     0.0003
# emotEnv2 1.000 0e+00     0.0000

postDf$emotEnv2=emotEnv2

#########################################################################
## make fluid reality composite score (theme 4)
#########################################################################
fluidRealNames = c("P077","P126","P132","P135","P143",
		"P144","P146")
# removed P064,P137,P110,P104,P141
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
#            cor.  p-raw p-adjusted
# P077      0.640 0.0004     0.0098
# P126      0.624 0.0007     0.0145
# P132      0.694 0.0001     0.0021
# P135      0.749 0.0000     0.0003
# P143      0.827 0.0000     0.0000
# P144      0.581 0.0019     0.0374
# P146      0.752 0.0000     0.0003
# fluidReal 1.000 0.0000     0.0000

postDf$fluidReal=fluidReal

#########################################################################
## make intense presence composite score (theme 5)
#########################################################################
intensePresNames = c("P071",
		"P072","P096","P105","P116","P119",
		"P139","P141","P147")
# removed P069,P059,P060,P073,P120,P137,P058,P061,P068,P082,P104
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
#              cor. p-raw p-adjusted
# P071        0.629 6e-04     0.0228
# P072        0.611 9e-04     0.0341
# P096        0.621 7e-04     0.0278
# P105        0.701 1e-04     0.0027
# P116        0.729 0e+00     0.0010
# P119        0.609 1e-03     0.0347
# P139        0.823 0e+00     0.0000
# P141        0.609 1e-03     0.0347
# P147        0.672 2e-04     0.0070
# intensePres 1.000 0e+00     0.0000

postDf$intensePres=intensePres

#########################################################################
## make pain experience composite score (theme 6)
#########################################################################
painExpNames = c("P124","P131","P133","P142","P145","P148","P155",
		"P156")
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
# P125   0.639 4e-04     0.0027
# P128   0.772 0e+00     0.0000
# P134   0.874 0e+00     0.0000
# P140   0.844 0e+00     0.0000
# memory 1.000 0e+00     0.0000

postDf$memory=memory

#########################################################################
## make focus composite score (theme 7)
#########################################################################
focusNames = c("P058","P059","P060","P061")
focusDf = postDf[,focusNames]
focusDf_scale = scale(focusDf)
describe(focusDf_scale)
focus = apply(focusDf_scale,1,sum)
focusDf_scale = data.frame(focusDf_scale, focus = focus)
focusDf_scale
fccorrtest = corr.test(focusDf_scale)
# correlations, unadjusted p and adjusted p
fccres = data.frame(round(fccorrtest$r[,5],3),
		round(fccorrtest$p[5,],4),
		round(fccorrtest$p[,5],4))
names(fccres) = c("cor.","p-raw","p-adjusted")
fccres
# fccres
#        cor.  p-raw p-adjusted
# P058  0.535 0.0048     0.0338
# P059  0.736 0.0000     0.0002
# P060  0.686 0.0001     0.0010
# P061  0.639 0.0004     0.0036
# focus 1.000 0.0000     0.0000

postDf$focus=focus

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
#         cor. p-raw p-adjusted
# P057   0.706 1e-04     0.0003
# P099   0.560 3e-03     0.0118
# P103   0.824 0e+00     0.0000
# vocals 1.000 0e+00     0.0000

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
## plot correlations
#########################################################################
groupsDf = data.frame(intuitMov = intuitMov, physEnv = physEnv,
		emotEnv2 = emotEnv2, fluidReal = fluidReal, intensePres = intensePres,
		painExp = painExp, expectations = expectations, focus = focus,
		vocals = vocals, outcomeMeasures = outcomeMeasures, panas = panas,
		laborLand = laborLand)
grpcorrtest = corr.test(groupsDf)
# correlations, unadjusted p and adjusted p
grpcres = data.frame(round(grpcorrtest$r[,12],3),
		round(grpcorrtest$p[12,],4),
		round(grpcorrtest$p[,12],4))
names(grpcres) = c("cor.","p-raw","p-adjusted")
grpcres
# grpcres
#                  cor.  p-raw p-adjusted
# intuitMov       0.275 0.1744     1.0000
# physEnv         0.073 0.7248     1.0000
# emotEnv2        0.455 0.0196     0.8447
# fluidReal       0.329 0.1005     1.0000
# intensePres     0.491 0.0108     0.5440
# painExp         0.423 0.0314     1.0000
# expectations    0.375 0.0590     1.0000
# focus           0.049 0.8117     1.0000
# vocals          0.240 0.2371     1.0000
# outcomeMeasures 0.572 0.0023     0.1369
# panas           0.410 0.0377     1.0000
# laborLand       1.000 0.0000     0.0000
# > cat("Synch1330656705573375000\n");

# grpcres
#                  cor.  p-raw p-adjusted
# intuitMov       0.736 0.0000     0.0000
# physEnv         0.224 0.1958     0.7008
# emotEnv         0.582 0.0002     0.0082
# fluidReal       0.421 0.0118     0.2003
# intensePres     0.580 0.0003     0.0084
# painExp         0.536 0.0009     0.0247
# expectations    0.382 0.0237     0.2848
# outcomeMeasures 0.607 0.0001     0.0041
# panas           0.342 0.0441     0.3807
# laborLand       1.000 0.0000     0.0000









## to get the whole correlation matrix
grpcorrtest$r
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
## panas
adf = grpcres[rownames(grpcres)=="panas",]
adf$x = -3
adf$y = 6
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

save(postDf,descPostDf,postNumVarNames,file="rdata/postData.RData")
