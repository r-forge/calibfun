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
laborLandNames = c("P071","P072","P073","P079","P080","P085","P086",
		"P089","P093","P094","P108","P115","P132","P146")
# 11 of these 14 overlap with other themes
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
#             cor.  p-raw p-adjusted
# P071       0.674 0.0004     0.0377
# P072       0.176 0.4229     1.0000
# P073      -0.146 0.5055     1.0000
# P079      -0.092 0.6759     1.0000
# P080       0.438 0.0365     1.0000
# P085       0.777 0.0000     0.0013
# P086       0.722 0.0001     0.0097
# P089       0.847 0.0000     0.0000
# P093       0.782 0.0000     0.0011
# P094       0.913 0.0000     0.0000
# P108       0.624 0.0015     0.1248
# P115       0.533 0.0089     0.7112
# P132       0.677 0.0004     0.0351
# P146       0.708 0.0002     0.0148
# laborLand  1.000 0.0000     0.0000


postDf$laborLand=laborLand

#########################################################################
## make intuitive movements composite score (theme 1)
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
imcorrtest = corr.test(intuitMovDf_scale)
# correlations, unadjusted p and adjusted p
imcres = data.frame(round(imcorrtest$r[,21],3),
		round(imcorrtest$p[21,],4),
		round(imcorrtest$p[,21],4))
names(imcres) = c("cor.","p-raw","p-adjusted")
imcres
# imcres
#            cor.  p-raw p-adjusted
# P057      0.169 0.3628     1.0000
# P078      0.066 0.7226     1.0000
# P086      0.442 0.0128     1.0000
# P087      0.671 0.0000     0.0073
# P089      0.693 0.0000     0.0032
# P093      0.749 0.0000     0.0003
# P094      0.818 0.0000     0.0000
# P099      0.171 0.3589     1.0000
# P100      0.703 0.0000     0.0021
# P101      0.760 0.0000     0.0002
# P103      0.397 0.0271     1.0000
# P106      0.378 0.0358     1.0000
# P107      0.658 0.0001     0.0114
# P108      0.563 0.0010     0.1760
# P112      0.455 0.0101     1.0000
# P115      0.640 0.0001     0.0206
# P121      0.482 0.0060     0.9981
# P127      0.574 0.0007     0.1357
# P130      0.642 0.0001     0.0200
# P136      0.609 0.0003     0.0528
# intuitMov 1.000 0.0000     0.0000

postDf$intuitMov=intuitMov
#########################################################################
## make physical environment composite score (theme 2)
#########################################################################
physEnvNames = c("P048","P049","P051","P053","P054","P055","P056",
		"P083","P102","P110")
physEnvDf = postDf[,physEnvNames]
physEnvDf_scale = scale(physEnvDf)
describe(physEnvDf_scale)
physEnv = apply(physEnvDf_scale,1,sum)
physEnvDf_scale = data.frame(physEnvDf_scale, physEnv = physEnv)
physEnvDf_scale
pecorrtest = corr.test(physEnvDf_scale)
# correlations, unadjusted p and adjusted p
pecres = data.frame(round(pecorrtest$r[,11],3),
		round(pecorrtest$p[11,],4),
		round(pecorrtest$p[,11],4))
names(pecres) = c("cor.","p-raw","p-adjusted")
pecres
# pecres
#           cor.  p-raw p-adjusted
# P048     0.297 0.1049     1.0000
# P049    -0.102 0.5860     1.0000
# P051     0.515 0.0030     0.1380
# P053     0.021 0.9103     1.0000
# P054     0.705 0.0000     0.0005
# P055     0.645 0.0001     0.0043
# P056     0.705 0.0000     0.0005
# P083     0.551 0.0013     0.0624
# P102     0.677 0.0000     0.0014
# P110     0.172 0.3549     1.0000
# physEnv  1.000 0.0000     0.0000

postDf$physEnv=physEnv
#########################################################################
## make emotional environment composite score (theme 3)
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
eecorrtest = corr.test(emotEnvDf_scale)
# correlations, unadjusted p and adjusted p
eecres = data.frame(round(eecorrtest$r[,22],3),
		round(eecorrtest$p[22,],4),
		round(eecorrtest$p[,22],4))
names(eecres) = c("cor.","p-raw","p-adjusted")
eecres
# eecres
#          cor.  p-raw p-adjusted
# P052    0.340 0.0609     1.0000
# P062    0.503 0.0039     0.7802
# P063    0.652 0.0001     0.0162
# P064    0.416 0.0201     1.0000
# P070    0.427 0.0167     1.0000
# P074    0.303 0.0978     1.0000
# P075    0.074 0.6942     1.0000
# P076    0.517 0.0029     0.5952
# P090    0.316 0.0834     1.0000
# P092    0.618 0.0002     0.0481
# P095    0.701 0.0000     0.0025
# P097    0.661 0.0001     0.0119
# P098    0.602 0.0003     0.0757
# P109    0.727 0.0000     0.0008
# P111    0.532 0.0021     0.4321
# P113    0.803 0.0000     0.0000
# P114    0.556 0.0012     0.2440
# P117    0.357 0.0484     1.0000
# P118    0.244 0.1857     1.0000
# P123    0.575 0.0007     0.1558
# P138    0.536 0.0019     0.3979
# emotEnv 1.000 0.0000     0.0000

postDf$emotEnv=emotEnv
#########################################################################
## make fluid reality composite score (theme 4)
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
frcorrtest = corr.test(fluidRealDf_scale)
# correlations, unadjusted p and adjusted p
frcres = data.frame(round(frcorrtest$r[,15],3),
		round(frcorrtest$p[15,],4),
		round(frcorrtest$p[,15],4))
names(frcres) = c("cor.","p-raw","p-adjusted")
frcres
# frcres
#             cor.  p-raw p-adjusted
# P077       0.452 0.0106     0.9680
# P104       0.360 0.0468     1.0000
# P125       0.436 0.0143     1.0000
# P126       0.449 0.0113     1.0000
# P128       0.190 0.3071     1.0000
# P132       0.712 0.0000     0.0007
# P134      -0.023 0.9018     1.0000
# P135       0.671 0.0000     0.0037
# P137       0.427 0.0165     1.0000
# P140      -0.185 0.3200     1.0000
# P141       0.514 0.0031     0.2944
# P143       0.646 0.0001     0.0085
# P144       0.535 0.0019     0.1860
# P146       0.687 0.0000     0.0020
# fluidReal  1.000 0.0000     0.0000


postDf$fluidReal=fluidReal
#########################################################################
## make intense presence composite score (theme 5)
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
ipcorrtest = corr.test(intensePresDf_scale)
# correlations, unadjusted p and adjusted p
ipcres = data.frame(round(ipcorrtest$r[,18],3),
		round(ipcorrtest$p[18,],4),
		round(ipcorrtest$p[,18],4))
names(ipcres) = c("cor.","p-raw","p-adjusted")
ipcres
# ipcres
#              cor.  p-raw p-adjusted
# P058        0.492 0.0050     0.6834
# P059        0.059 0.7543     1.0000
# P060        0.121 0.5182     1.0000
# P061        0.447 0.0117     1.0000
# P068        0.251 0.1724     1.0000
# P069        0.073 0.6950     1.0000
# P071        0.658 0.0001     0.0086
# P072        0.593 0.0004     0.0636
# P073        0.242 0.1897     1.0000
# P082        0.344 0.0578     1.0000
# P096        0.504 0.0039     0.5384
# P105        0.586 0.0005     0.0769
# P116        0.552 0.0013     0.1839
# P119        0.701 0.0000     0.0017
# P120        0.483 0.0060     0.8035
# P139        0.724 0.0000     0.0006
# P147        0.632 0.0001     0.0201
# intensePres 1.000 0.0000     0.0000

postDf$intensePres=intensePres
#########################################################################
## make pain experience composite score (theme 6)
#########################################################################
painExpNames = c("P050","P124","P129","P131","P133","P142","P145",
		"P148","P154","P155","P156")
painExpDf = postDf[,painExpNames]
painExpDf_scale = scale(painExpDf)
describe(painExpDf_scale)
painExp = apply(painExpDf_scale,1,sum)
painExpDf_scale = data.frame(painExpDf_scale, painExp = painExp)
painExpDf_scale
pexcorrtest = corr.test(painExpDf_scale)
# correlations, unadjusted p and adjusted p
pexcres = data.frame(round(pexcorrtest$r[,12],3),
		round(pexcorrtest$p[12,],4),
		round(pexcorrtest$p[,12],4))
names(pexcres) = c("cor.","p-raw","p-adjusted")
pexcres
# pexcres
#          cor.  p-raw p-adjusted
# P050    0.183 0.3249     1.0000
# P124    0.862 0.0000     0.0000
# P129    0.330 0.0700     1.0000
# P131    0.477 0.0067     0.3070
# P133    0.915 0.0000     0.0000
# P142    0.710 0.0000     0.0005
# P145    0.776 0.0000     0.0000
# P148    0.691 0.0000     0.0010
# P154    0.379 0.0353     1.0000
# P155    0.466 0.0082     0.3709
# P156    0.593 0.0004     0.0235
# painExp 1.000 0.0000     0.0000

postDf$painExp=painExp
#########################################################################
## make expectations composite score (theme 7)
#########################################################################
expectationsNames = c("P067","P081","P084","P088","P091")
expectationsDf = postDf[,expectationsNames]
expectationsDf_scale = scale(expectationsDf)
describe(expectationsDf_scale)
expectations = apply(expectationsDf_scale,1,sum)
expectationsDf_scale = data.frame(expectationsDf_scale, expectations = expectations)
expectationsDf_scale
excorrtest = corr.test(expectationsDf_scale)
# correlations, unadjusted p and adjusted p
excres = data.frame(round(excorrtest$r[,6],3),
		round(excorrtest$p[6,],4),
		round(excorrtest$p[,6],4))
names(excres) = c("cor.","p-raw","p-adjusted")
excres
# excres
#                cor.  p-raw p-adjusted
# P067          0.779 0.0000     0.0000
# P081         -0.069 0.7111     1.0000
# P084          0.586 0.0005     0.0070
# P088          0.569 0.0008     0.0101
# P091          0.122 0.5141     1.0000
# expectations  1.000 0.0000     0.0000

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
# P065            0.821 0e+00     0.0000
# P066            0.622 2e-04     0.0022
# P122            0.777 0e+00     0.0000
# P149            0.895 0e+00     0.0000
# P150            0.808 0e+00     0.0000
# P151            0.732 0e+00     0.0000
# outcomeMeasures 1.000 0e+00     0.0000

postDf$outcomeMeasures=outcomeMeasures


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
## correlation of each theme with laborLand
# grpcres
#                  cor.  p-raw p-adjusted
# intuitMov       0.787 0.0000     0.0000
# physEnv         0.297 0.1041     0.4165
# emotEnv         0.467 0.0081     0.0907
# fluidReal       0.709 0.0000     0.0002
# intensePres     0.673 0.0000     0.0010
# painExp         0.617 0.0002     0.0059
# expectations    0.406 0.0234     0.1639
# outcomeMeasures 0.615 0.0002     0.0059
# laborLand       1.000 0.0000     0.0000

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