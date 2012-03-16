###############################################################################
## factor-themeAnalyses.R
##
## check to see if themes and factors correlate with outcomes etc.
## 
## Author: Haaland
###############################################################################

load("rdata/preData.RData")
load("rdata/postData.RData")
library(ggplot2)
library(psych)
library(GPArotation)
library(Hmisc)

## copied from plotScalesPost.R; took out panas
groups1Df = data.frame(intuitMov = postDf$intuitMov, physEnv = postDf$physEnv,
		emotEnv = postDf$emotEnv, fluidReal = postDf$fluidReal, intensePres = postDf$intensePres,
		painExp = postDf$painExp, expectations = postDf$expectations,
		vocals = postDf$vocals, laborLand = postDf$laborLand, 
		outcomeMeasures = postDf$outcomeMeasures)
grp1corrtest = corr.test(groups1Df)
# correlations, unadjusted p and adjusted p
grp1cres = data.frame(round(grp1corrtest$r[,10],3),
		round(grp1corrtest$p[10,],4),
		round(grp1corrtest$p[,10],4))
names(grp1cres) = c("cor.","p-raw","p-adjusted")
grp1cres
# grp1cres
#                  cor.  p-raw p-adjusted
# intuitMov       0.696 0.0000     0.0001
# physEnv         0.552 0.0006     0.0158
# emotEnv         0.733 0.0000     0.0000
# fluidReal       0.554 0.0006     0.0154
# intensePres     0.543 0.0007     0.0188
# painExp         0.760 0.0000     0.0000
# expectations    0.588 0.0002     0.0069
# vocals          0.028 0.8753     1.0000
# laborLand       0.627 0.0001     0.0021
# outcomeMeasures 1.000 0.0000     0.0000
## Thus, all but vocals correlate with outcomeMeasures


groupFactorDf = data.frame(M1 = postDf$M1, M2 = postDf$M2, 
		M3 = postDf$M3, laborLand = postDf$laborLand, M5 = postDf$M5, 
		outcomeMeasures = postDf$outcomeMeasures)
grpfactorcorrtest = corr.test(groupFactorDf)
# correlations, unadjusted p and adjusted p
grpfactorcres = data.frame(round(grpfactorcorrtest$r[,6],3),
		round(grpfactorcorrtest$p[6,],4),
		round(grpfactorcorrtest$p[,6],4))
names(grpfactorcres) = c("cor.","p-raw","p-adjusted")
grpfactorcres
# grpfactorcres
#                   cor.  p-raw p-adjusted
# M1               0.737 0.0000     0.0000
# M2              -0.071 0.6851     1.0000
# M3               0.409 0.0146     0.1316
# laborLand        0.627 0.0001     0.0008
# M5               0.532 0.0010     0.0130
# outcomeMeasures  1.000 0.0000     0.0000


groupFactorrefDf = data.frame(M1ref = postDf$M1ref, M2ref = postDf$M2ref, 
		M3ref = postDf$M3ref, laborLand = postDf$laborLand, M5ref = postDf$M5ref, 
		outcomeMeasures = postDf$outcomeMeasures)
grpfactorrefcorrtest = corr.test(groupFactorrefDf)
# correlations, unadjusted p and adjusted p
grpfactorrefcres = data.frame(round(grpfactorrefcorrtest$r[,6],3),
		round(grpfactorrefcorrtest$p[6,],4),
		round(grpfactorrefcorrtest$p[,6],4))
names(grpfactorrefcres) = c("cor.","p-raw","p-adjusted")
grpfactorrefcres
# grpfactorrefcres
#                   cor.  p-raw p-adjusted
# M1ref            0.790 0.0000     0.0000
# M2ref           -0.144 0.4083     1.0000
# M3ref            0.500 0.0022     0.0246
# laborLand        0.627 0.0001     0.0008
# M5ref            0.576 0.0003     0.0038
# outcomeMeasures  1.000 0.0000     0.0000

## Thus, M1, M3, and M5 (and laborLand) correlate strongly with outcomeMeasures



