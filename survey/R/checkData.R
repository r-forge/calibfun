###############################################################################
## checkData.R
##
## look for outliers in repondents, questions to be thrown out, etc.
## 
## Author: Haaland
###############################################################################
library(ggplot2)
library(psych)
library(GPArotation)
library(Hmisc)
load("rdata/postData.RData")
load("rdata/origPostData.RData")

#########################################################################
## Who might need to be removed:
## Cesareans - 4
## Medications - 4 additional (8 total)
## Long pp interval women - 6 (maybe only longest 2?)
#########################################################################

#    P017 P018 P016 P015
# those who had an intentional hospital birth (5)
#4     3    1    1    1
#8     3    1    1    1
#15    3    1    1    1
#27    3    1    2    1
#36    3    1    1    1

# those who had an unintentional hospital birth vaginally (4)
#18    3    2    1    1
#11    3    2    2    1
#34    3    2    2    1
#31    3    2    3    1

# those who had an unintended hospital birth by cesarean (4)
#17    3    2    2    2
#24    3    2    2    2
#35    3    2    2    2
#29    3    2    4    2

#########################################################################
## are cesarean women outliers? looking at P015 against all themes
#########################################################################
# fine
ggplot(postDf,aes(x=P015, y=laborLand, color=P015))+geom_point()
#fine
ggplot(postDf,aes(x=intuitMov, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=intuitMov, color=P015))+geom_point()
# looks like 2 cesareans are lowest on physEnv
ggplot(postDf,aes(x=physEnv, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=physEnv, color=P015))+geom_point()
# fine
ggplot(postDf,aes(x=emotEnv, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=emotEnv, color=P015))+geom_point()
# very large range: 1 is best; 1 is almost worst
ggplot(postDf,aes(x=fluidReal, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=fluidReal, color=P015))+geom_point()
# fine
ggplot(postDf,aes(x=intensePres, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=intensePres, color=P015))+geom_point()
# 3 are very high; 1 is very low (but within range)
ggplot(postDf,aes(x=painExp, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=painExp, color=P015))+geom_point()
# all in lower half, but no problem
ggplot(postDf,aes(x=expectations, y=laborLand, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=expectations, color=P015))+geom_point()
# fine 
ggplot(postDf,aes(x=laborLand, y=outcomeMeasures, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=outcomeMeasures, color=P015))+geom_point()
# fine
ggplot(postDf,aes(x=laborLand, y=stateQs, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=stateQs, color=P015))+geom_point()
# fine
ggplot(postDf,aes(x=laborLand, y=panas, color=P015))+geom_point()
ggplot(postDf,aes(x=P015, y=panas, color=P015))+geom_point()

# check anovas for unexpected (fluidReal; painExp; expectations)
aovCheck1 = aov(fluidReal~P015,data=postDf)
summary(aovCheck1)
# summary(aovCheck1)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P015         1    0.7   0.739   0.044  0.834
# Residuals   33  549.2  16.644               

aovCheck2 = aov(painExp~P015,data=postDf)
summary(aovCheck2)
# summary(aovCheck2)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P015         1   37.2   37.17   1.132  0.295
# Residuals   33 1083.9   32.84               

aovCheck3 = aov(expectations~P015,data=postDf)
summary(aovCheck3)
# summary(aovCheck3)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P015         1   19.0   19.03    1.48  0.232
# Residuals   33  424.2   12.85                

## based on anova, cesareans are not significantly different from
## non-cesarean population on anything

loopvars = c("laborLand","intuitMov","physEnv","emotEnv","fluidReal",
		"intensePres","painExp","expectations","outcomeMeasures",
		"vocals","memory","panas","tActualAct","tDiffAct","tActualPush",
		"tDiffPush","tPerceivedAct","tPerceivedPush")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"drugsplit")]
	names(df)[1]="y"
	aovCheck4=aov(y~drugsplit,data=df)
	print(summary(aovCheck4))
}
ggplot(postDf,aes(x=drugsplit, y=intuitMov, color=drugsplit))+ 
		stat_summary(fun.data = "mean_cl_boot") 
#i= 2  var= intuitMov 
#			 Df Sum Sq Mean Sq F value Pr(>F)  
#drugsplit    1  43.07   43.07    4.65 0.0384 *
#Residuals   33 305.66    9.26 
ddply(postDf,.(drugsplit),function(df){
			data.frame(mean = mean(df$intuitMov),
					sd=sd(df$intuitMov),
					n=nrow(df))
		})
#  drugsplit       mean       sd  n
#1     drugs -2.0380339 2.872158  8
#2   nodrugs  0.6038619 3.087918 27
t.test(intuitMov~drugsplit, var.equal = TRUE, data=postDf)
# t.test(intuitMov~drugsplit, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  intuitMov by drugsplit 
# t = -2.1565, df = 33, p-value = 0.03843
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -5.1343739 -0.1494176 
# sample estimates:
#   mean in group drugs mean in group nodrugs 
#            -2.0380339             0.6038619 


ggplot(postDf,aes(x=drugsplit, y=physEnv, color=drugsplit))+ 
		stat_summary(fun.data = "mean_cl_boot") 
#i= 3  var= physEnv 
#			 Df Sum Sq Mean Sq F value  Pr(>F)   
#drugsplit    1  62.82   62.82    7.51 0.00983 **
#Residuals   33 276.07    8.37  
ddply(postDf,.(drugsplit),function(df){
			data.frame(mean = mean(df$physEnv),
					sd=sd(df$physEnv),
					n=nrow(df))
		})
#  drugsplit       mean       sd  n
#1     drugs -2.4613245 3.472592  8
#2   nodrugs  0.7292813 2.715066 27
t.test(physEnv~drugsplit, var.equal = TRUE, data=postDf)
# t.test(physEnv~drugsplit, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  physEnv by drugsplit 
# t = -2.7404, df = 33, p-value = 0.009826
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -5.559379 -0.821833 
# sample estimates:
#   mean in group drugs mean in group nodrugs 
#            -2.4613245             0.7292813 
# 


ggplot(postDf,aes(x=drugsplit, y=expectations, color=drugsplit))+ 
		stat_summary(fun.data = "mean_cl_boot") 
#i= 8  var= expectations 
#			 Df Sum Sq Mean Sq F value  Pr(>F)   
#drugsplit    1   96.1   96.08   9.134 0.00482 **
#Residuals   33  347.1   10.52  
ddply(postDf,.(drugsplit),function(df){
			data.frame(mean = mean(df$expectations),
					sd=sd(df$expectations),
					n=nrow(df))
		})
#  drugsplit       mean       sd  n
#1     drugs -3.0438296 2.069855  8
#2   nodrugs  0.9018754 3.492577 27
t.test(expectations~drugsplit, var.equal = TRUE, data=postDf)
# t.test(expectations~drugsplit, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  expectations by drugsplit 
# t = -3.0222, df = 33, p-value = 0.004824
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -6.601924 -1.289486 
# sample estimates:
#   mean in group drugs mean in group nodrugs 
#            -3.0438296             0.9018754 


ggplot(postDf,aes(x=drugsplit, y=tActualAct, color=drugsplit))+ 
		stat_summary(fun.data = "mean_cl_boot") 
#i= 13  var= tActualAct 
#			 Df Sum Sq Mean Sq F value Pr(>F)  
#drugsplit    1   92.9   92.89   6.233 0.0177 *
#Residuals   33  491.8   14.90  
ddply(postDf,.(drugsplit),function(df){
			data.frame(mean = mean(df$tActualAct),
					sd=sd(df$tActualAct),
					n=nrow(df))
		})
#  drugsplit    mean       sd  n
#1     drugs 9.25000 4.334249  8
#2   nodrugs 5.37037 3.722573 27
t.test(tActualAct~drugsplit, var.equal = TRUE, data=postDf)
# t.test(tActualAct~drugsplit, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  tActualAct by drugsplit 
# t = 2.4966, df = 33, p-value = 0.01771
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  0.7180526 7.0412066 
# sample estimates:
#   mean in group drugs mean in group nodrugs 
#               9.25000               5.37037 


ggplot(postDf,aes(x=drugsplit, y=tPerceivedAct, color=drugsplit))+ 
		stat_summary(fun.data = "mean_cl_boot") 
#i= 17  var= tPerceivedAct 
#			 Df Sum Sq Mean Sq F value  Pr(>F)   
#drugsplit    1  170.7  170.70   11.33 0.00195 **
#Residuals   33  497.2   15.07   
ddply(postDf,.(drugsplit),function(df){
			data.frame(mean = mean(df$tPerceivedAct),
					sd=sd(df$tPerceivedAct),
					n=nrow(df))
		})
#  drugsplit     mean       sd  n
#1     drugs 9.000000 5.554921  8
#2   nodrugs 3.740741 3.288589 27
t.test(tPerceivedAct~drugsplit, var.equal = TRUE, data=postDf)
# t.test(tPerceivedAct~drugsplit, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  tPerceivedAct by drugsplit 
# t = 3.366, df = 33, p-value = 0.001949
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  2.080408 8.438111 
# sample estimates:
#   mean in group drugs mean in group nodrugs 
#              9.000000              3.740741 


#i= 1  var= laborLand 
#			 Df Sum Sq Mean Sq F value Pr(>F)
#drugsplit    1   66.1   66.14   0.751  0.392
#Residuals   33 2905.8   88.05   
t.test(laborLand~drugsplit, var.equal = TRUE, data=postDf)
# t.test(laborLand~drugsplit, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  laborLand by drugsplit 
# t = -0.8667, df = 33, p-value = 0.3924
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -10.958637   4.411261 
# sample estimates:
#   mean in group drugs mean in group nodrugs 
#            -2.5254166             0.7482716 
ddply(postDf,.(drugsplit),function(df){
			data.frame(mean = mean(df$laborLand),
					sd=sd(df$laborLand),
					n=nrow(df))
		})
#  drugsplit       mean       sd  n
#1     drugs -2.5254166 9.551201  8
#2   nodrugs  0.7482716 9.338056 27



#########################################################################
## check for any effects of pain medications/hospital birth
#########################################################################
ggplot(postDf,aes(x=P016, y=laborLand, color=P016))+geom_point()
# who is this woman with the worst laborLand score (had epidural)
# low laborLand, intuitMov, emotEnv, painExp, outcomeMeasures, panas
subset(postDf,laborLand==min(laborLand))

ggplot(postDf,aes(x=P016, y=painExp, color=P016))+geom_point()
ggplot(postDf,aes(x=P016, y=outcomeMeasures, color=P016))+geom_point()

ggplot(postDf,aes(x=laborLand, y=painExp, color=P016))+geom_point()
ggplot(postDf,aes(x=drugsplit, y=laborLand))+geom_point()

# find four women who had cesareans
subset(postDf,P015==2)
# find 8 women who used pain medication
subset(postDf,P016!=1)
# R_dnfNFyZENCZUYHa had very negative scores on all measures?
# had vaginal birth w/ epidural; in hospital, but did not intend 
# to be there

# how many hospital births were intended to be hospital births?
# only 5 (of 13) planned hospital births; 4 of these 5 had no drugs;
# of transfers, 4 due to cesarean; 3 more were to receive drugs; 1
# transfered but did not receive drugs (possibly got pitocin)
subset(postDf,P017==3,select=c(P015,P016,P017,P018))             


#########################################################################
## Test for w/ or w/o doula --> no sig. difference
#########################################################################
ggplot(postDf,aes(x=P026, y=laborLand))+geom_point()
ggplot(postDf,aes(x=laborLand, y=outcomeMeasures, color=P026))+geom_point()
aovCheck4 = aov(laborLand~P026,data=postDf)
summary(aovCheck4)
# summary(aovCheck4)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P026         1    171  171.44   1.769  0.193
# Residuals   33   3198   96.91               
ddply(postDf,.(P026),function(df){
			data.frame(mean = mean(df$laborLand),
					se=sqrt(var(df$laborLand)/nrow(df)),
					n=nrow(df))
		})       
#  P026       mean       se  n
#1    1 -2.4342085 3.072763 10
#2    2  0.9736834 1.843536 25

loopvars = c("laborLand","intuitMov","physEnv","emotEnv","fluidReal",
		"intensePres","painExp","expectations","outcomeMeasures",
		"vocals","memory","panas","tActualAct","tDiffAct","tActualPush",
		"tDiffPush","tPerceivedAct","tPerceivedPush")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"P026")]
	names(df)[1]="y"
	aovCheck4b=aov(y~P026,data=df)
	print(summary(aovCheck4b))
}
t.test(laborLand~P026, var.equal = TRUE, data=postDf)
# t.test(laborLand~P026, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  laborLand by P026 
# t = -0.9734, df = 33, p-value = 0.3374
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -10.530476   3.714692 
# sample estimates:
# mean in group 1 mean in group 2 
#      -2.4342085       0.9736834 
ddply(postDf,.(P026),function(df){
			data.frame(mean = mean(df$laborLand),
					sd=sd(df$laborLand),
					n=nrow(df))
		})
#  P026       mean       sd  n
#1    1 -2.4342085 9.716929 10
#2    2  0.9736834 9.217681 25

t.test(outcomeMeasures~P026, var.equal = TRUE, data=postDf)
# t.test(outcomeMeasures~P026, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by P026 
# t = -1.5012, df = 33, p-value = 0.1428
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -5.696622  0.859211 
# sample estimates:
# mean in group 1 mean in group 2 
#      -1.7276469       0.6910588 
 ddply(postDf,.(P026),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures),
					n=nrow(df))
		})
#  P026       mean       sd  n
#1    1 -1.7276469 3.881197 10
#2    2  0.6910588 4.454855 25

t.test(painExp~P026, var.equal = TRUE, data=postDf)
# t.test(painExp~P026, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  painExp by P026 
# t = -0.3715, df = 33, p-value = 0.7126
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -5.236224  3.619054 
# sample estimates:
# mean in group 1 mean in group 2 
#      -0.5775608       0.2310243 
ddply(postDf,.(P026),function(df){
			data.frame(mean = mean(df$painExp),
					sd=sd(df$painExp),
					n=nrow(df))
		})
#  P026       mean       sd  n
#1    1 -0.5775608 6.595502 10
#2    2  0.2310243 5.495698 25

#########################################################################
## Test for w/ or w/o birth plan --> no sig. difference
#########################################################################
ggplot(postDf,aes(x=P027, y=laborLand))+geom_point()
ggplot(postDf,aes(x=laborLand, y=outcomeMeasures, color=P027))+geom_point()
aovCheck5 = aov(laborLand~P027,data=postDf)
summary(aovCheck5)
# summary(aovCheck5)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P027         1      0     0.0       0      1
# Residuals   33   3370   102.1               
 



#########################################################################
## check for any effects of postpartum length
#########################################################################
postDf$P002
# 5/4/11, 7/16/12???, 1/3/11, 5/11/11, 6/26/11, 9/23/10
# 18, 19, 20*, 22, 25, 30*

# add a variable called ppInterval

ppInterval = c(5,15,20,22,23,16,54,116,23,41,94,14,14,122,47,69,275,
		203,397,87,270,144,118,226,15,62,69,52,510,21,15,27,21,13,160)
length(ppInterval)
postDf$ppInterval = ppInterval

# split participants into two groups (shortpp and longpp)
# short pp = anything under 124 days (4 months)

## split women into short and long groups
pIindex = postDf$ppInterval>=124
# selects TRUE values
postDf$ppInterval[pIindex]
postDf$ppIntervalsplit = "shortinterval"
postDf$ppIntervalsplit[pIindex] = "longinterval"
postDf$ppIntervalsplit = factor(postDf$ppIntervalsplit,levels = c("shortinterval","longinterval"))
# 8 women fall into longinterval
ggplot(postDf,aes(x=ppIntervalsplit))+geom_histogram()

# check for any differences in short v. long pp interval
loopvars = c("laborLand","outcomeMeasures","painExp","memory",
		"fluidReal","intensePres","physEnv","emotEnv","expectations",
		"intuitMov","panas","tPerceivedAct","tPerceivedPush","vocals")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"ppIntervalsplit")]
	names(df)[1]="y"
	aov1=aov(y~ppIntervalsplit,data=df)
	print(summary(aov1))
}


ggplot(postDf,aes(x=ppIntervalsplit, y=outcomeMeasures))+geom_boxplot()
# t-test
t.test(outcomeMeasures~ppIntervalsplit, var.equal = TRUE, data=postDf)
# t.test(outcomeMeasures~ppIntervalsplit, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by ppIntervalsplit 
# t = -1.5793, df = 33, p-value = 0.1238
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -6.2427200  0.7862845 
# sample estimates:
# mean in group shortinterval  mean in group longinterval 
#                  -0.6235926                   2.1046251 

ddply(postDf,.(ppIntervalsplit),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					se=sqrt(var(df$outcomeMeasures)/nrow(df)))
		})
#ppIntervalsplit       mean        se
#1   shortinterval -0.6235926 0.8547341
#2    longinterval  2.1046251 1.3015248

ggplot(postDf,aes(x=ppIntervalsplit, y=laborLand))+geom_boxplot()
t.test(laborLand~ppIntervalsplit, var.equal = TRUE, data=postDf)
# t.test(laborLand~ppIntervalsplit, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  laborLand by ppIntervalsplit 
# t = -2.3674, df = 33, p-value = 0.02393
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -15.547014  -1.175732 
# sample estimates:
# mean in group shortinterval  mean in group longinterval 
#                   -1.911171                    6.450202 
ddply(postDf,.(ppIntervalsplit),function(df){
			data.frame(mean = mean(df$laborLand),
					sd=sd(df$laborLand))
		})
#  ppIntervalsplit      mean       sd
#1   shortinterval -1.911171 8.978196
#2    longinterval  6.450202 7.969862





ggplot(postDf,aes(x=ppIntervalsplit, y=painExp))+geom_boxplot()
ggplot(postDf,aes(x=ppIntervalsplit, y=expectations))+geom_boxplot()







#########################################################################
## find the respondent with the worst outcome
#########################################################################
# this graph plots om v ll with P015 as color (red color = cesarean)
ggplot(postDf,aes(x=laborLand, y=outcomeMeasures, color=P015))+geom_point()
# pulls out the responses for this worst outcomes woman
subset(postDf,outcomeMeasures==min(outcomeMeasures))
# finding that women on this graph, she also has almost worst painExp
ggplot(postDf,aes(x=painExp, y=outcomeMeasures, color=P015))+geom_point()
ggplot(postDf,aes(x=laborLand, y=painExp, color=P015))+geom_point()

# R_elY2IJEJ215KY16:
# laborLand intuitMov
# -6.994437 -8.646781
#    physEnv   emotEnv fluidReal intensePres   painExp expectations    memory
# -0.9921606 -10.26134 -1.597448   -8.046077 -11.13847    -6.180475 -3.174058
#    vocals outcomeMeasures  stateQs     panas tActualAct tPerceivedAct
# -2.295789       -11.93492 -45.2912 -12.73046          9             7
# tDiffAct tActualPush tPerceivedPush tDiffPush mslaborLand lmhpanas drugsplit
#        2          30             50       -20         low      low   nodrugs
# mspainExp msexpectations primipsplit agesplit
#       low            low      multip    older
