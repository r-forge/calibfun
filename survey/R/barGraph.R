###############################################################################
## barGraph.R
##
## TODO: Add comment
## 
## Author: Haaland
###############################################################################
load("rdata/preData.RData")
load("rdata/postData.RData")
library(ggplot2)
library(psych)
library(GPArotation)
library(Hmisc)

#########################################################################
## analysis of variance based on categories of mslaborLand
#########################################################################
# IV: mslaborLand; DV: outcomeMeasures
t1 = t.test(outcomeMeasures~mslaborLand, var.equal = TRUE, data=postDf)
names(t1)
pvals = c(mslaborLand=t1$p.value)
# t.test(outcomeMeasures~mslaborLand, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by mslaborLand 
# t = -3.9454, df = 33, p-value = 0.0003928
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -7.419515 -2.370887 
# sample estimates:
#  mean in group low mean in group high 
#          -2.517532           2.377669 
df1 = ddply(postDf,.(mslaborLand),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures))
		})
#  mslaborLand      mean       sd
#1         low -2.517532 4.336100
#2        high  2.377669 2.903591
df1
names(df1)[1]="Levels"
df1$variable="Laborland"
df1
bcdf = df1

t1 = t.test(outcomeMeasures~mspainExp, var.equal = TRUE, data=postDf)
pvals = c(pvals,mspainExp=t1$p.value)
pvals
# t.test(outcomeMeasures~mspainExp, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by mspainExp 
# t = -3.7048, df = 33, p-value = 0.0007712
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -7.259911 -2.112815 
# sample estimates:
#  mean in group low mean in group high 
#          -2.410130           2.276234 
df1 = ddply(postDf,.(mspainExp),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures))
		})
#  mspainExp      mean       sd
#1       low -2.410130 4.651492
#2      high  2.276234 2.606157
names(df1)[1]="Levels"
df1$variable="Pain\nMgt"
df1
bcdf = rbind(bcdf,df1)
bcdf

t1 = t.test(outcomeMeasures~msintuitMov, var.equal = TRUE, data=postDf)
pvals = c(pvals,msintuitMov=t1$p.value)
# t.test(outcomeMeasures~msintuitMov, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by msintuitMov 
# t = -3.4398, df = 33, p-value = 0.001597
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -7.069396 -1.814733 
# sample estimates:
#  mean in group low mean in group high 
#          -2.284490           2.157574 
df1 = ddply(postDf,.(msintuitMov),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures))
		})
#  msintuitMov      mean       sd
#1         low -2.284490 4.441860
#2        high  2.157574 3.119787
names(df1)[1]="Levels"
df1$variable="Intuitive\nMovt"
df1
bcdf = rbind(bcdf,df1)
bcdf


t1 = t.test(outcomeMeasures~msphysEnv, var.equal = TRUE, data=postDf)
pvals = c(pvals,msphysEnv=t1$p.value)
# t.test(outcomeMeasures~msphysEnv, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by msphysEnv 
# t = -2.8455, df = 33, p-value = 0.007563
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -6.582127 -1.093868 
# sample estimates:
#  mean in group low mean in group high 
#          -1.973827           1.864170 
df1 = ddply(postDf,.(msphysEnv),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures))
		})
#  msphysEnv      mean       sd
#1       low -1.973827 4.761681
#2      high  1.864170 3.087885
names(df1)[1]="Levels"
df1$variable="Physical\nEnv"
df1
bcdf = rbind(bcdf,df1)
bcdf

t1 = t.test(outcomeMeasures~msemotEnv, var.equal = TRUE, data=postDf)
pvals = c(pvals,msemotEnv=t1$p.value)
# t.test(outcomeMeasures~msemotEnv, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by msemotEnv 
# t = -3.6857, df = 33, p-value = 0.0008132
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -7.246692 -2.091795 
# sample estimates:
#  mean in group low mean in group high 
#          -2.401325           2.267918 
df1 = ddply(postDf,.(msemotEnv),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures))
		})
#  msemotEnv      mean       sd
#1       low -2.401325 4.799852
#2      high  2.267918 2.356842
names(df1)[1]="Levels"
df1$variable="Emotional\nEnv"
df1
bcdf = rbind(bcdf,df1)
bcdf


t1 = t.test(outcomeMeasures~msfluidReal, var.equal = TRUE, data=postDf)
pvals = c(pvals,msfluidReal=t1$p.value)
# t.test(outcomeMeasures~msfluidReal, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by msfluidReal 
# t = -2.9927, df = 33, p-value = 0.005204
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -6.710851 -1.279084 
# sample estimates:
#  mean in group low mean in group high 
#          -2.054555           1.940413 
df1 = ddply(postDf,.(msfluidReal),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures))
		})
#  msfluidReal      mean       sd
#1         low -2.054555 4.556663
#2        high  1.940413 3.271200
names(df1)[1]="Levels"
df1$variable="Fluid\nReality"
df1
bcdf = rbind(bcdf,df1)
bcdf

t1 = t.test(outcomeMeasures~msintensePres, var.equal = TRUE, data=postDf)
pvals = c(pvals,msintensePres=t1$p.value)
# t.test(outcomeMeasures~msintensePres, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by msintensePres 
# t = -2.4543, df = 33, p-value = 0.01956
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -6.213246 -0.581085 
# sample estimates:
#  mean in group low mean in group high 
#          -1.747114           1.650052 
df1 = ddply(postDf,.(msintensePres),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures))
		})
#  msintensePres      mean       sd
#1           low -1.747114 4.732214
#2          high  1.650052 3.382108
names(df1)[1]="Levels"
df1$variable="Intense\nPresence"
df1
bcdf = rbind(bcdf,df1)
bcdf

t1 = t.test(outcomeMeasures~msexpectations, var.equal = TRUE, data=postDf)
pvals = c(pvals,msexpectations=t1$p.value)
# t.test(outcomeMeasures~msexpectations, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by msexpectations 
# t = -2.5057, df = 33, p-value = 0.01733
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -6.2639742 -0.6501255 
# sample estimates:
#  mean in group low mean in group high 
#          -1.777911           1.679138 
df1 = ddply(postDf,.(msexpectations),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures))
		})
#  msexpectations      mean       sd
#1            low -1.777911 4.906272
#2           high  1.679138 3.106217
names(df1)[1]="Levels"
df1$variable="Meeting\nExpectns"
df1
bcdf = rbind(bcdf,df1)
bcdf


#########################################################################
## not going to graph these
#########################################################################

t1 = t.test(outcomeMeasures~vocalsplit, var.equal = TRUE, data=postDf)
pvals = c(pvals,vocalsplit=t1$p.value)
# t.test(outcomeMeasures~vocalsplit, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by vocalsplit 
# t = -0.9753, df = 33, p-value = 0.3365
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -4.481067  1.577027 
# sample estimates:
#  mean in group low mean in group high 
#         -0.7882394          0.6637805 
ddply(postDf,.(vocalsplit),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures))
		})
#  vocalsplit       mean       sd
#1        low -0.7882394 4.842105
#2       high  0.6637805 3.969714


t1 = t.test(outcomeMeasures~msmemory, var.equal = TRUE, data=postDf)
pvals = c(pvals,msmemory=t1$p.value)
# t.test(outcomeMeasures~msmemory, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by msmemory 
# t = -0.5491, df = 33, p-value = 0.5866
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -3.871223  2.225652 
# sample estimates:
#  mean in group low mean in group high 
#         -0.4231466          0.3996385 
ddply(postDf,.(msmemory),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures))
		})
#  msmemory       mean       sd
#1      low -0.4231466 4.902845
#2     high  0.3996385 3.934259

t1 = t.test(outcomeMeasures~mspanas, var.equal = TRUE, data=postDf)
pvals = c(pvals,mspanas=t1$p.value)
# t.test(outcomeMeasures~mspanas, var.equal = TRUE, data=postDf)
# 
# 	Two Sample t-test
# 
# data:  outcomeMeasures by mspanas 
# t = -1.835, df = 33, p-value = 0.07553
# alternative hypothesis: true difference in means is not equal to 0 
# 95 percent confidence interval:
#  -5.548216  0.286014 
# sample estimates:
#  mean in group low mean in group high 
#          -1.353138           1.277963 
ddply(postDf,.(mspanas),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					sd=sd(df$outcomeMeasures))
		})
#  mspanas      mean       sd
#1     low -1.353138 4.780681
#2    high  1.277963 3.657828

p.adjust(pvals, method="holm")
# p.adjust(pvals, method="holm")
#    mslaborLand      mspainExp    msintuitMov      msphysEnv      msemotEnv 
#    0.004320657    0.007711896    0.012773037    0.045376070    0.007711896 
#    msfluidReal  msintensePres msexpectations     vocalsplit       msmemory 
#    0.036426561    0.086650151    0.086650151    0.673037719    0.673037719 
#        mspanas 
#    0.226583482 

bcdflow = bcdf
bcdflow$mean = bcdflow$mean-2*bcdflow$sd/sqrt(17.5)
bcdfhigh = bcdf
bcdfhigh$mean = bcdfhigh$mean+2*bcdfhigh$sd/sqrt(17.5)
bcdflimbs = rbind(bcdflow,bcdfhigh)


bcdf$variable = factor(bcdf$variable,
		levels=unique(bcdf$variable)[c(1,2,5,3,6,4,8,7)])
pdf=data.frame(x=1.5,y=1.5,variable=levels(bcdf$variable),
		text=c("**","**","**",rep("*",3),rep(".",2)))

pdf("plots/Figure1.pdf")
ggplot(data=bcdf,aes(x=Levels,y=mean))+geom_point()+
		geom_line(aes(x=as.numeric(Levels),y=mean))+
		geom_line(aes(x=Levels,y=mean),data=bcdflimbs)+
		geom_text(aes(x=x,y=y,label=text),data=pdf)+
		facet_grid(~variable)+
		opts(strip.text.x=theme_text(size=6))+
		theme_bw()+
		xlab("")+ylab("Outcome Measures")
dev.off()

