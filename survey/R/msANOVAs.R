###############################################################################
## msANOVAs.R
##
## testing relationships between variables using median splits
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
aov1 = aov(outcomeMeasures~mslaborLand,data=postDf)
summary(aov1)
# summary(aov1)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# mslaborLand  1  209.5  209.51   15.57 0.000393 ***
# Residuals   33  444.2   13.46                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# this code gives me the mean and std for my anovas!
ddply(postDf,.(mslaborLand),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					se=sqrt(var(df$outcomeMeasures)/nrow(df)))
		})
#mslaborLand      mean        se
#1         low -2.517532 1.0516588
#2        high  2.377669 0.6843831

# this plot isn't very helpful - don't need to do it
# plot(aov1)

# this code block plots om v. ll with error bars [CI] added
g1=ggplot(postDf,aes(x=mslaborLand, y=outcomeMeasures))
stat_sum_df <- function(fun, geom="crossbar", ...) { 
	stat_summary(fun.data=fun, colour="red", geom=geom, width=0.2, ...) 
}
# means plus 95% confidence interval
g1 + stat_sum_df("mean_cl_normal", geom = "errorbar") + 
		stat_sum_df("mean_cl_normal", geom = "point") 


#########################################################################
## 2x3 analysis of variance: outcomeMeasures~mslaborLand*lmhpanas
#########################################################################
aov2 = aov(outcomeMeasures~lmhpanas*mslaborLand,data=postDf)
summary(aov2)
# summary(aov2)
#                      Df Sum Sq Mean Sq F value  Pr(>F)    
# mslaborLand           1  209.5  209.51  16.663 0.00032 ***
# lmhpanas              2   25.8   12.88   1.024 0.37170    
# mslaborLand:lmhpanas  2   53.8   26.89   2.139 0.13598    
# Residuals            29  364.6   12.57                    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#old results (including P049 and P069)
# summary(aov2)
#                      Df Sum Sq Mean Sq F value   Pr(>F)    
# mslaborLand           1 228.55  228.55  21.472 7.01e-05 ***
# lmhpanas              2  45.10   22.55   2.119   0.1384    
# mslaborLand:lmhpanas  2  71.31   35.66   3.350   0.0491 *  
# Residuals            29 308.69   10.64                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
ddply(postDf,.(mslaborLand,lmhpanas),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					se=sqrt(var(df$outcomeMeasures)/nrow(df)),
					n=nrow(df))
		})

# try it with mspanas instead of lmhpanas
ggplot(postDf,aes(x=mspanas, y=outcomeMeasures, color=mslaborLand))+ 
		stat_summary(fun.data = "mean_cl_boot") 
aov2 = aov(outcomeMeasures~mspanas*mslaborLand,data=postDf)
summary(aov2)
#try the other order
aov2 = aov(outcomeMeasures~mslaborLand*mspanas,data=postDf)
summary(aov2)


## this plot isn't very helpful - don't need to do it each time
plot(aov2)

# haven't resaved this after tweaking the ll score (want to save for
# comparison) - 3/2/12
pdf("plots/2x3anova.pdf")
ggplot(postDf,aes(x=lmhpanas, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot") 
dev.off()

#########################################################################
## analysis of variance: effect of panas on different themes
## redid with mspanas on 3/11/12
#########################################################################
ggplot(postDf,aes(x=mspanas, y=laborLand)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov3 = aov(laborLand~mspanas,data=postDf)
summary(aov3)
# summary(aov3)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# mspanas      1  238.2  238.22   2.876 0.0993 .
# Residuals   33 2733.7   82.84                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#########################################################################
ggplot(postDf,aes(x=mspanas, y=outcomeMeasures)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov10 = aov(outcomeMeasures~mspanas,data=postDf)
summary(aov10)
# summary(aov10)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# mspanas      1   60.5   60.52   3.367 0.0755 .
# Residuals   33  593.1   17.97                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#########################################################################
ggplot(postDf,aes(x=mspanas, y=painExp)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov11 = aov(painExp~mspanas,data=postDf)
summary(aov11)
# summary(aov11)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# mspanas      1  123.2  123.22   4.075 0.0517 .
# Residuals   33  997.8   30.24                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1         

#########################################################################
ggplot(postDf,aes(x=mspanas, y=emotEnv)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov12 = aov(emotEnv~mspanas,data=postDf)
summary(aov12)
# summary(aov12)
#             Df Sum Sq Mean Sq F value Pr(>F)
# mspanas      1   83.9   83.94   2.105  0.156
# Residuals   33 1316.0   39.88               

#########################################################################
ggplot(postDf,aes(x=mspanas, y=expectations)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov13 = aov(expectations~mspanas,data=postDf)
summary(aov13)
# summary(aov13)
#             Df Sum Sq Mean Sq F value Pr(>F)   
# mspanas      1  41.67   41.67   8.239 0.0071 **
# Residuals   33 166.88    5.06                  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#########################################################################
ggplot(postDf,aes(x=mspanas, y=intuitMov)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov14 = aov(intuitMov~mspanas,data=postDf)
summary(aov14)
# summary(aov14)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
# mspanas      1  89.33   89.33   11.36 0.00192 **
# Residuals   33 259.41    7.86                   
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  

#########################################################################
## looking at other relationships
#########################################################################
# shows that painExp is much higher for high ll women
ggplot(postDf,aes(x=mslaborLand, y=painExp)) + 
		stat_summary(fun.data = "mean_cl_boot") 
# show that this is a significant difference
aov4 = aov(painExp~mslaborLand,data=postDf)
summary(aov4)
# summary(aov4)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
# mslaborLand  1  238.5  238.46   8.916 0.00529 **
# Residuals   33  882.6   26.74                   
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1              

#########################################################################
ggplot(postDf,aes(x=drugsplit, y=laborLand)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov15 = aov(laborLand~drugsplit,data=postDf)
summary(aov15)
# summary(aov15)
#             Df Sum Sq Mean Sq F value Pr(>F)
# drugsplit    1   66.1   66.14   0.751  0.392
# Residuals   33 2905.8   88.05               

# this shows that high ll is clustered in the corner of best
# painExp and best outcomeMeasures; but low ll is spread all 
# along the axis (but still w/ a correlation)
ggplot(postDf,aes(x=painExp, y=outcomeMeasures, color=mslaborLand))+geom_point()
ggplot(postDf,aes(x=mspainExp, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")+facet_wrap(~mspanas)
# it matters what order the terms come in (when the design is unbalanced)
aov5 = aov(outcomeMeasures~mspainExp*mslaborLand,data=postDf)
summary(aov5)
# summary(aov5)
#                       Df Sum Sq Mean Sq F value   Pr(>F)    
# mspainExp              1  192.0  192.01  18.661 0.000149 ***
# mslaborLand            1  113.8  113.75  11.055 0.002282 ** 
# mspainExp:mslaborLand  1   28.9   28.92   2.811 0.103698    
# Residuals             31  319.0   10.29                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# if you look at anovas within each of the levels of pain, the 
# differences are significent --> led us to analysis of subgroups 
# comparisons
ddply(postDf,.(mspainExp),function(df){
			aov5b = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aov5b))
			coef(aov5b)
		})
#Df Sum Sq Mean Sq F value  Pr(>F)   
#mslaborLand  1  128.1  128.05   8.806 0.00959 **
#		Residuals   15  218.1   14.54                   
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)
#mslaborLand  1  14.62  14.620    2.32  0.147
#Residuals   16 100.85   6.303               
#mspainExp (Intercept) mslaborLandhigh
#1       low   -4.437116        5.743128
#2      high    1.001706        1.911792

ggplot(postDf,aes(x=mspanas, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov8 = aov(painExp~mslaborLand*mspanas,data=postDf)
summary(aov8)
# summary(aov8)
#                     Df Sum Sq Mean Sq F value  Pr(>F)   
# mslaborLand          1  238.5  238.46   9.207 0.00485 **
# mspanas              1   43.4   43.41   1.676 0.20504   
# mslaborLand:mspanas  1   36.2   36.23   1.399 0.24590   
# Residuals           31  802.9   25.90                   
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
ddply(postDf,.(mspanas),function(df){
			aov8b = aov(painExp~mslaborLand,data=df)
			print(summary(aov8b))
			coef(aov8b)
		})
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  172.3  172.34   7.732  0.014 *
#		Residuals   15  334.3   22.29                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)
#mslaborLand  1   22.5   22.55    0.77  0.393
#Residuals   16  468.6   29.29               
#mspanas (Intercept) mslaborLandhigh
#1     low  -4.2821968        6.662611
#2    high   0.2406909        2.374104



#########################################################################
## does laborLand mediate the expectations->outcomeMeasures relationship?
#########################################################################
ggplot(postDf,aes(x=msexpectations, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov6 = aov(outcomeMeasures~msexpectations*mslaborLand,data=postDf)
summary(aov6)
# summary(aov6)
#                            Df Sum Sq Mean Sq F value   Pr(>F)    
# msexpectations              1   93.0   92.95   8.178 0.007520 ** 
# mslaborLand                 1  187.8  187.82  16.525 0.000305 ***
# msexpectations:mslaborLand  1   20.6   20.55   1.808 0.188471    
# Residuals                  31  352.3   11.37                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
ddply(postDf,.(msexpectations),function(df){
			aov6b = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aov6b))
			coef(aov6b)
		})
#Df Sum Sq Mean Sq F value Pr(>F)   
#mslaborLand  1  164.3  164.28    9.79 0.0069 **
#		Residuals   15  251.7   16.78                  
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  44.09   44.09   7.011 0.0175 *
#		Residuals   16 100.62    6.29                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#msexpectations (Intercept) mslaborLandhigh
#1            low  -4.6077566        6.228053
#2           high  -0.1660292        3.149596

## don't get the same relationship observed elsewhere (that in high 
## group, ll doesn't matter, but it matters a lot in low group)
## thus, the expectations/outcomeMeasures relationship is not really
## mediated by laborLand

#what about expectations->painExp?
ggplot(postDf,aes(x=msexpectations, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov7 = aov(painExp~msexpectations*mslaborLand,data=postDf)
summary(aov7)
# summary(aov7)
#                            Df Sum Sq Mean Sq F value   Pr(>F)    
# msexpectations              1  275.3  275.30  14.005 0.000743 ***
# mslaborLand                 1  198.3  198.34  10.090 0.003364 ** 
# msexpectations:mslaborLand  1   38.0   38.05   1.936 0.174054    
# Residuals                  31  609.4   19.66                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
ddply(postDf,.(msexpectations),function(df){
			aov7b = aov(painExp~mslaborLand,data=df)
			print(summary(aov7b))
			coef(aov7b)
		})
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  203.1  203.11   7.099 0.0177 *
#		Residuals   15  429.2   28.61                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)
#mslaborLand  1  33.28   33.28   2.956  0.105
#Residuals   16 180.17   11.26               
#msexpectations (Intercept) mslaborLandhigh
#1            low   -6.144710        6.925012
#2           high    1.205296        2.736464


#########################################################################
## look at outcomeMeasures~mslaborLand as each individual question
#########################################################################
# outcomeMeasures = P065,P066,P122,P149,P150,P151

loopvars = c("P065","P066","P122","P149","P150","P151")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"mslaborLand")]
	names(df)[1]="y"
	aov9=aov(y~mslaborLand,data=df)
	print(summary(aov9))
}
# i= 1  var= P065 
# Df Sum Sq Mean Sq F value Pr(>F)  
# mslaborLand  1   5.70   5.696   5.903 0.0207 *
# 		Residuals   33  31.85   0.965                 
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# i= 2  var= P066 
# Df Sum Sq Mean Sq F value Pr(>F)
# mslaborLand  1  2.243  2.2432    2.64  0.114
# Residuals   33 28.042  0.8498               
# i= 3  var= P122 
# Df Sum Sq Mean Sq F value Pr(>F)  
# mslaborLand  1  2.073  2.0729   3.309  0.078 .
# Residuals   33 20.670  0.6264                 
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# i= 4  var= P149 
# Df Sum Sq Mean Sq F value  Pr(>F)   
# mslaborLand  1  10.48  10.479   12.44 0.00126 **
# 		Residuals   33  27.81   0.843                   
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# i= 5  var= P150 
# Df Sum Sq Mean Sq F value Pr(>F)   
# mslaborLand  1  20.02  20.016   13.04  0.001 **
# 		Residuals   33  50.67   1.535                  
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# i= 6  var= P151 
# Df Sum Sq Mean Sq F value  Pr(>F)   
# mslaborLand  1  6.959   6.959   9.026 0.00505 **
# 		Residuals   33 25.441   0.771                   
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

## what might better predict P066(I felt in control of the care I 
## received) and P122(I was involved in all decision-making 
## processes (especially regarding interventions))


#########################################################################
## check multip v. primip against factors
ggplot(postDf,aes(x=primipsplit, y=outcomeMeasures)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov16 = aov(outcomeMeasures~primipsplit,data=postDf)
summary(aov16)
# summary(aov16)
#             Df Sum Sq Mean Sq F value Pr(>F)
# primipsplit  1    0.6   0.628   0.032   0.86
# Residuals   33  653.0  19.789               

ggplot(postDf,aes(x=primipsplit, y=laborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov17 = aov(laborLand~primipsplit,data=postDf)
summary(aov17)
# summary(aov17)
#             Df Sum Sq Mean Sq F value Pr(>F)
# primipsplit  1   50.4   50.43    0.57  0.456
# Residuals   33 2921.5   88.53       

ggplot(postDf,aes(x=primipsplit, y=vocals)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov22 = aov(vocals~primipsplit,data=postDf)
summary(aov22)
# summary(aov22)
#             Df Sum Sq Mean Sq F value Pr(>F)
# primipsplit  1   2.17   2.167   0.516  0.478
# Residuals   33 138.66   4.202               


#########################################################################
## check age v. factors
ggplot(postDf,aes(x=agesplit, y=laborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov18 = aov(laborLand~agesplit,data=postDf)
summary(aov18)
# summary(aov18)
#             Df Sum Sq Mean Sq F value Pr(>F)
# agesplit     1   41.7   41.74    0.47  0.498
# Residuals   33 2930.2   88.79               

ggplot(postDf,aes(x=agesplit, y=outcomeMeasures)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov19 = aov(outcomeMeasures~agesplit,data=postDf)
summary(aov19)
# summary(aov19)
#             Df Sum Sq Mean Sq F value Pr(>F)
# agesplit     1   11.0   11.04   0.567  0.457
# Residuals   33  642.6   19.47               

ggplot(postDf,aes(x=mslaborLand, y=outcomeMeasures, color=agesplit)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov20 = aov(outcomeMeasures~mslaborLand*agesplit,data=postDf)
summary(aov20)
# summary(aov20)
#                      Df Sum Sq Mean Sq F value   Pr(>F)    
# mslaborLand           1  209.5  209.51  16.161 0.000345 ***
# agesplit              1   40.1   40.13   3.096 0.088375 .  
# mslaborLand:agesplit  1    2.1    2.15   0.166 0.686833    
# Residuals            31  401.9   12.96                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

ggplot(postDf,aes(x=agesplit, y=vocals)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov21 = aov(vocals~agesplit,data=postDf)
summary(aov21)
# summary(aov21)
#             Df Sum Sq Mean Sq F value Pr(>F)
# agesplit     1   0.47   0.468    0.11  0.742
# Residuals   33 140.36   4.253               

ggplot(postDf,aes(x=agesplit, y=tActualPush)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov23 = aov(tActualPush~agesplit,data=postDf)
summary(aov23)
# summary(aov23)
#             Df Sum Sq Mean Sq F value Pr(>F)
# agesplit     1   3474    3474   1.392  0.246
# Residuals   33  82354    2496               

ggplot(postDf,aes(x=agesplit, y=tActualAct)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov24 = aov(tActualAct~agesplit,data=postDf)
summary(aov24)
# summary(aov24)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# agesplit     1   69.4   69.38   4.443 0.0427 *
# Residuals   33  515.3   15.62                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

ggplot(postDf,aes(x=primipsplit, y=tActualAct)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov25 = aov(tActualAct~primipsplit,data=postDf)
summary(aov25)
# summary(aov25)
#             Df Sum Sq Mean Sq F value Pr(>F)
# primipsplit  1   22.2   22.20   1.302  0.262
# Residuals   33  562.5   17.05               

ggplot(postDf,aes(x=primipsplit, y=tActualAct, color=agesplit)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov26 = aov(tActualAct~agesplit*primipsplit,data=postDf)
summary(aov26)
# summary(aov26)
#                      Df Sum Sq Mean Sq F value Pr(>F)  
# agesplit              1   69.4   69.38   4.438 0.0433 *
# primipsplit           1    7.1    7.13   0.456 0.5044  
# agesplit:primipsplit  1   23.6   23.59   1.509 0.2286  
# Residuals            31  484.6   15.63                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
ddply(postDf,.(primipsplit),function(df){
			aov26b = aov(tActualAct~agesplit,data=df)
			print(summary(aov26b))
			coef(aov26b)
		})
#Df Sum Sq Mean Sq F value Pr(>F)
#agesplit     1   2.02   2.017   0.103  0.753
#Residuals   14 273.73  19.552               
#Df Sum Sq Mean Sq F value Pr(>F)  
#agesplit     1  75.88   75.88   6.118 0.0242 *
#		Residuals   17 210.86   12.40                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#primipsplit (Intercept) agesplitolder
#1      primip    7.400000    -0.7333333
#2      multip    8.142857    -4.1428571


ggplot(postDf,aes(x=primipsplit, y=outcomeMeasures, color=agesplit)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(primipsplit),function(df){
			aov26x = aov(outcomeMeasures~agesplit,data=df)
			print(summary(aov26x))
			coef(aov26x)
		})
# no significance (good)

#########################################################################
# look at the role of vocalizing
ggplot(postDf,aes(x=mslaborLand, y=vocals)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov27 = aov(vocals~mslaborLand,data=postDf)
summary(aov27)
# summary(aov27)
#             Df Sum Sq Mean Sq F value Pr(>F)
# mslaborLand  1   5.93   5.928    1.45  0.237
# Residuals   33 134.90   4.088               

ggplot(postDf,aes(x=vocalsplit, y=outcomeMeasures)) + 
		stat_summary(fun.data = "mean_cl_boot")
loopvars = c("laborLand","outcomeMeasures","painExp","memory",
		"fluidReal","intensePres","physEnv","emotEnv","expectations")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"vocalsplit")]
	names(df)[1]="y"
	aov28=aov(y~vocalsplit,data=df)
	print(summary(aov28))
}
# none are significant (or even come close)

#########################################################################
# look at location of birth,P017,(interaction w/ planned location?P018)

ggplot(postDf,aes(x=P017, y=laborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov29 = aov(laborLand~P017,data=postDf)
summary(aov29)
# summary(aov29)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P017         1  197.4  197.45   2.348  0.135
# Residuals   33 2774.5   84.07               

ggplot(postDf,aes(x=P017, y=outcomeMeasures)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov30 = aov(outcomeMeasures~P017,data=postDf)
summary(aov30)
# summary(aov30)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P017         1   23.7   23.66   1.239  0.274
# Residuals   33  630.0   19.09               


ggplot(postDf,aes(x=P017, y=outcomeMeasures)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov31 = aov(outcomeMeasures~P017*P018,data=postDf)
summary(aov31)
# summary(aov31)
#             Df Sum Sq Mean Sq F value Pr(>F)
# P017         1   23.7   23.66   1.226  0.276
# P018         1   12.7   12.66   0.656  0.424
# Residuals   32  617.3   19.29               
## ????????????????

loopvars = c("laborLand","outcomeMeasures","painExp","memory","intuitMov",
		"fluidReal","intensePres","physEnv","emotEnv","expectations")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"P017")]
	names(df)[1]="y"
	aov32=aov(y~P017,data=df)
	print(summary(aov32))
}
ggplot(postDf,aes(x=P017, y=physEnv)) + 
		stat_summary(fun.data = "mean_cl_boot")
# i= 8  var= physEnv 
# Df Sum Sq Mean Sq F value Pr(>F)  
# P017         1  61.51   61.51   7.318 0.0107 *
# 		Residuals   33 277.39    8.41                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
ggplot(postDf,aes(x=P017, y=expectations)) + 
		stat_summary(fun.data = "mean_cl_boot")
ggplot(postDf,aes(x=msexpectations, y=P017)) + 
		stat_summary(fun.data = "mean_cl_boot")
# i= 10  var= expectations 
# Df Sum Sq Mean Sq F value  Pr(>F)   
# P017         1  50.81   50.81   10.63 0.00259 **
# 		Residuals   33 157.74    4.78                   
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

ggplot(postDf,aes(x=P018, y=laborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ggplot(postDf,aes(x=P018, y=outcomeMeasures)) + 
		stat_summary(fun.data = "mean_cl_boot")
loopvars = c("laborLand","outcomeMeasures","painExp","memory","intuitMov",
		"fluidReal","intensePres","physEnv","emotEnv","expectations")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"P018")]
	names(df)[1]="y"
	aov33=aov(y~P018,data=df)
	print(summary(aov33))
}
ggplot(postDf,aes(x=P018, y=physEnv)) + 
		stat_summary(fun.data = "mean_cl_boot")
# i= 8  var= physEnv 
# Df Sum Sq Mean Sq F value  Pr(>F)   
# P018         1   65.8   65.80   7.951 0.00807 **
# 		Residuals   33  273.1    8.28                   
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
ggplot(postDf,aes(x=P018, y=expectations)) + 
		stat_summary(fun.data = "mean_cl_boot")
ggplot(postDf,aes(x=msexpectations, y=P018)) + 
		stat_summary(fun.data = "mean_cl_boot")
# i= 10  var= expectations 
# Df Sum Sq Mean Sq F value  Pr(>F)    
# P018         1  59.59   59.59    13.2 0.00094 ***
# 		Residuals   33 148.96    4.51                    
# ---
# 		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

ggplot(postDf,aes(x=msexpectations, y=P018)) + 
		stat_summary(fun.data = "mean_cl_boot")

#########################################################################
## testing center: looking for more plots that carry the signature trend
## for outcomeMeasures~variable*mslaborLand
#########################################################################
ggplot(postDf,aes(x=msexpectations,y=outcomeMeasures,color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(msexpectations),function(df){
			aovtest1 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest1))
			coef(aovtest1)
		})
#Df Sum Sq Mean Sq F value Pr(>F)   
#mslaborLand  1  164.3  164.28    9.79 0.0069 **
#		Residuals   15  251.7   16.78                  
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  44.09   44.09   7.011 0.0175 *
#		Residuals   16 100.62    6.29                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#msexpectations (Intercept) mslaborLandhigh
#1            low  -4.6077566        6.228053
#2           high  -0.1660292        3.149596
##### NOPE #####

ggplot(postDf,aes(x=agesplit,y=outcomeMeasures,color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=primipsplit,y=outcomeMeasures,color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=msexpectations, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(msexpectations),function(df){
			aovtest3 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest3))
			coef(aovtest3)
		})
#Df Sum Sq Mean Sq F value Pr(>F)   
#mslaborLand  1  164.3  164.28    9.79 0.0069 **
#		Residuals   15  251.7   16.78                  
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  44.09   44.09   7.011 0.0175 *
#		Residuals   16 100.62    6.29                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#msexpectations (Intercept) mslaborLandhigh
#1            low  -4.6077566        6.228053
#2           high  -0.1660292        3.149596
##### NOT QUITE ##### - Just Different...When it comes to 
## expectations, laborLand matters even for high expectations

ggplot(postDf,aes(x=msintuitMov, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=msphysEnv, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=msemotEnv, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=msfluidReal, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(msfluidReal),function(df){
			aovtest4 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest4))
			coef(aovtest4)
		})
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  84.25   84.25   5.097 0.0393 *
#		Residuals   15 247.96   16.53                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  31.12  31.124   3.303  0.088 .
#Residuals   16 150.79   9.424                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#msfluidReal (Intercept) mslaborLandhigh
#1         low  -3.4915513        4.885788
#2        high  -0.1798855        2.935798
##### YEP #####

ggplot(postDf,aes(x=msintensePres, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(msintensePres),function(df){
			aovtest5 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest5))
			coef(aovtest5)
		})
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  91.08   91.08   5.112 0.0391 *
#		Residuals   15 267.23   17.82                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  40.91   40.91   4.263 0.0556 .
#Residuals   16 153.55    9.60                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#msintensePres (Intercept) mslaborLandhigh
#1           low  -3.2411809        5.079829
#2          high  -0.7807744        3.365759
##### YEP #####

ggplot(postDf,aes(x=vocalsplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=agesplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=primipsplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=educationsplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(educationsplit),function(df){
			aovtest6 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest6))
			coef(aovtest6)
		})
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  142.4  142.42   9.034 0.0148 *
#		Residuals    9  141.9   15.77                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  45.29   45.29   4.268 0.0508 .
#Residuals   22 233.46   10.61                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#educationsplit (Intercept) mslaborLandhigh
#1 <4-year-college  -4.5798975        8.079456
#2 4-year-college+  -0.6843181        2.837609
##### YEP #####

ggplot(postDf,aes(x=incomesplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=P015, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=drugsplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=P017, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(P017),function(df){
			aovtest7 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest7))
			coef(aovtest7)
		})
#Df Sum Sq Mean Sq F value  Pr(>F)   
#mslaborLand  1  177.8  177.84   14.11 0.00452 **
#		Residuals    9  113.5   12.61                   
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  27.61  27.611   4.902 0.0541 .
#Residuals    9  50.69   5.632                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)
#mslaborLand  1  53.48   53.48    3.15  0.104
#Residuals   11 186.75   16.98               
#P017 (Intercept) mslaborLandhigh
#1    1  -3.9122817        8.075188
#2    2  -0.2699821        3.181820
#3    3  -3.6027904        4.068707
##### HUH???? WHAT DOES THIS MEAN??? #####
ddply(postDf,.(mslaborLand,P017),function(df){
			data.frame(mean = mean(df$outcomeMeasures),
					se=sqrt(var(df$outcomeMeasures)/nrow(df)),
					n=nrow(df))
		})

ggplot(postDf,aes(x=P018, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE?? #####

ggplot(postDf,aes(x=duedatesplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(duedatesplit),function(df){
			aovtest8 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest8))
			coef(aovtest8)
		})
##### NOPE #####

ggplot(postDf,aes(x=msactiveLabor, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(msactiveLabor),function(df){
			aovtest9 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest9))
			coef(aovtest9)
		})
#Df Sum Sq Mean Sq F value Pr(>F)
#mslaborLand  1  24.02   24.02   2.134  0.162
#Residuals   17 191.36   11.26               
#Df Sum Sq Mean Sq F value  Pr(>F)   
#mslaborLand  1  206.0  205.99   14.89 0.00174 **
#		Residuals   14  193.7   13.83                   
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#msactiveLabor (Intercept) mslaborLandhigh
#1         short  -0.5081091        2.330896
#2          long  -3.9241280        7.411562
##### YEP #####

ggplot(postDf,aes(x=mspushing, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=P026, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=P027, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
# 1 = used birth plan; 2 = didn't use birth plan
ddply(postDf,.(P027),function(df){
			aovtest10 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest10))
			coef(aovtest10)
		})
#Df Sum Sq Mean Sq F value   Pr(>F)    
#mslaborLand  1 182.64  182.64   21.84 0.000679 ***
#		Residuals   11  92.01    8.36                     
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  63.35   63.35   4.029 0.0584 .
#Residuals   20 314.47   15.72                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#P027 (Intercept) mslaborLandhigh
#1    1   -4.500101        7.704379
#2    2   -1.691462        3.407843
##### YEP #####

ggplot(postDf,aes(x=P051, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
# 1 = noboby present besides partner & care providers; 2 = opposite
ddply(postDf,.(P051),function(df){
			aovtest11 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest11))
			coef(aovtest11)
		})
#Df Sum Sq Mean Sq F value Pr(>F)
#mslaborLand  1  12.35   12.35   0.929  0.354
#Residuals   12 159.39   13.28               
#Df Sum Sq Mean Sq F value   Pr(>F)    
#mslaborLand  1  247.5  247.52   20.95 0.000206 ***
#		Residuals   19  224.5   11.81                     
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#P051 (Intercept) mslaborLandhigh
#1    1   -1.591585        1.878113
#2    2   -3.165695        6.874090
##### YEP #####

ggplot(postDf,aes(x=P054, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
## only 6 in second category --> not the best comparison?
ddply(postDf,.(P054),function(df){
			aovtest12 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest12))
			coef(aovtest12)
		})
##### NOPE ##### (both are sig.)

ggplot(postDf,aes(x=msactiveFeel, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(msactiveFeel),function(df){
			aovtest13 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest13))
			coef(aovtest13)
		})
#Df Sum Sq Mean Sq F value Pr(>F)
#mslaborLand  1  33.59   33.59   2.833   0.11
#Residuals   18 213.42   11.86               
#Df Sum Sq Mean Sq F value   Pr(>F)    
#mslaborLand  1  244.0  243.97   21.09 0.000505 ***
#		Residuals   13  150.4   11.57                     
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#msactiveFeel (Intercept) mslaborLandhigh
#1        <4hrs  -0.7834319        2.591954
#2        >4hrs  -4.9948178        8.083920
##### YEP #####

ggplot(postDf,aes(x=mspushingFeel, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

#########################################################################
## testing for other relationships
#########################################################################
ggplot(postDf,aes(x=mspanas,y=memory,color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=agesplit,y=painExp,color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(agesplit),function(df){
			aovtest2 = aov(painExp~mslaborLand,data=df)
			print(summary(aovtest2))
			coef(aovtest2)
		})
#Df Sum Sq Mean Sq F value Pr(>F)
#mslaborLand  1   66.0   65.98   2.383  0.143
#Residuals   15  415.3   27.69               
#Df Sum Sq Mean Sq F value Pr(>F)  
#mslaborLand  1  179.6  179.61    6.33 0.0229 *
#		Residuals   16  454.0   28.38                 
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#agesplit (Intercept) mslaborLandhigh
#1  younger   -2.078517        4.003113
#2    older   -3.553550        6.479718
##### YEP #####

ggplot(postDf,aes(x=primipsplit,y=painExp,color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=primipsplit,y=painExp,color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")

ggplot(postDf,aes(x=primipsplit, y=tActualAct, color=agesplit)) + 
		stat_summary(fun.data = "mean_cl_boot")
# see anova above (copied)

ggplot(postDf,aes(x=primipsplit, y=outcomeMeasures, color=agesplit)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOT QUITE #####

ggplot(postDf,aes(x=primipsplit, y=painExp, color=agesplit)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=primipsplit, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=agesplit, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOT QUITE #####



