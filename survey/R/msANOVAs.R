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
aov2 = aov(outcomeMeasures~mslaborLand*lmhpanas,data=postDf)
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


## this plot isn't very helpful - don't need to do it each time
#plot(aov2)

# haven't resaved this after tweaking the ll score (want to save for
# comparison) - 3/2/12
pdf("plots/2x3anova.pdf")
ggplot(postDf,aes(x=lmhpanas, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot") 
dev.off()

#########################################################################
## analysis of variance: effect of panas on different themes
#########################################################################
ggplot(postDf,aes(x=lmhpanas, y=laborLand)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov3 = aov(laborLand~lmhpanas,data=postDf)
summary(aov3)
# summary(aov3)
#             Df Sum Sq Mean Sq F value Pr(>F)
# lmhpanas     2  218.1  109.05   1.267  0.295
# Residuals   32 2753.8   86.06                                            

## this says that panas scores do not create a significant difference
## in laborLand scores

#########################################################################
ggplot(postDf,aes(x=lmhpanas, y=outcomeMeasures)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov10 = aov(outcomeMeasures~lmhpanas,data=postDf)
summary(aov10)
# summary(aov10)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# lmhpanas     2   92.9   46.47   2.652 0.0859 .
# Residuals   32  560.7   17.52                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#########################################################################
ggplot(postDf,aes(x=lmhpanas, y=painExp)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov11 = aov(painExp~lmhpanas,data=postDf)
summary(aov11)
# summary(aov11)
#             Df Sum Sq Mean Sq F value Pr(>F)
# lmhpanas     2  149.7   74.86   2.466  0.101
# Residuals   32  971.3   30.35               

#########################################################################
ggplot(postDf,aes(x=lmhpanas, y=emotEnv)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov12 = aov(emotEnv~lmhpanas,data=postDf)
summary(aov12)
# summary(aov12)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# lmhpanas     2    215  107.52   2.904 0.0694 .
# Residuals   32   1185   37.03                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#########################################################################
ggplot(postDf,aes(x=lmhpanas, y=expectations)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov13 = aov(expectations~lmhpanas,data=postDf)
summary(aov13)
# summary(aov13)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# lmhpanas     2  28.93  14.465   2.577 0.0917 .
# Residuals   32 179.62   5.613                 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

#########################################################################
ggplot(postDf,aes(x=lmhpanas, y=intuitMov)) + 
		stat_summary(fun.data = "mean_cl_boot") 
aov14 = aov(intuitMov~lmhpanas,data=postDf)
summary(aov14)
# summary(aov14)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
# lmhpanas     2  92.51   46.25   5.777 0.00721 **
# Residuals   32 256.23    8.01                   
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
# mslaborLand  1  248.3  248.26   9.387 0.00433 **
# Residuals   33  872.8   26.45                   
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
		stat_summary(fun.data = "mean_cl_boot")
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


ggplot(postDf,aes(x=lmhpanas, y=painExp, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov8 = aov(painExp~mslaborLand*lmhpanas,data=postDf)
summary(aov8)
# summary(aov8)
#                      Df Sum Sq Mean Sq F value  Pr(>F)   
# mslaborLand           1  238.5  238.46   9.282 0.00489 **
# lmhpanas              2   61.2   30.59   1.191 0.31842   
# mslaborLand:lmhpanas  2   76.3   38.17   1.486 0.24306   
# Residuals            29  745.1   25.69                   
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 


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



