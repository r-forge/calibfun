###############################################################################
## interactionTesting.R
##
## file for testing the specific outcomeMeasures~variable*mslaborLand
## interaction
## 
## Author: Haaland
###############################################################################

#########################################################################
## successful patterns
#########################################################################

## PANAS
ggplot(postDf,aes(x=mspanas, y=outcomeMeasures, color=mslaborLand))+ 
		stat_summary(fun.data = "mean_cl_boot") 
aov2 = aov(outcomeMeasures~mspanas*mslaborLand,data=postDf)
summary(aov2)
# summary(aov2)
#                     Df Sum Sq Mean Sq F value  Pr(>F)   
# mspanas              1   60.5   60.52   4.543 0.04109 * 
# mslaborLand          1  160.6  160.62  12.056 0.00154 **
# mspanas:mslaborLand  1   19.5   19.53   1.466 0.23519   
# Residuals           31  413.0   13.32                   
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
ddply(postDf,.(mspanas),function(df){
			aov2b = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aov2b))
			coef(aov2b)
		})
#			 Df Sum Sq Mean Sq F value  Pr(>F)   
#mslaborLand  1  145.0  145.01   9.857 0.00675 **
#Residuals   15  220.7   14.71                   
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#			 Df Sum Sq Mean Sq F value Pr(>F)
#mslaborLand  1  35.13   35.13   2.922  0.107
#Residuals   16 192.33   12.02               
#	mspanas (Intercept) mslaborLandhigh
#1     low  -3.5101726        6.111599
#2    high  -0.6976908        2.963481


#########################################################################
## Pain Management
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



#########################################################################
## Meeting Expectations
ggplot(postDf,aes(x=msexpectations, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aov6 = aov(outcomeMeasures~msexpectations*mslaborLand,data=postDf)
summary(aov6)
# summary(aov6)
#                            Df Sum Sq Mean Sq F value  Pr(>F)   
# msexpectations              1  104.5  104.49   8.562 0.00637 **
# mslaborLand                 1  140.8  140.81  11.538 0.00189 **
# msexpectations:mslaborLand  1   30.0   30.03   2.460 0.12690   
# Residuals                  31  378.3   12.20                   
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
ddply(postDf,.(msexpectations),function(df){
			aov6b = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aov6b))
			coef(aov6b)
		})
#Df Sum Sq Mean Sq F value  Pr(>F)   
#mslaborLand  1  149.6   149.6   9.528 0.00752 **
#		Residuals   15  235.5    15.7                   
#---
#		Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
#Df Sum Sq Mean Sq F value Pr(>F)
#mslaborLand  1  21.23  21.229   2.379  0.143
#Residuals   16 142.80   8.925               
#msexpectations (Intercept) mslaborLandhigh
#1            low  -3.9688917        6.207778
#2           high   0.1432943        2.303766


#########################################################################
## Education
ggplot(postDf,aes(x=educationsplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aovx1 = aov(outcomeMeasures~educationsplit*mslaborLand,data=postDf)
summary(aovx1)
# summary(aovx1)
#                            Df Sum Sq Mean Sq F value  Pr(>F)   
# educationsplit              1   90.6   90.59   7.482 0.01021 * 
# mslaborLand                 1  144.5  144.52  11.936 0.00162 **
# educationsplit:mslaborLand  1   43.2   43.20   3.567 0.06831 . 
# Residuals                  31  375.3   12.11                   
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
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



#########################################################################
## Birth Plan
ggplot(postDf,aes(x=P027, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
# 1 = used birth plan; 2 = didn't use birth plan
aovx3 = aov(outcomeMeasures~P027*mslaborLand,data=postDf)
summary(aovx3)
# summary(aovx3)
#                  Df Sum Sq Mean Sq F value   Pr(>F)    
# P027              1    1.2    1.20   0.092 0.764106    
# mslaborLand       1  209.7  209.67  15.991 0.000366 ***
# P027:mslaborLand  1   36.3   36.32   2.770 0.106147    
# Residuals        31  406.5   13.11                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
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


#########################################################################
## People Present
ggplot(postDf,aes(x=P051, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
# 1 = noboby present besides partner & care providers; 2 = opposite
aovx4 = aov(outcomeMeasures~P051*mslaborLand,data=postDf)
summary(aovx4)
# summary(aovx4)
#                  Df Sum Sq Mean Sq F value   Pr(>F)    
# P051              1    9.9    9.94   0.802 0.377295    
# mslaborLand       1  207.5  207.49  16.757 0.000281 ***
# P051:mslaborLand  1   52.4   52.37   4.229 0.048236 *  
# Residuals        31  383.9   12.38                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
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


#########################################################################
## Actual time spent in active labor
ggplot(postDf,aes(x=msactiveLabor, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aovx2 = aov(outcomeMeasures~msactiveLabor*mslaborLand,data=postDf)
summary(aovx2)
# summary(aovx2)
#                           Df Sum Sq Mean Sq F value   Pr(>F)    
# msactiveLabor              1   38.6   38.63   3.110 0.087666 .  
# mslaborLand                1  177.6  177.64  14.303 0.000667 ***
# msactiveLabor:mslaborLand  1   52.4   52.37   4.217 0.048540 *  
# Residuals                 31  385.0   12.42                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
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


#########################################################################
## Perceived time spent in active labor
ggplot(postDf,aes(x=msactiveFeel, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
aovx5 = aov(outcomeMeasures~msactiveFeel*mslaborLand,data=postDf)
summary(aovx5)
# summary(aovx5)
#                          Df Sum Sq Mean Sq F value   Pr(>F)    
# msactiveFeel              1   12.3   12.26   1.045 0.314676    
# mslaborLand               1  213.1  213.10  18.157 0.000176 ***
# msactiveFeel:mslaborLand  1   64.5   64.47   5.493 0.025683 *  
# Residuals                31  363.8   11.74                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
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








#########################################################################
## No Pattern Found
#########################################################################

ggplot(postDf,aes(x=agesplit,y=outcomeMeasures,color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=primipsplit,y=outcomeMeasures,color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

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
##### NOPE #####

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
##### NOPE #####

ggplot(postDf,aes(x=vocalsplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=agesplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=primipsplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

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
##### NOPE #####

ggplot(postDf,aes(x=duedatesplit, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
ddply(postDf,.(duedatesplit),function(df){
			aovtest8 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest8))
			coef(aovtest8)
		})
##### NOPE #####

ggplot(postDf,aes(x=mspushing, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=P026, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####

ggplot(postDf,aes(x=P054, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
## only 6 in second category --> not the best comparison?
ddply(postDf,.(P054),function(df){
			aovtest12 = aov(outcomeMeasures~mslaborLand,data=df)
			print(summary(aovtest12))
			coef(aovtest12)
		})
##### NOPE ##### (both are sig.)

ggplot(postDf,aes(x=mspushingFeel, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")
##### NOPE #####


ggplot(postDf,aes(x=P052, y=outcomeMeasures, color=mslaborLand)) + 
		stat_summary(fun.data = "mean_cl_boot")



