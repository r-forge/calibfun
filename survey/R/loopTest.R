###############################################################################
## loopTest.R
##
## intro to loops
## 
## Author: Haaland
###############################################################################

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
# P015         1  23.32  23.317   4.154 0.0496 *
# Residuals   33 185.23   5.613       

loopvars = c("painExp","expectations")
for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"P015")]
	names(df)[1]="y"
	aov1=aov(y~P015,data=df)
	print(summary(aov1))
}

for (i in 1:length(loopvars)){
	cat("i=",i," var=",loopvars[i],"\n")
	df=postDf[,c(loopvars[i],"laborLand")]
	names(df)[1]="y"
	lm1=lm(y~laborLand,data=df)
	print(summary(lm1))
}