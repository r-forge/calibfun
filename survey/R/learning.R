###############################################################################
## learning.R
##
## Practicing code
## 
## Author: Haaland
###############################################################################



#########################################################################
## plot examples from Crawley
#########################################################################

plot(rnorm(100), rnorm(100), ylab="Label for y axis", 
		xlab="Label for x axis",
		las=1)

x <- seq(-4,4,len=101)
plot(x,sin(x),type="l",xaxt="n",xlab=expression(paste("Phase Angle",phi)),
		ylab=expression("sin"*phi))
		axis(1,at=c(-pi, -pi/2, 0, pi/2, pi),
		lab=expression(-pi, -pi/2, 0, pi/2, pi))
text(-pi/2,0.5,substitute(chi^2=="24.5"))

A <- array (1:30, c(5,3,2))
A
A[,2:3,]
A[2:4,2:3,]
A[2:4,2:3,2]


x <- c(1:10)
x <- y*1.25

central <- function(x) {
	cat("Median", median(x),"\n")
	cat("Arithmetic mean", mean(x), "\n")
}

central(x)