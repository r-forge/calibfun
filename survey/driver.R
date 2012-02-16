##############################################################################
## driver.R
##
## shows and explains the order in which scripts should be run
## 
## Author: Haaland
###############################################################################

library(RUnit)
library(psych)

#####################################################################################
## read the surveys -- this needs to be done after every new download
## the data will be saved to an rdata object
#####################################################################################


#####################################################################################
## makes rdata/preData.RData
## 
#####################################################################################
## Note: when you save the data, you should always modify the name so it 
## shows the date, then correct the next line accordingly
prefile = "data/Prenatal_Survey20120127.csv"
source("R/readPrenatalData.R")
## check to be sure you have the number of responses you are expecting
dim(preDf)
# dim(preDf)
# [1]  12 102
dim(descPreDf)
# dim(descPreDf)
# [1] 101   3
## the number of columns shouldn't change
checkEquals(ncol(preDf),102)
checkEquals(nrow(descPreDf),101)

## now fix the recoding or reverse orders
source("R/recodePreVars.R")


#####################################################################################
## make rdata/postData.RData
## 
#####################################################################################
## Note: when you save the data, you should always modify the name so it 
## shows the date, then correct the next line accordingly
postfile = "data/Postpartum_Survey20120215.csv"
source("R/readPostpartumData.R")
## check to be sure you have the number of responses you are expecting
dim(postDf)
# dim(postDf)
# [1]  31 157
dim(descPostDf)
# dim(descPostDf)
# [1] 156   3
## the number of columns shouldn't change
checkEquals(ncol(postDf),157)
checkEquals(nrow(descPostDf),156)

## now fix the recoding or reverse orders
source("R/recodePostVars.R")


#####################################################################################
## make some histograms of the data 
#####################################################################################
source("R/plotHistograms.R")

#########################################################################
## make laborLand and theme scales - do line by line -
#########################################################################
#source("R/makeScalesPost.R")

#########################################################################
## run individual analyses
#########################################################################
source("R/individualAnalyses.R")

