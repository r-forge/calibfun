##############################################################################
## driver.R
##
## shows and explains the order in which scripts should be run
## 
## Author: Haaland
###############################################################################

library(RUnit)

#####################################################################################
## read the surveys -- this needs to be done after every new download
## the data will be saved to an rdata object
#####################################################################################
## makes rdata/preData.RData
## Note: when you save the data, you should always modify the name so it 
## shows the date, then correct the next line accordingly
prefile = "data/Prenatal_Survey20111222.csv"
source("R/readPrenatalData.R")
## check to be sure you have the number of responses you are expecting
dim(preDf)
# dim(preDf)
# [1]   4 105
dim(descPreDf)
# dim(descPreDf)
# [1] 102   3
## the number of columns shouldn't change
checkEquals(ncol(preDf),105)
checkEquals(nrow(descPreDf),102)

## make rdata/postData.RData
postfile = "data/Postpartum_Survey20111124.csv"
source("R/readPostpartumData.R")
## check to be sure you have the number of responses you are expecting
dim(postDf)
# dim(postDf)
# [1]   4 161
dim(descPostDf)
# dim(descPostDf)
# [1] 158   3
## the number of columns shouldn't change
checkEquals(ncol(postDf),161)
checkEquals(nrow(descPostDf),158)
