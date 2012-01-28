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
prefile = "data/Prenatal_Survey20120127.csv"
source("R/readPrenatalData.R")
## check to be sure you have the number of responses you are expecting
dim(preDf)
# dim(preDf)
# [1]   4 104
dim(descPreDf)
# dim(descPreDf)
# [1] 101   3
## the number of columns shouldn't change
checkEquals(ncol(preDf),104)
checkEquals(nrow(descPreDf),101)

## make rdata/postData.RData
postfile = "data/Postpartum_Survey20120127.csv"
source("R/readPostpartumData.R")
## check to be sure you have the number of responses you are expecting
dim(postDf)
# dim(postDf)
# [1]   4 160
dim(descPostDf)
# dim(descPostDf)
# [1] 157   3
## the number of columns shouldn't change
checkEquals(ncol(postDf),160)
checkEquals(nrow(descPostDf),157)
