###############################################################################
## matchData.R
##
## create a new data.frame that includes 8 matched data points
## 
## Author: Haaland
###############################################################################

## matched pp ResponseIds are: 
## R_54rQTggSaBLCCA4 (5) - R_cY1nryWnpQituwQ
## R_0VDqOn7hxieFCu0 (13) - R_ey3MjL9slm9g5GA
## R_3ylAg4I45c5TyBu (33) - R_1BoEBK4nLlWop4o
## R_cx7sE7IbR2887l2 (25) - R_8vUmIGnc0zXaf76
## R_1Bn3j9AY0UEGK0c (12) - R_2beAKU8XexSLQ1u
## R_bCsIzFgI54ralPC (31) - R_bsgDpryJoF413KI
## R_8vOGp3BkFjRSLE8 (30) - R_0kxQzt8H3gF5TLe
## R_01dq0wGV233o0N6 (34) - R_e3vMFdxeq9OJGtK

# combine these two response sets into one matrix
# this includes final recoded data for both sets (3/7/12)
matchedPostdf = postDf[c(5,13,33,25,12,31,30,34),]
matchedPredf = preDf
matchedDataDf = cbind(matchedPostdf,matchedPredf)

# check due dates
matchedDataDf[,c("P002","N002")]
# matchedDataDf[,c("P002","N002")]
#        P002     N002
# 6  12/28/11 12/28/11
# 14  1/16/12  1/16/12
# 34  1/30/12  1/31/12
# 26  1/25/12  1/25/12
# 13  1/14/12  1/14/12
# 32  1/31/12  1/31/12
# 31  1/25/12  1/25/12
# 35   2/7/12   2/3/12

# compare PANAS scores
(1:ncol(matchedDataDf))[names(matchedDataDf) %in% c("P028","P047")]
# (1:ncol(matchedDataDf))[names(matchedDataDf) %in% c("P028","P047")]
# [1] 29 48
(1:ncol(matchedDataDf))[names(matchedDataDf) %in% c("N015","N034")]
# (1:ncol(matchedDataDf))[names(matchedDataDf) %in% c("N015","N034")]
# [1] 197 216
matchedDataDf[,c(29:48,197:216)]
# matchedDataDf[,c(29:48,197:216)]
#    P028 P029 P030 P031 P032 P033 P034 P035 P036 P037 P038 P039 P040 P041 P042
# 6     3    4    3    4    4    5    4    5    3    4    4    3    5    2    4
# 14    4    3    5    3    5    4    5    5    4    4    3    5    4    4    4
# 34    4    4    4    5    4    5    4    5    3    3    3    4    5    3    3
# 26    3    5    3    4    4    4    5    5    3    4    4    3    5    4    5
# 13    3    5    3    4    3    5    4    5    3    3    4    3    4    3    4
# 32    4    4    3    4    4    5    5    5    4    4    4    4    5    4    5
# 31    5    5    5    5    4    5    5    5    5    5    4    5    5    4    5
# 35    4    3    4    3    3    4    4    5    4    4    5    4    4    4    3
#    P043 P044 P045 P046 P047 N015 N016 N017 N018 N019 N020 N021 N022 N023 N024
# 6     4    3    5    1    5    3    4    3    4    1    5    5    5    2    2
# 14    4    4    5    5    4    5    4    5    5    5    5    3    5    5    5
# 34    4    4    5    4    5    4    4    3    4    4    5    5    5    3    3
# 26    4    4    5    3    5    4    4    3    4    4    4    4    5    3    4
# 13    3    3    5    3    4    3    5    3    4    3    4    4    5    3    3
# 32    4    4    4    4    5    5    4    4    4    4    5    4    5    5    4
# 31    5    5    5    5    5    4    5    5    5    4    5    4    5    5    5
# 35    3    4    4    4    4    4    5    4    5    4    4    4    5    4    3
#    N025 N026 N027 N028 N029 N030 N031 N032 N033 N034
# 6     4    2    5    1    3    3    2    4    2    5
# 14    4    4    5    5    3    5    4    5    4    4
# 34    4    3    5    3    4    4    4    5    4    5
# 26    3    3    4    4    4    3    4    5    3    4
# 13    4    3    4    3    5    3    3    5    3    4
# 32    3    4    5    4    4    4    4    5    4    5
# 31    4    4    5    4    4    5    5    5    4    5
# 35    5    4    5    4    5    3    5    4    4    4

