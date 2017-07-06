# TODO: Add comment
# 
# Author: Scott
###############################################################################


getwd()
setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data"

Claims=read.csv("ClaimsData.csv")
str(Claims)

table(Claims$bucket2009)/nrow(Claims)
# 
#           1           2           3           4           5 
# 0.671267781 0.190170413 0.089466272 0.043324855 0.005770679 

library(caTools)
set.seed(88)
spl=sample.split(Claims$bucket2009,SplitRatio = 0.6)
ClaimsTrain=subset(Claims,spl==TRUE)
ClaimsTest=subset(Claims,spl==FALSE)
mean(ClaimsTrain$age)
# [1] 72.63773


nrow(subset(ClaimsTrain,diabetes==1))/nrow(ClaimsTrain)
# [1] 0.3808983
summary(ClaimsTrain)
table(Claims$diabetes)

table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
#    
#          1      2      3      4      5
#   1 110138   7787   3427   1452    174
#   2  16000  10721   4629   2931    559
#   3   7006   4629   2774   1621    360
#   4   2688   1943   1415   1539    352
#   5    293    191    160    309    104
(110138+10721+2774+1539+104)/nrow(ClaimsTest)
# accuracy of baseline model 
# [1] 0.6838135
(110138)/nrow(ClaimsTest)
# alternative baseline model that buckets everyone in bucket 1
# [1] 0.6011834


PenaltyMatrix= matrix(c(0, 1, 2, 3, 4, 2, 0, 1, 2, 3,4, 2, 0, 1, 2,6, 4, 2, 0, 1,8, 6, 4, 2, 0), byrow=TRUE, nrow=5)
PenaltyMatrix
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    0    1    2    3    4
# [2,]    2    0    1    2    3
# [3,]    4    2    0    1    2
# [4,]    6    4    2    0    1
# [5,]    8    6    4    2    0

as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix
#    
#         1     2     3     4     5
#   1     0  7787  6854  4356   696
#   2 32000     0  4629  5862  1677
#   3 28024  9258     0  1621   720
#   4 16128  7772  2830     0   352
#   5  2344  1146   640   618     0


sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)
# penalty error
# [1] 0.7386055

(122978)/nrow(ClaimsTest)
# alternative baseline model that buckets everyone in bucket 1
# [1] 0.67127

ClaimsTest$altbucket=1
table(ClaimsTest$bucket2009, ClaimsTest$altbucket)
#    
#          1
#   1 122978
#   2  34840
#   3  16390
#   4   7937
#   5   1057


altPenaltyMatrix= matrix(c(0, 2, 4, 6, 8), byrow=TRUE, nrow=5)
altPenaltyMatrix
#      [,1]
# [1,]    0
# [2,]    2
# [3,]    4
# [4,]    6
# [5,]    8

as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$altbucket))*altPenaltyMatrix
#    
#         1
#   1     0
#   2 69680
#   3 65560
#   4 47622
#   5  8456

sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$altbucket))*altPenaltyMatrix)/nrow(ClaimsTest)
# [1] 1.044301


32000+28024+16128+2344

library(rpart)
library(rpart.plot)

ClaimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp = 0.00005)
prp(ClaimsTree)
PredictTest=predict(ClaimsTree,newdata=ClaimsTest,type="class")
table(ClaimsTest$bucket2009,PredictTest)
#    PredictTest
#          1      2      3      4      5
#   1 114141   8610    124    103      0
#   2  18409  16102    187    142      0
#   3   8027   8146    118     99      0
#   4   3099   4584     53    201      0
#   5    351    657      4     45      0
(114141 + 16102 + 118 + 201 + 0)/nrow(ClaimsTest)
# [1] 0.7126669

as.matrix(table(ClaimsTest$bucket2009,PredictTest))*PenaltyMatrix
#    PredictTest
#         1     2     3     4     5
#   1     0  8610   248   309     0
#   2 36818     0   187   284     0
#   3 32108 16292     0    99     0
#   4 18594 18336   106     0     0
#   5  2808  3942    16    90     0

sum(as.matrix(table(ClaimsTest$bucket2009,PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
# [1] 0.7578902

ClaimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp = 0.00005, parms = list(loss=PenaltyMatrix))
PredictTest=predict(ClaimsTree,newdata=ClaimsTest,type="class")
table(ClaimsTest$bucket2009,PredictTest)
#    PredictTest
#         1     2     3     4     5
#   1 94310 25295  3087   286     0
#   2  7176 18942  8079   643     0
#   3  3590  7706  4692   401     1
#   4  1304  3193  2803   636     1
#   5   135   356   408   156     2

(94310 + 18942+ 4692 + 636 + 2)/nrow(ClaimsTest)
# [1] 0.6472746



as.matrix(table(ClaimsTest$bucket2009,PredictTest))*PenaltyMatrix
#    PredictTest
#         1     2     3     4     5
#   1     0 25295  6174   858     0
#   2 14352     0  8079  1286     0
#   3 14360 15412     0   401     2
#   4  7824 12772  5606     0     1
#   5  1080  2136  1632   312     0

sum(as.matrix(table(ClaimsTest$bucket2009,PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
# [1] 0.6418161

