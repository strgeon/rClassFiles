# TODO: Add comment
# 
# Author: Scott
###############################################################################


getwd()
setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data"

14/20
stevens=read.csv("stevens.csv")
str(stevens)

library(caTools)
set.seed(3000)
spl=sample.split(stevens$Reverse, SplitRatio=0.7)
train=subset(stevens,spl==TRUE)
test=subset(stevens,spl==FALSE)
install.packages("rpart")
library(rpart)
library(rpart.plot)
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, method="class",minbucket=25)
prp(StevensTree)
table(stevens$Respondent)
PredictCART=predict(StevensTree,newdata=test,type="class")
table(test$Reverse, PredictCART)
#    PredictCART
#      0  1
#   0 41 36
#   1 22 71
#baseline model that predicts always reverse
(22+71)/(41+36+22+71)
# [1] 0.5470588

library(ROCR)
PredictROC=predict(StevensTree,newdata=test)
PredictROC
pred=prediction(PredictROC[,2],test$Reverse)
perf=performance(pred,"tpr","fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)
# [1] 0.6927105
StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, method="class",minbucket=5)
prp(StevensTree2)
StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, method="class",minbucket=100)
prp(StevensTree3)

library(randomForest)
train$Reverse=as.factor(train$Reverse)
test$Reverse=as.factor(test$Reverse)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, ntree=200,nodesize=25)
PredictForest=predict(StevensForest,newdata=test)
table(test$Reverse,PredictForest)
#    PredictForest
#      0  1
#   0 41 36
#   1 17 76
set.seed(200)
StevensForest2 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, ntree=200,nodesize=25)
PredictForest2=predict(StevensForest2,newdata=test)
table(test$Reverse,PredictForest2)
#seed 200
#    PredictForest2
#      0  1
#   0 44 33
#   1 17 76
# seed 100
#    PredictForest2
#      0  1
#   0 43 34
#   1 19 74

(43+74)/(43+74+19+34)
# [1] 0.6882353
(44+76)/(44+76+17+33)
# [1] 0.7058824

library(caret)
library(e1071)

numFolds=trainControl(method="cv",number=10)
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, method="rpart", trControl=numFolds, tuneGrid=cpGrid)
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=train, method="class", cp=0.18)
predictCV=predict(StevensTreeCV,newdata=test,type="class")
table(test$Reverse,predictCV)
#    predictCV
#      0  1
#   0 59 18
#   1 29 64
(59+64)/(59+64+19+18)
# [1] 0.76875
prp(StevensTreeCV)