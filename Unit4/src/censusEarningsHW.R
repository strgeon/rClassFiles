# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data"

census=read.csv("census.csv")
str(census)
# 'data.frame':	31978 obs. of  13 variables:
#  $ age          : int  39 50 38 53 28 37 49 52 31 42 ...
#  $ workclass    : Factor w/ 9 levels " ?"," Federal-gov",..: 8 7 5 5 5 5 5 7 5 5 ...
#  $ education    : Factor w/ 16 levels " 10th"," 11th",..: 10 10 12 2 10 13 7 12 13 10 ...
#  $ maritalstatus: Factor w/ 7 levels " Divorced"," Married-AF-spouse",..: 5 3 1 3 3 3 4 3 5 3 ...
#  $ occupation   : Factor w/ 15 levels " ?"," Adm-clerical",..: 2 5 7 7 11 5 9 5 11 5 ...
#  $ relationship : Factor w/ 6 levels " Husband"," Not-in-family",..: 2 1 2 1 6 6 2 1 2 1 ...
#  $ race         : Factor w/ 5 levels " Amer-Indian-Eskimo",..: 5 5 5 3 3 5 3 5 5 5 ...
#  $ sex          : Factor w/ 2 levels " Female"," Male": 2 2 2 2 1 1 1 2 1 2 ...
#  $ capitalgain  : int  2174 0 0 0 0 0 0 0 14084 5178 ...
#  $ capitalloss  : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ hoursperweek : int  40 13 40 40 40 40 16 45 50 40 ...
#  $ nativecountry: Factor w/ 41 levels " Cambodia"," Canada",..: 39 39 39 39 5 39 23 39 39 39 ...
#  $ over50k      : Factor w/ 2 levels " <=50K"," >50K": 1 1 1 1 1 1 1 2 2 2 ...

library(caTools)
set.seed(2000)
split=sample.split(census$over50k, SplitRatio=0.6)
train=subset(census,split==TRUE)
test=subset(census,split==FALSE)

logm=glm(over50k ~ ., data=train, family="binomial")
#censusglm = glm( over50k ~ . , family="binomial", data = train)
#pred=predict(censusglm, newdata=test, type="response")
#table(pred>.5,test$over50k)
summary(logm)

pred=predict(logm, newdata=test, type="response")
table(test$over50k,pred>.5)
#         
#          FALSE TRUE
#    <=50K  9051  662
#    >50K   1190 1888

(9051+1888)/nrow(test)
# [1] 0.8552107
#baseline choosing most likely value of < 50K

(9051 +662)/nrow(test)
# [1] 0.7593621

library(ROCR)
ROCRpred = prediction(pred,test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
# [1] 0.9061598

library(rpart)
library(rpart.plot)
CARTc = rpart(over50k ~ ., data=train, method="class")
prp(CARTc)

pred=predict(CARTc,newdata=test,type="class")
table(pred,test$over50k)
#         
# pred      <=50K  >50K
#    <=50K   9243  1482
#    >50K     470  1596
(9243+1596)/nrow(test)
# [1] 0.8473927

#need to remove type=class for ROCR
# from previous lecture
#pred=prediction(PredictROC[,2],test$Reverse)
CARTd = rpart(over50k ~ ., data=train, method="class")
pred2=predict(CARTd,newdata=test)
ROCRpred = prediction(pred2[,2],test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
# [1] 0.8470256

perf=performance(ROCRpred,"tpr","fpr")
plot(perf)

#getting a smaller data set for the random forest problem, much more memory intenstive
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
library(randomForest)

# Build random forest model
set.seed(1)
lfor = randomForest(over50k ~ ., data=trainSmall)

pfor = predict(lfor,newdata=test)
table(test$over50k,pfor)
#         pfor
#           <=50K  >50K
#    <=50K   9586   127
#    >50K    1985  1093
(9586+1093)/nrow(test)
# [1] 0.8348839
#As we discussed in lecture, random forest models work by building a large collection of trees. As a result, we lose some of the interpretability that comes with CART in terms of seeing how predictions are made and which variables are important. However, we can still compute metrics that give us insight into which variables are important.
#
#One metric that we can look at is the number of times, aggregated over all of the trees in the random forest model, that a certain variable is selected for a split. To view this metric, run the following lines of R code (replace "MODEL" with the name of your random forest model):
	
vu = varUsed(lfor, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(lfor$forest$xlevels[vusorted$ix]))
# measuring impurity ???
varImpPlot(lfor)
#This code produces a chart that for each variable measures the number of times that variable was selected for splitting (the value on the x-axis). Which of the following variables is the most important in terms of the number of splits?

#now doing cross validation
library("caret")
library(e1071)

tr.control= trainControl(method="cv",number=10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

tr=train(over50k ~ ., data=train, method="rpart", trControl=tr.control, tuneGrid=cartGrid)
tr
#The final value used for the model was cp = 0.002

CARTe = rpart(over50k ~ ., data=train, method="class",cp=.002)
pred3=predict(CARTe,newdata=test,type="class")
table(test$over50k,pred3)
#         pred3
#           <=50K  >50K
#    <=50K   9178   535
#    >50K    1240  1838
(9178+1838)/nrow(test)
# [1] 0.8612306
prp(CARTe)


ROCRpred = prediction(pred3[,2],test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
