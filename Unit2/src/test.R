# Title     : TODO
# Objective : TODO
# Created by: Scott
# Created on: 7/1/2017


#part 3 - flu queries

FluTrain=read.csv("FluTrain.csv")

#next one is wrong, don't use $ notation in lm
#FluTrend1=lm(log(FluTrain$ILI) ~ FluTrain$Queries, data = pisaTrain)
FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
FluTest=read.csv("FluTest.csv")
predTest1=exp(predict(FluTrend1, newdata=FluTest))
which(FluTest$Week == "2012-03-11 - 2012-03-17")
#answer 11
predTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
#getting the actual, the estimated, then the relative error (act-est)/act
FluTest$ILI[11]
predTest1[11]
(FluTest$ILI[11]-predTest1[11])/FluTest$ILI[11]
#SSE=sum((pisaTest$readingScore - predTest)^2)
#RMSE=sqrt(SSE/nrow(pisaTest))
#mean(pisaTrain$readingScore)
#SST=sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
SSE=sum((FluTest$ILI - predTest1)^2)
SST=sum((FluTest$ILI - mean(FluTrain$ILI))^2)
RMSE=sqrt(SSE/nrow(FluTest))
#MIT answer
#The RMSE can be calculated by first computing the SSE:
SSE = sum((PredTest1-FluTest$ILI)^2)
#and then dividing by the number of observations and taking the square root:
RMSE = sqrt(SSE / nrow(FluTest))
#Alternatively, you could use the following command:
RMSE = sqrt(mean((PredTest1-FluTest$ILI)^2))
#my analysis -
#SSE is the sum of the absolute values of the -
#difference between predicted and observed i.e. "standard error" -
#or the sum of length of the difference vectors
#RMSE is the mean of the absolute values of the -
#difference between the prediction and the observed
#basically the mean of the length of the difference vector
install.packages("zoo")
library(zoo)s

