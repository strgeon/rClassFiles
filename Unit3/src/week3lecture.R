logit=-1.5 + 3 - 2.5
exp(logit)
1/1+(exp(-logit))
#value of p=1
1/(1 + (exp(-logit)))

setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit3/src")
getwd()
library(caTools)
set.seed(88)

quality = read.csv("quality.csv")

# Look at structure
str(quality)

# Table outcome
table(quality$PoorCare)

split = sample.split(quality$PoorCare, SplitRatio=0.75)
split
qualityTrain=subset(quality,split==TRUE)
qualityTest=subset(quality,split==FALSE)
QualityLog=glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
QualityLog2=glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
PredictTrain=predict(QualityLog, type="response")
summary(PredictTrain)

predictTest = predict(QualityLog, type="response", newdata=qualityTest)
summary(QualityLog)
summary(QualityLog2)

#install.packages("ROCR")
library(ROCR)

ROCRpred=prediction(PredictTrain, qualityTrain$PoorCare)
ROCRperf=performance(ROCRpred, "tpr","fpr")
plot(ROCRperf)

plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

predictTest = predict(QualityLog2, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
summary(auc)
str(auc)


