#Part 2 - Reading test scores
pisaTest=read.csv("pisa2009test.csv")
pisaTrain=read.csv("pisa2009train.csv")

str(pisaTest)
str(pisaTrain)

tapply(pisaTrain$readingScore,pisaTrain$male,mean)
#Type the following commands into your R console to remove observations with any missing value from pisaTrain and pisaTest:

pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)

LinReg = lm(readingScore ~ ., data = pisaTrain)
summary(LinReg)

#Root Mean Squared Error (RMSE)
#The square root of the mean/average of the square of all of the error.
#RMSE is very commonly used and makes for an excellent general purpose error metric for numerical predictions.
#Compared to the similar Mean Absolute Error, RMSE amplifies and severely punishes large errors.
SSE=sum(LinReg$residuals^2)
RMSE=sqrt(SSE/nrow(pisaTrain))

pisamdl1=lm(readingScore ~ grade,data=pisaTrain)
105.289 + (40.752*(11))
105.289 + (40.752*(9))
554-472
76.489006 +(29.542707*11)
401.04-341.86

lmScore=lm(readingScore ~ ., data=pisaTrain)

predTest=predict(lmScore, newdata=pisaTest)

lmTest = lm(readingScore ~ ., data = pisaTest)
summary(lmTest)
SSE=sum(lmTest$residuals^2)
SSE
RMSE=sqrt(SSE/nrow(pisaTest))
RMSE
SSE=sum((pisaTest$readingScore - predTest)^2)

lmScore=lm(readingScore ~ ., data=pisaTest)
summary(lmScore)
SSE=sum(lmScore$residuals^2)

SSE=sum((pisaTest$readingScore - predTest)^2)
RMSE=sqrt(SSE/nrow(pisaTest))
mean(pisaTrain$readingScore)
SST=sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)

