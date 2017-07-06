#hyundai elantry
setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit2")
elanTot=read.csv("elantra.csv")

elanTrain=subset(elanTot,Year<2013)
elanTest=subset(elanTot,Year>2012)
elanSales=lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data=elanTrain)
elanSales=lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data=elanTrain)
elanTrain$MthFactor=as.factor(elanTrain$Month)
elanSales=lm(ElantraSales ~ MthFactor + Unemployment + CPI_all + CPI_energy + Queries, data=elanTrain)
elanTest$MthFactor=as.factor(elanTest$Month)

cor(elanTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])
elanSales=lm(ElantraSales ~ MthFactor + Unemployment + CPI_all + CPI_energy, data=elanTrain)
summary(elanSales)

PredictTest = predict(elanSales,newdata=elanTest)
SSE = sum(PredictTest - elanTest$ElantraSales)^2

SSE=(sum(elanSales$residuals^2))