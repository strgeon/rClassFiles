#Climate change models
cng=read.csv("climate_change.csv")
cngtrn=subset(cng,Year < 2007)
cngtst=subset(cng,Year > 2006)
cngmdl1=lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=cngtrn)
summary(cngmdl1)
cor(cngtrn)
cngmdl2=lm(Temp ~ MEI + N2O + TSI + Aerosols, data=cngtrn)
summary(cngmdl2)
cngmdl3=step(cngmdl1)
predictTest=predict(cngmdl3, newdata=cngtst)
predictTest
SSE=sum((cngtst$Temp - predictTest)^2)
SST=sum((cngtst$Temp - mean(cngtrn$Temp))^2)
1-SSE/SST