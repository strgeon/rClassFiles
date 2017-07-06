data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
plot(statedata$x,statedata$y)
tapply(statedata$HS.Grad,statedata$state.region, mean)
boxplot(statedata$Murder ~ statedata$state.region)

ne=subset(statedata,statedata$state.region=="Northeast")
ne[c("Murder", "state.name")]
ne[c("Murder")]

lifeEx=lm(Life.Exp ~ Population + Income + Illiteracy + 
Murder + HS.Grad + Frost + Area,data=statedata)

#LinReg = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
#summary(LinReg)

plot(statedata$Income, statedata$Life.Exp
cor(statedata$Income, statedata$HS.Grad)
cor(statedata$Income, statedata$Illiteracy)
cor(statedata$Income, statedata$Murder)
cor(statedata$Income, statedata$Area)

lifeEx=lm(Life.Exp ~ Population + Murder + HS.Grad + Frost,data=statedata)

predTest=predict(lifeEx,data=statedata)
statedata[c("state.name","Life.Exp")]

lifeEx2=statedata[c("Life.Exp")]
sort(lifeEx2)

which.min(statedata$Life.Exp)
statedata$state.name[40]

#looking for the lowest residual (error)
which.min(lifeEx$residuals^2)
which.max(lifeEx$residuals^2)
sort(abs(lifeEx$residuals))
sort(abs(statedata$Life.Exp - predict(lifeEx)))


