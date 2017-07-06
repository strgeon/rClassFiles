setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge")
wine=read.csv("wine.csv")
model1=lm(Price ~ AGST, data=wine)
SSE=sum(model1$residuals^2)
model2=lm(Price ~ AGST + HarvestRain, data=wine)
SSE=sum(model2$residuals^2)
model3=lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
SSE=sum(model3$residuals^2)
model4=lm(Price ~ HarvestRain + WinterRain, data=wine)
model4=lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
plot(wine$WinterRain, wine$Price)
or(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)
strsummary(model5)
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
wineTest=read.csv("wine_test.csv")
wineTest
predictTest=predict(model4, newdata=wineTest)
predictTest
#get the sum of squares for predictions
SSE=sum((wineTest$Price - predictTest)^2)
#get the sum of squares for prediction data vs. original training set
SST=sum((wineTest$Price - mean(wine$Price))^2)
#R^2 on test data against training data
1 - SSE/SST
baseball=read.csv("baseball.csv")
str(baseball)
moneyball = subset(baseball, Year < 2002)
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

sum(moneyball$RD)
plot(moneyball$RD, moneyball$W)
winsReg=lm(W~RD,data=moneyball)
summary(winsReg)
#predicting the number of wins based on the intercept and RD cooefficient
80.881375 + (0.105766*(713-614))
runsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(runsReg)
cor(moneyball)
cor(wine)
cor(moneyball$RS, moneyball$OBP, moneyball$SLG, moneyball$BA)
cor(moneyball$BA, moneyball$OBP)
cor(moneyball$BA, moneyball$SLG)
cor(moneyball$OBP, moneyball$SLG)
runsReg = lm(RS ~ OBP + SLG, data=moneyball)
runsReg = lm(RS ~ OBP, data=moneyball)
runsAllowed = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(runsAllowed)
-804.63 + (2737.77*0.311) + (1584.91*0.405)
-837.38 + (2913.60*0.297) + (1514.29*0.370)

#Player Name	OBP	SLG	Salary
#Eric Chavez	0.338	0.540	$1,400,000
-804.63 + (2737.77*0.338) + (1584.91*0.540)
#eremy Giambi	0.391	0.450	$1,065,000
-804.63 + (2737.77*0.391) + (1584.91*0.450)
#Frank Menechino	0.369	0.374	$295,000
-804.63 + (2737.77*0.369) + (1584.91*0.374)
#Greg Myers	0.313	0.447	$800,000
-804.63 + (2737.77*0.313) + (1584.91*0.447)
#Carlos Pena	0.361	0.500	$300,000
-804.63 + (2737.77*0.361) + (1584.91*0.500)

teamRank = c(1,2,3,3,4,4,4,4,5,5)

#Rank 1: San Francisco Giants (Wins = 94)
#Rank 2: Detroit Tigers (Wins = 88)
#Rank 3: New York Yankees (Wins = 95), and St. Louis Cardinals (Wins = 88)
#Rank 4: Baltimore Orioles (Wins = 93), Oakland A's (Wins = 94), Washington Nationals (Wins = 98), Cincinnati Reds (Wins = 97)
#Rank 5: Texas Rangers (Wins = 93), and Atlanta Braves (Wins = 94) 
#reate a vector in R called wins2012, that has the wins of each team in 2012, in order of rank (the vector should have 10 numbers).
#In 2013, the ranking of the teams and their regular season wins were as follows:
#Rank 1: Boston Red Sox (Wins = 97)
#Rank 2: St. Louis Cardinals (Wins = 97)
#Rank 3: Los Angeles Dodgers (Wins = 92), and Detroit Tigers (Wins = 93)
#Rank 4: Tampa Bay Rays (Wins = 92), Oakland A's (Wins = 96), Pittsburgh Pirates (Wins = 94), and Atlanta Braves (Wins = 96)
#ank 5: Cleveland Indians (Wins = 92), and Cincinnati Reds (Wins = 90) 

wins2012=c(94,88,95,88,93,94,98,97,93,94)
wins2013=c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)
cor(teamRank,wins2013)