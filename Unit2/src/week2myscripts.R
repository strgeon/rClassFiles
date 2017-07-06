NBA=read.csv("NBA_train.csv")
str(NBA)
table(NBA$W, NBA$Playoffs)
NBA$PTSdiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
winsReg = lm(W ~ PTSdiff, data=NBA)
summary(winsReg)
pointsReg=lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data=NBA)
summary(pointsReg)
pointsReg$residuals
SSE =  sum(pointsReg$residuals^2)
RMSE = sqrt(SSE/nrow(NBA))
RMSE
mean(NBA$PTS)
pointsReg2=lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data=NBA)
pointsReg3=lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data=NBA)
pointsReg4=lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=NBA)
pointsReg5=lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=NBA)
pointsReg6=lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=NBA)
summary(pointsReg3)
NBA_test = read.csv("NBA_test.csv")
PointsPrediction = predict(pointsReg4, newdata = NBA_test)
SSE=sum((PointsPrediction - NBA_test$PTS)^2)
SST=sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSE/SST
R2
RMSE = sqrt(SSE/nrow(NBA_test))
RMSE
