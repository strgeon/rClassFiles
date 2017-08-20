# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Final Exam/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Final Exam/data"

visits = read.csv("park_visits.csv")

str(visits)
# 'data.frame':	25587 obs. of  12 variables:
#  $ ParkName        : Factor w/ 305 levels "Abraham Lincoln Birthplace NHP",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ ParkType        : Factor w/ 8 levels "National Battlefield",..: 3 3 3 3 3 3 3 3 3 3 ...
#  $ Region          : Factor w/ 7 levels "Alaska","Intermountain ",..: 7 7 7 7 7 7 7 7 7 7 ...
#  $ State           : Factor w/ 53 levels "AK","AL","AR",..: 19 19 19 19 19 19 19 19 19 19 ...
#  $ Year            : int  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
#  $ Month           : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ lat             : num  37.6 37.6 37.6 37.6 37.6 ...
#  $ long            : num  -85.7 -85.7 -85.7 -85.7 -85.7 ...
#  $ cost            : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ logVisits       : num  8.26 8.55 8.99 9.81 9.87 ...
#  $ laglogVisits    : num  NA 8.26 8.55 8.99 9.81 ...
#  $ laglogVisitsYear: num  NA NA NA NA NA NA NA NA NA NA ...
visits2016july = subset(visits,Year==2016 & Month==7)
str(visits2016july)

table(visits2016july$ParkType)
# 
#     National Battlefield   National Historic Site National Historical Park 
#                       10                       75                       42 
#        National Memorial        National Monument            National Park 
#                       28                       68                       57 
# National Recreation Area        National Seashore 
#                       15                       10 
which.max(visits2016july$logVisits)
# [1] 138
visits2016july$ParkName

tapply(visits2016july$logVisits,visits2016july$Region,mean)
#            Alaska    Intermountain           Midwest  National Capital  
#          9.374157         10.129625          9.747281         10.293175 
#        Northeast      Pacific West         Southeast  
#          9.914755         10.767849         10.027672 

cor(visits2016july$cost,visits2016july$logVisits)
# [1] 0.4010611

ys=subset(visits,ParkName == "Yellowstone NP")
# 

str(ys)
# 'data.frame':	84 obs. of  12 variables:
#  $ ParkName        : Factor w/ 305 levels "Abraham Lincoln Birthplace NHP",..: 303 303 303 303 303 303 303 303 303 303 ...
#  $ ParkType        : Factor w/ 8 levels "National Battlefield",..: 6 6 6 6 6 6 6 6 6 6 ...
#  $ Region          : Factor w/ 7 levels "Alaska","Intermountain ",..: 2 2 2 2 2 2 2 2 2 2 ...
#  $ State           : Factor w/ 53 levels "AK","AL","AR",..: 53 53 53 53 53 53 53 53 53 53 ...
#  $ Year            : int  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
#  $ Month           : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ lat             : num  44.6 44.6 44.6 44.6 44.6 ...
#  $ long            : num  -111 -111 -111 -111 -111 ...
#  $ cost            : num  30 30 30 30 30 30 30 30 30 30 ...
#  $ logVisits       : num  10.15 10.28 9.95 10.4 12.43 ...
#  $ laglogVisits    : num  NA 10.15 10.28 9.95 10.4 ...
#  $ laglogVisitsYear: num  NA NA NA NA NA NA NA NA NA NA ...

ys_ts=ts(ys$logVisits,start=c(2010,1),freq=12)
# 
plot(ys_ts)
# 
colSums(is.na(visits))
#         ParkName         ParkType           Region            State 
#                0                0                0                0 
#             Year            Month              lat             long 
#                0                0               84               84 
#             cost        logVisits     laglogVisits laglogVisitsYear 
#                0                0              305             3660 

is.na(visits)
visits$ParkName[7488]

fl=subset(visits,ParkName=="Flight 93 NMEM")
str(fl)
# 'data.frame':	84 obs. of  12 variables:
#  $ ParkName        : Factor w/ 305 levels "Abraham Lincoln Birthplace NHP",..: 90 90 90 90 90 90 90 90 90 90 ...
#  $ ParkType        : Factor w/ 8 levels "National Battlefield",..: 4 4 4 4 4 4 4 4 4 4 ...
#  $ Region          : Factor w/ 7 levels "Alaska","Intermountain ",..: 5 5 5 5 5 5 5 5 5 5 ...
#  $ State           : Factor w/ 53 levels "AK","AL","AR",..: 40 40 40 40 40 40 40 40 40 40 ...
#  $ Year            : int  2010 2010 2010 2010 2010 2010 2010 2010 2010 2010 ...
#  $ Month           : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ lat             : num  40.1 40.1 40.1 40.1 40.1 ...
#  $ long            : num  -78.9 -78.9 -78.9 -78.9 -78.9 ...
#  $ cost            : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ logVisits       : num  7.31 5.93 8.31 9.1 9.55 ...
#  $ laglogVisits    : num  NA 7.31 5.93 8.31 9.1 ...
#  $ laglogVisitsYear: num  NA NA NA NA NA NA NA NA NA NA ...
summary(fl)
#                            ParkName                      ParkType 
#  Flight 93 NMEM                :84   National Memorial       :84  
#  Abraham Lincoln Birthplace NHP: 0   National Battlefield    : 0  
#  Acadia NP                     : 0   National Historic Site  : 0  
#  Adams NHP                     : 0   National Historical Park: 0  
#  African Burial Ground NM      : 0   National Monument       : 0  
#  Agate Fossil Beds NM          : 0   National Park           : 0  
#  (Other)                       : 0   (Other)                 : 0  
#                Region       State         Year          Month      
#  Alaska           : 0   PA     :84   Min.   :2010   Min.   : 1.00  
#  Intermountain    : 0   AK     : 0   1st Qu.:2011   1st Qu.: 3.75  
#  Midwest          : 0   AL     : 0   Median :2013   Median : 6.50  
#  National Capital : 0   AR     : 0   Mean   :2013   Mean   : 6.50  
#  Northeast        :84   AS     : 0   3rd Qu.:2015   3rd Qu.: 9.25  
#  Pacific West     : 0   AZ     : 0   Max.   :2016   Max.   :12.00  
#  Southeast        : 0   (Other): 0                                 
#       lat             long             cost     logVisits       laglogVisits   
#  Min.   :40.06   Min.   :-78.89   Min.   :0   Min.   : 5.930   Min.   : 5.930  
#  1st Qu.:40.06   1st Qu.:-78.89   1st Qu.:0   1st Qu.: 8.958   1st Qu.: 8.946  
#  Median :40.06   Median :-78.89   Median :0   Median : 9.883   Median : 9.894  
#  Mean   :40.06   Mean   :-78.89   Mean   :0   Mean   : 9.643   Mean   : 9.651  
#  3rd Qu.:40.06   3rd Qu.:-78.89   3rd Qu.:0   3rd Qu.:10.549   3rd Qu.:10.551  
#  Max.   :40.06   Max.   :-78.89   Max.   :0   Max.   :11.231   Max.   :11.231  
#                                                                NA's   :1       
#  laglogVisitsYear
#  Min.   : 5.930  
#  1st Qu.: 8.846  
#  Median : 9.849  
#  Mean   : 9.574  
#  3rd Qu.:10.529  
#  Max.   :11.161  
#  NA's   :12      

visits = visits[rowSums(is.na(visits)) == 0, ]
# 
str(visits)
# 'data.frame':	21855 obs. of  12 variables:
#  $ ParkName        : Factor w/ 305 levels "Abraham Lincoln Birthplace NHP",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ ParkType        : Factor w/ 8 levels "National Battlefield",..: 3 3 3 3 3 3 3 3 3 3 ...
#  $ Region          : Factor w/ 7 levels "Alaska","Intermountain ",..: 7 7 7 7 7 7 7 7 7 7 ...
#  $ State           : Factor w/ 53 levels "AK","AL","AR",..: 19 19 19 19 19 19 19 19 19 19 ...
#  $ Year            : int  2011 2011 2011 2011 2011 2011 2011 2011 2011 2011 ...
#  $ Month           : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ lat             : num  37.6 37.6 37.6 37.6 37.6 ...
#  $ long            : num  -85.7 -85.7 -85.7 -85.7 -85.7 ...
#  $ cost            : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ logVisits       : num  7.88 8.2 8.98 9.87 9.74 ...
#  $ laglogVisits    : num  8.32 7.88 8.2 8.98 9.87 ...
#  $ laglogVisitsYear: num  8.26 8.55 8.99 9.81 9.87 ...

visits$Month = as.factor(visits$Month)
str(visits)
# 'data.frame':	21855 obs. of  12 variables:
#  $ ParkName        : Factor w/ 305 levels "Abraham Lincoln Birthplace NHP",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ ParkType        : Factor w/ 8 levels "National Battlefield",..: 3 3 3 3 3 3 3 3 3 3 ...
#  $ Region          : Factor w/ 7 levels "Alaska","Intermountain ",..: 7 7 7 7 7 7 7 7 7 7 ...
#  $ State           : Factor w/ 53 levels "AK","AL","AR",..: 19 19 19 19 19 19 19 19 19 19 ...
#  $ Year            : int  2011 2011 2011 2011 2011 2011 2011 2011 2011 2011 ...
#  $ Month           : Factor w/ 12 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
#  $ lat             : num  37.6 37.6 37.6 37.6 37.6 ...
#  $ long            : num  -85.7 -85.7 -85.7 -85.7 -85.7 ...
#  $ cost            : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ logVisits       : num  7.88 8.2 8.98 9.87 9.74 ...
#  $ laglogVisits    : num  8.32 7.88 8.2 8.98 9.87 ...
#  $ laglogVisitsYear: num  8.26 8.55 8.99 9.81 9.87 ...

train=subset(visits, Year<2015)

test=subset(visits, Year>2014)

str(train)

str(test)

mod=lm(logVisits ~ laglogVisits, data=train)

summary(mod)
# 
# Call:
# lm(formula = logVisits ~ laglogVisits, data = train)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -12.5265  -0.2866   0.0413   0.3306  12.3876 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.659455   0.028923    22.8   <2e-16 ***
# laglogVisits 0.927945   0.003073   301.9   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8817 on 14557 degrees of freedom
# Multiple R-squared:  0.8623,	Adjusted R-squared:  0.8623 
# F-statistic: 9.117e+04 on 1 and 14557 DF,  p-value: < 2.2e-16

pred = predict(mod, newdata=test)

# Compute out-of-sample R^2
SSE = sum((pred - test$logVisits)^2)
SST = sum((mean(test$logVisits) - test$logVisits)^2)
R2 = 1 - SSE/SST
R2
# [1] 0.8973278

mod2=lm(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=train)

summary(mod2)


pred2 = predict(mod2, newdata=test)

# Compute out-of-sample R^2
SSE = sum((pred2 - test$logVisits)^2)
SST = sum((mean(test$logVisits) - test$logVisits)^2)
R2 = 1 - SSE/SST
R2
# [1] 0.9370909



# 
# Call:
# lm(formula = logVisits ~ laglogVisits + laglogVisitsYear + Year + 
#     Month + Region + ParkType + cost, data = train)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -12.2891  -0.2014   0.0058   0.2293  10.1600 
# 
# Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      -2.514727  10.909227  -0.231 0.817696    
# laglogVisits                      0.650702   0.005177 125.682  < 2e-16 ***
# laglogVisitsYear                  0.314173   0.005163  60.845  < 2e-16 ***
# Year                              0.001244   0.005421   0.230 0.818468    
# Month2                            0.174008   0.029701   5.859 4.77e-09 ***
# Month3                            0.422671   0.029859  14.156  < 2e-16 ***
# Month4                            0.276278   0.029842   9.258  < 2e-16 ***
# Month5                            0.397868   0.030025  13.251  < 2e-16 ***
# Month6                            0.272168   0.030113   9.038  < 2e-16 ***
# Month7                            0.255440   0.030234   8.449  < 2e-16 ***
# Month8                            0.027959   0.030201   0.926 0.354589    
# Month9                           -0.024103   0.030090  -0.801 0.423141    
# Month10                          -0.216415   0.029958  -7.224 5.30e-13 ***
# Month11                          -0.177488   0.029782  -5.960 2.59e-09 ***
# Month12                          -0.105784   0.029709  -3.561 0.000371 ***
# RegionIntermountain               0.207608   0.037177   5.584 2.39e-08 ***
# RegionMidwest                     0.207578   0.038276   5.423 5.95e-08 ***
# RegionNational Capital            0.205045   0.047474   4.319 1.58e-05 ***
# RegionNortheast                   0.234444   0.038089   6.155 7.70e-10 ***
# RegionPacific West                0.233219   0.037759   6.177 6.73e-10 ***
# RegionSoutheast                   0.249889   0.038567   6.479 9.50e-11 ***
# ParkTypeNational Historic Site   -0.048652   0.036199  -1.344 0.178960    
# ParkTypeNational Historical Park  0.029225   0.037831   0.772 0.439832    
# ParkTypeNational Memorial         0.028364   0.039655   0.715 0.474452    
# ParkTypeNational Monument        -0.026587   0.037258  -0.714 0.475489    
# ParkTypeNational Park             0.019296   0.039394   0.490 0.624276    
# ParkTypeNational Recreation Area  0.060879   0.045403   1.341 0.179980    
# ParkTypeNational Seashore         0.034273   0.048327   0.709 0.478218    
# cost                              0.002859   0.001028   2.780 0.005435 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7309 on 14530 degrees of freedom
# Multiple R-squared:  0.9056,	Adjusted R-squared:  0.9054 
# F-statistic:  4977 on 28 and 14530 DF,  p-value: < 2.2e-16


#now for regression tree
library(caTools)
library(rpart)
library(rpart.plot)
visitTree = rpart(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=train, cp=0.05)
prp(visitTree)

visitCART=predict(visitTree,newdata=test)

SSE = sum((visitCART - test$logVisits)^2)
SST = sum((mean(test$logVisits) - test$logVisits)^2)
R2 = 1 - SSE/SST
R2
# [1] 0.7858791

library(caret)
library(e1071)
set.seed(201)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.0001,0.005,0.0001)) 

# Perform the cross validation
train(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
visitTreeCV = rpart(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=train, cp = 0.0001)

#summary(visitTreeCV)



# Make predictions
PredictCV = predict(visitTreeCV, newdata = test)
SSE = sum((PredictCV - test$logVisits)^2)
SST = sum((mean(test$logVisits) - test$logVisits)^2)
R2 = 1 - SSE/SST
R2
# [1] 0.9369506



library(randomForest)
visitForest = randomForest(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=train)
PredictForest=predict(visitForest,newdata=test)
#table(test$Reverse,PredictForest)

SSE = sum((PredictForest - test$logVisits)^2)
SST = sum((mean(test$logVisits) - test$logVisits)^2)
R2 = 1 - SSE/SST
R2
# randomForest 4.6-12
# Type rfNews() to see new features/changes/bug fixes.
# 
# Attaching package: 'randomForest'
# 
# The following object is masked from 'package:ggplot2':
# 
#     margin
# 
# Warning message:
# package 'randomForest' was built under R version 3.4.1 
# [1] 0.9472945




























#table(test$logVisits < 10, visitCART)
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