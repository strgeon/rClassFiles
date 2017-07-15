# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data"

data(state)
statedata = data.frame(state.x77)

str(statedata)
# 'data.frame':	50 obs. of  8 variables:
#  $ Population: num  3615 365 2212 2110 21198 ...
#  $ Income    : num  3624 6315 4530 3378 5114 ...
#  $ Illiteracy: num  2.1 1.5 1.8 1.9 1.1 0.7 1.1 0.9 1.3 2 ...
#  $ Life.Exp  : num  69 69.3 70.5 70.7 71.7 ...
#  $ Murder    : num  15.1 11.3 7.8 10.1 10.3 6.8 3.1 6.2 10.7 13.9 ...
#  $ HS.Grad   : num  41.3 66.7 58.1 39.9 62.6 63.9 56 54.6 52.6 40.6 ...
#  $ Frost     : num  20 152 15 65 20 166 139 103 11 60 ...
#  $ Area      : num  50708 566432 113417 51945 156361 ...

lin=lm(Life.Exp ~ .,data = statedata) 
#Population + Income + Illiteracy + Murder + HS.Grad + Frost, Area
summary(lin)
# 
# Call:
# lm(formula = Life.Exp ~ ., data = statedata)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.48895 -0.51232 -0.02747  0.57002  1.49447 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.094e+01  1.748e+00  40.586  < 2e-16 ***
# Population   5.180e-05  2.919e-05   1.775   0.0832 .  
# Income      -2.180e-05  2.444e-04  -0.089   0.9293    
# Illiteracy   3.382e-02  3.663e-01   0.092   0.9269    
# Murder      -3.011e-01  4.662e-02  -6.459 8.68e-08 ***
# HS.Grad      4.893e-02  2.332e-02   2.098   0.0420 *  
# Frost       -5.735e-03  3.143e-03  -1.825   0.0752 .  
# Area        -7.383e-08  1.668e-06  -0.044   0.9649    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7448 on 42 degrees of freedom
# Multiple R-squared:  0.7362,	Adjusted R-squared:  0.6922 
# F-statistic: 16.74 on 7 and 42 DF,  p-value: 2.534e-10

pred=predict(lin)
sse=sum((pred - statedata$Life.Exp)^2)
sse
# [1] 23.29714


lin2=lm(Life.Exp ~ Population + Murder + Frost + HS.Grad,data = statedata) 
summary(lin2)
# 
# Call:
# lm(formula = Life.Exp ~ Population + Murder + Frost + HS.Grad, 
#     data = statedata)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.47095 -0.53464 -0.03701  0.57621  1.50683 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.103e+01  9.529e-01  74.542  < 2e-16 ***
# Population   5.014e-05  2.512e-05   1.996  0.05201 .  
# Murder      -3.001e-01  3.661e-02  -8.199 1.77e-10 ***
# Frost       -5.943e-03  2.421e-03  -2.455  0.01802 *  
# HS.Grad      4.658e-02  1.483e-02   3.142  0.00297 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7197 on 45 degrees of freedom
# Multiple R-squared:  0.736,	Adjusted R-squared:  0.7126 
# F-statistic: 31.37 on 4 and 45 DF,  p-value: 1.696e-12

pred2=predict(lin2)
sse2=sum((pred2 - statedata$Life.Exp)^2)
sse2
# [1] 23.30804
# alternate way to calc
SSE3 = sum(lin2$residuals^2)
SSE3
# [1] 23.30804
cor(statedata$Life.Exp, statedata$Income)
cor(statedata$Life.Exp, statedata$Illiteracy)
cor(statedata$Life.Exp, statedata$Area)
# [1] 0.3402553
# [1] -0.5884779
# [1] -0.1073319

#now regression tree
library(rpart)
library(rpart.plot)
crta = rpart(Life.Exp ~ .,data = statedata)
prp(crta)

pred=predict(crta, data=statedata)
SSE4 = sum(crta$residuals^2)
SSE4
# [1] 0
SSE4=sum((pred - statedata$Life.Exp)^2)
SSE4
# [1] 28.99848

crtb = rpart(Life.Exp ~ .,data = statedata, minbucket=5)
prp(crtb)

pred=predict(crtb, data=statedata)
SSE5=sum((pred - statedata$Life.Exp)^2)
SSE5
# [1] 23.64283

crtc = rpart(Life.Exp ~ Area,data = statedata, minbucket=1)
prp(crtc)

pred6=predict(crtc, data=statedata)
SSE6=sum((pred6 - statedata$Life.Exp)^2)
SSE6
# [1] 9.312442

#experiement
crtd = rpart(Life.Exp ~ Area,data = statedata)
prp(crtd)

pred7=predict(crtd, data=statedata)
SSE7=sum((pred7 - statedata$Life.Exp)^2)
SSE7
# [1] 44.26817

#cross validation
library("caret")
library(e1071)

set.seed(111)
tr.control= trainControl(method="cv",number=10)
cartGrid =expand.grid(.cp=seq(0.01,0.5,0.01))

tr=train(Life.Exp ~ ., data=statedata, method="rpart", trControl=tr.control, tuneGrid=cartGrid)
tr
# Warning message:
# In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
#   There were missing values in resampled performance measures.
# CART 
# 
# 50 samples
#  7 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 45, 45, 46, 45, 45, 43, ... 
# Resampling results across tuning parameters:
# 
# RMSE was used to select the optimal model using  the smallest value.
# The final value used for the model was cp = 0.12.

set.seed(111)
crte = rpart(Life.Exp ~ ., data=statedata, cp=.012)
pred8=predict(crte,data=statedata)
prp(crte)
pred8=predict(crte)

SSE8=sum((pred8 - statedata$Life.Exp)^2)
SSE8
# [1] 28.99848

# [1] 28.99848
#my answer didn't match theirs...


set.seed(111)
tr.control= trainControl(method="cv",number=10)
cartGrid =expand.grid(.cp=seq(0.01,0.5,0.01))

tr=train(Life.Exp ~ Area, data=statedata, method="rpart", trControl=tr.control, tuneGrid=cartGrid)
tr
# Warning message:
# In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
#   There were missing values in resampled performance measures.
# CART 
# 
# 50 samples
#  1 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 45, 45, 46, 45, 45, 43, ... 
# Resampling results across tuning parameters:
# 

# RMSE was used to select the optimal model using  the smallest value.
# The final value used for the model was cp = 0.02.

crtf = rpart(Life.Exp ~ Area, data=statedata, cp=.02)
pred9=predict(crtf,data=statedata)
prp(crtf)

pred9=predict(crtf)
SSE9=sum((pred9 - statedata$Life.Exp)^2)
SSE9
# [1] 44.26817
