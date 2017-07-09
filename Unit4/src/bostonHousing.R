# TODO: Add comment
# 
# Author: Scott
###############################################################################


getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge"

setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data"

boston=read.csv("boston.csv")
str(boston)
# 'data.frame':	506 obs. of  16 variables:
#  $ TOWN   : Factor w/ 92 levels "Arlington","Ashland",..: 54 77 77 46 46 46 69 69 69 69 ...
#  $ TRACT  : int  2011 2021 2022 2031 2032 2033 2041 2042 2043 2044 ...
#  $ LON    : num  -71 -71 -70.9 -70.9 -70.9 ...
#  $ LAT    : num  42.3 42.3 42.3 42.3 42.3 ...
#  $ MEDV   : num  24 21.6 34.7 33.4 36.2 28.7 22.9 22.1 16.5 18.9 ...
#  $ CRIM   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
#  $ ZN     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
#  $ INDUS  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
#  $ CHAS   : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ NOX    : num  0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
#  $ RM     : num  6.58 6.42 7.18 7 7.15 ...
#  $ AGE    : num  65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
#  $ DIS    : num  4.09 4.97 4.97 6.06 6.06 ...
#  $ RAD    : int  1 2 2 3 3 3 5 5 5 5 ...
#  $ TAX    : int  296 242 242 222 222 222 311 311 311 311 ...
#  $ PTRATIO: num  15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...

plot(boston$LON,boston$LAT)
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],col="blue", pch=19)
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red", pch=19)
summary(boston$NOX)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.3850  0.4490  0.5380  0.5547  0.6240  0.8710 

points(boston$LON[boston$NOX>=.55],boston$LAT[boston$NOX>=.55],col="green", pch=19)

plot(boston$LON,boston$LAT)

points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red", pch=19)

latlonlm = lm(MEDV ~ LAT + LON, data=boston)
summary(latlonlm)
# 
# Call:
# lm(formula = MEDV ~ LAT + LON, data = boston)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -16.460  -5.590  -1.299   3.695  28.129 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -3178.472    484.937  -6.554 1.39e-10 ***
# LAT             8.046      6.327   1.272    0.204    
# LON           -40.268      5.184  -7.768 4.50e-14 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 8.693 on 503 degrees of freedom
# Multiple R-squared:  0.1072,	Adjusted R-squared:  0.1036 
# F-statistic: 30.19 on 2 and 503 DF,  p-value: 4.159e-13

points(boston$LON[latlonlm$fitted.values >= 21.2],boston$LAT[latlonlm$fitted.values>=21.2],col="blue", pch="$")
# this last plot shows that the linear regression model doesn't work so hot

library(rpart)
library(rpart.plot)
latlontree = rpart(MEDV ~ LAT + LON, data=boston)
prp(latlontree)

#using the same two commands to build the plot then overlay the tree findings
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red", pch=19)
fitted.values=predict(latlontree)
points(boston$LON[fitted.values >= 21.2],boston$LAT[fitted.values>=21.2],col="blue", pch="$")

#try less splits by using minbucked=50 and then use the other type of plot
latlontree = rpart(MEDV ~ LAT + LON, data=boston,minbucket=50)
plot(latlontree)
text(latlontree)

#plotting ablines based on the values in the tree
plot(boston$LON,boston$LAT)
abline(v=-71.07)
abline(h=42.17)
abline(h=42.21)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red", pch=19)

library(caTools)
set.seed(123)
split=sample.split(boston$MEDV, SplitRatio=0.7)
train=subset(boston,split==TRUE)
test=subset(boston,split==FALSE)

str(boston)
# 'data.frame':	506 obs. of  16 variables:
#  $ TOWN   : Factor w/ 92 levels "Arlington","Ashland",..: 54 77 77 46 46 46 69 69 69 69 ...
#  $ TRACT  : int  2011 2021 2022 2031 2032 2033 2041 2042 2043 2044 ...
#  $ LON    : num  -71 -71 -70.9 -70.9 -70.9 ...
#  $ LAT    : num  42.3 42.3 42.3 42.3 42.3 ...
#  $ MEDV   : num  24 21.6 34.7 33.4 36.2 28.7 22.9 22.1 16.5 18.9 ...
#  $ CRIM   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
#  $ ZN     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
#  $ INDUS  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
#  $ CHAS   : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ NOX    : num  0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
#  $ RM     : num  6.58 6.42 7.18 7 7.15 ...
#  $ AGE    : num  65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
#  $ DIS    : num  4.09 4.97 4.97 6.06 6.06 ...
#  $ RAD    : int  1 2 2 3 3 3 5 5 5 5 ...
#  $ TAX    : int  296 242 242 222 222 222 311 311 311 311 ...
#  $ PTRATIO: num  15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...

linreg=lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
summary(linreg)
# 
# Call:
# lm(formula = MEDV ~ LAT + LON + CRIM + ZN + INDUS + NOX + RM + 
#     AGE + DIS + RAD + TAX + PTRATIO, data = train)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -14.676  -2.825  -0.765   1.708  36.799 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -3.813e+02  4.362e+02  -0.874 0.382703    
# LAT          1.156e+00  5.224e+00   0.221 0.824981    
# LON         -5.032e+00  4.739e+00  -1.062 0.289059    
# CRIM        -1.844e-01  4.417e-02  -4.175 3.76e-05 ***
# ZN           3.165e-02  1.889e-02   1.675 0.094834 .  
# INDUS       -3.045e-02  8.513e-02  -0.358 0.720749    
# NOX         -2.060e+01  5.433e+00  -3.791 0.000176 ***
# RM           6.321e+00  4.857e-01  13.015  < 2e-16 ***
# AGE         -4.132e-02  1.793e-02  -2.305 0.021751 *  
# DIS         -1.556e+00  2.859e-01  -5.442 9.92e-08 ***
# RAD          2.628e-01  9.764e-02   2.691 0.007463 ** 
# TAX         -1.293e-02  5.435e-03  -2.379 0.017894 *  
# PTRATIO     -1.006e+00  1.949e-01  -5.160 4.14e-07 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5.632 on 351 degrees of freedom
# Multiple R-squared:  0.6595,	Adjusted R-squared:  0.6479 
# F-statistic: 56.66 on 12 and 351 DF,  p-value: < 2.2e-16

linreg.pred=predict(linreg, newdata=test)
linreg.sse=sum((linreg.pred - test$MEDV)^2)
linreg.sse
# [1] 3202.652

tree=rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(tree)
tree.pred=predict(tree, newdata=test)
tree.sse=sum((tree.pred - test$MEDV)^2)
tree.sse
# [1] 4328.988

#shows that the total of the standard errors is higher than regression, so is not as good a model as linear regression

#When you're actually using cp in your R code, you don't need to think exactly what it means-- just
#that small numbers of cp encourage large trees, and large values of cp encourage small trees.
		
library("caret")
library(e1071)

tr.control= trainControl(method="cv",number=10)
cp.grid = expand.grid(.cp=(0:10)*.001)
cp.grid
#      .cp
# 1  0.000
# 2  0.001
# 3  0.002
# 4  0.003
# 5  0.004
# 6  0.005
# 7  0.006
# 8  0.007
# 9  0.008
# 10 0.009
# 11 0.010

tr=train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, 
		data=train, method="rpart", trControl=tr.control, tuneGrid=cp.grid)

tr
# CART 
# 
# 364 samples
#  13 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 328, 328, 328, 326, 328, 327, ... 
# Resampling results across tuning parameters:
# 
#   cp     RMSE      Rsquared 
#   0.000  4.822583  0.7421816
#   0.001  4.823609  0.7406824
#   0.002  4.867666  0.7385670
#   0.003  4.859287  0.7384202
#   0.004  4.879579  0.7340200
#   0.005  4.885134  0.7333741
#   0.006  4.961371  0.7245878
#   0.007  4.951231  0.7262725
#   0.008  4.924076  0.7283912
#   0.009  4.971871  0.7237314
#   0.010  4.971871  0.7237314
# 
# RMSE was used to select the optimal model using  the smallest value.
# The final value used for the model was cp = 0.

		
best.tree=tr$finalModel
prp(best.tree)

best.tree.pred=predict(best.tree, newdata=test)
best.tree.sse=sum((best.tree.pred - test$MEDV)^2)
best.tree.sse
# [1] 3660.149

