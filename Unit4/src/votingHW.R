# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit4/data"

gerber=read.csv("gerber.csv")
str(gerber)
# 'data.frame':	344084 obs. of  8 variables:
#  $ sex      : int  0 1 1 1 0 1 0 0 1 0 ...
#  $ yob      : int  1941 1947 1982 1950 1951 1959 1956 1981 1968 1967 ...
#  $ voting   : int  0 0 1 1 1 1 1 0 0 0 ...
#  $ hawthorne: int  0 0 1 1 1 0 0 0 0 0 ...
#  $ civicduty: int  1 1 0 0 0 0 0 0 0 0 ...
#  $ neighbors: int  0 0 0 0 0 0 0 0 0 0 ...
#  $ self     : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ control  : int  0 0 0 0 0 1 1 1 1 1 ...

summary(gerber)
#       sex              yob           voting         hawthorne    
#  Min.   :0.0000   Min.   :1900   Min.   :0.0000   Min.   :0.000  
#  1st Qu.:0.0000   1st Qu.:1947   1st Qu.:0.0000   1st Qu.:0.000  
#  Median :0.0000   Median :1956   Median :0.0000   Median :0.000  
#  Mean   :0.4993   Mean   :1956   Mean   :0.3159   Mean   :0.111  
#  3rd Qu.:1.0000   3rd Qu.:1965   3rd Qu.:1.0000   3rd Qu.:0.000  
#  Max.   :1.0000   Max.   :1986   Max.   :1.0000   Max.   :1.000  
#    civicduty        neighbors          self           control      
#  Min.   :0.0000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000  
#  1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000  
#  Median :0.0000   Median :0.000   Median :0.0000   Median :1.0000  
#  Mean   :0.1111   Mean   :0.111   Mean   :0.1111   Mean   :0.5558  
#  3rd Qu.:0.0000   3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.:1.0000  
#  Max.   :1.0000   Max.   :1.000   Max.   :1.0000   Max.   :1.0000  

table(gerber$voting)
# 
#      0      1 
# 235388 108696 

table(gerber$voting,gerber$civicduty)
#    
#          0      1
#   0 209191  26197
#   1  96675  12021

nrow(subset(gerber,civicduty==1 & voting==1))/nrow(subset(gerber,civicduty==1))
# [1] 0.3145377

nrow(subset(gerber,hawthorne==1 & voting==1))/nrow(subset(gerber,hawthorne==1))
# [1] 0.3223746

nrow(subset(gerber,neighbors==1 & voting==1))/nrow(subset(gerber,neighbors==1))
# [1] 0.3779482

nrow(subset(gerber,self==1 & voting==1))/nrow(subset(gerber,self==1))
# [1] 0.3451515

nrow(subset(gerber,control==1 & voting==1))/nrow(subset(gerber,control==1))
# [1] 0.2966383

#another way to do it
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)
#         0         1 
# 0.3160698 0.3145377 
#         0         1 
# 0.3150909 0.3223746 
#         0         1 
# 0.3122446 0.3451515 
#         0         1 
# 0.3081505 0.3779482 

gerblog=glm(voting ~ civicduty + hawthorne + neighbors + self, data=gerber, family=binomial)
summary(gerblog)
# 
# Call:
# glm(formula = voting ~ civicduty + hawthorne + neighbors + self, 
#     family = binomial, data = gerber)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -0.9744  -0.8691  -0.8389   1.4586   1.5590  
# 
# Coefficients:
#              Estimate Std. Error  z value Pr(>|z|)    
# (Intercept) -0.863358   0.005006 -172.459  < 2e-16 ***
# civicduty    0.084368   0.012100    6.972 3.12e-12 ***
# hawthorne    0.120477   0.012037   10.009  < 2e-16 ***
# neighbors    0.365092   0.011679   31.260  < 2e-16 ***
# self         0.222937   0.011867   18.786  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 429238  on 344083  degrees of freedom
# Residual deviance: 428090  on 344079  degrees of freedom
# AIC: 428100
# 
# Number of Fisher Scoring iterations: 4

pred=predict(gerblog,type="response")
table(gerber$voting,pred >.5)
#    
#      FALSE
#   0 235388
#   1 108696
table(gerber$voting,pred >.3)
#    
#      FALSE   TRUE
#   0 134513 100875
#   1  56730  51966

235388/(235388+108696)
# [1] 0.6841004
(134513+51966)/(134513+51966+56730+100875)
# [1] 0.5419578

library(ROCR)
ROCRpred = prediction(pred,gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)
# [1] 0.5308461

# now building a tree as the logistic regression wasn't very good
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
 
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits=6)

CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits=6)

.34-.296638
# [1] 0.043362


.334176-.290456
# [1] 0.04372

.345818-.302795
# [1] 0.043023

gerblog2=glm(voting ~ control + sex, data=gerber, family=binomial)
summary(gerblog2)
# 
# Call:
# glm(formula = voting ~ control + sex, family = binomial, data = gerber)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -0.9220  -0.9012  -0.8290   1.4564   1.5717  
# 
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.635538   0.006511 -97.616  < 2e-16 ***
# control     -0.200142   0.007364 -27.179  < 2e-16 ***
# sex         -0.055791   0.007343  -7.597 3.02e-14 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 429238  on 344083  degrees of freedom
# Residual deviance: 428443  on 344081  degrees of freedom
# AIC: 428449
# 
# Number of Fisher Scoring iterations: 4

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(gerblog2, newdata=Possibilities, type="response")
#         1         2         3         4 
# 0.3462559 0.3024455 0.3337375 0.2908065 

.302795-.2908065
# [1] 0.0119885
.290456-.2908065
# [1] -0.0003505

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
# 
# Call:
# glm(formula = voting ~ sex + control + sex:control, family = "binomial", 
#     data = gerber)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -0.9213  -0.9019  -0.8284   1.4573   1.5724  
# 
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.637471   0.007603 -83.843  < 2e-16 ***
# sex         -0.051888   0.010801  -4.804 1.55e-06 ***
# control     -0.196553   0.010356 -18.980  < 2e-16 ***
# sex:control -0.007259   0.014729  -0.493    0.622    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 429238  on 344083  degrees of freedom
# Residual deviance: 428442  on 344080  degrees of freedom
# AIC: 428450
# 
# Number of Fisher Scoring iterations: 4

predict(LogModel2, newdata=Possibilities, type="response")
#         1         2         3         4 
# 0.3458183 0.3027947 0.3341757 0.2904558 


# 
0.2904558-.290456
# [1] -2e-07
