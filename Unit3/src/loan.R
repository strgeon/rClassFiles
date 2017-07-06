# TODO: Add comment
# 
# Author: Scott
###############################################################################


getwd()
setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit3/src")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit3/src"

loans=read.csv("loans.csv")
str(loans)
summary(loans)
# 'data.frame':	9578 obs. of  14 variables:
#  $ credit.policy    : int  1 1 1 1 1 1 1 1 1 1 ...
#  $ purpose          : Factor w/ 7 levels "all_other","credit_card",..: 3 2 3 3 2 2 3 1 5 3 ...
#  $ int.rate         : num  0.119 0.107 0.136 0.101 0.143 ...
#  $ installment      : num  829 228 367 162 103 ...
#  $ log.annual.inc   : num  11.4 11.1 10.4 11.4 11.3 ...
#  $ dti              : num  19.5 14.3 11.6 8.1 15 ...
#  $ fico             : int  737 707 682 712 667 727 667 722 682 707 ...
#  $ days.with.cr.line: num  5640 2760 4710 2700 4066 ...
#  $ revol.bal        : int  28854 33623 3511 33667 4740 50807 3839 24220 69909 5630 ...
#  $ revol.util       : num  52.1 76.7 25.6 73.2 39.5 51 76.8 68.6 51.1 23 ...
#  $ inq.last.6mths   : int  0 0 1 1 0 0 0 0 1 1 ...
#  $ delinq.2yrs      : int  0 0 0 0 1 0 0 0 0 0 ...
#  $ pub.rec          : int  0 0 0 0 0 0 1 0 0 0 ...
#  $ not.fully.paid   : int  0 0 0 0 0 0 1 1 0 0 ...
#  credit.policy                 purpose        int.rate       installment    
#  Min.   :0.000   all_other         :2331   Min.   :0.0600   Min.   : 15.67  
#  1st Qu.:1.000   credit_card       :1262   1st Qu.:0.1039   1st Qu.:163.77  
#  Median :1.000   debt_consolidation:3957   Median :0.1221   Median :268.95  
#  Mean   :0.805   educational       : 343   Mean   :0.1226   Mean   :319.09  
#  3rd Qu.:1.000   home_improvement  : 629   3rd Qu.:0.1407   3rd Qu.:432.76  
#  Max.   :1.000   major_purchase    : 437   Max.   :0.2164   Max.   :940.14  
#                  small_business    : 619                                    
#  log.annual.inc        dti              fico       days.with.cr.line
#  Min.   : 7.548   Min.   : 0.000   Min.   :612.0   Min.   :  179    
#  1st Qu.:10.558   1st Qu.: 7.213   1st Qu.:682.0   1st Qu.: 2820    
#  Median :10.928   Median :12.665   Median :707.0   Median : 4140    
#  Mean   :10.932   Mean   :12.607   Mean   :710.8   Mean   : 4562    
#  3rd Qu.:11.290   3rd Qu.:17.950   3rd Qu.:737.0   3rd Qu.: 5730    
#  Max.   :14.528   Max.   :29.960   Max.   :827.0   Max.   :17640    
#  NA's   :4                                         NA's   :29       
#    revol.bal         revol.util     inq.last.6mths    delinq.2yrs     
#  Min.   :      0   Min.   :  0.00   Min.   : 0.000   Min.   : 0.0000  
#  1st Qu.:   3187   1st Qu.: 22.70   1st Qu.: 0.000   1st Qu.: 0.0000  
#  Median :   8596   Median : 46.40   Median : 1.000   Median : 0.0000  
#  Mean   :  16914   Mean   : 46.87   Mean   : 1.572   Mean   : 0.1638  
#  3rd Qu.:  18250   3rd Qu.: 71.00   3rd Qu.: 2.000   3rd Qu.: 0.0000  
#  Max.   :1207359   Max.   :119.00   Max.   :33.000   Max.   :13.0000  
#                    NA's   :62       NA's   :29       NA's   :29       
#     pub.rec       not.fully.paid  
#  Min.   :0.0000   Min.   :0.0000  
#  1st Qu.:0.0000   1st Qu.:0.0000  
#  Median :0.0000   Median :0.0000  
#  Mean   :0.0621   Mean   :0.1601  
#  3rd Qu.:0.0000   3rd Qu.:0.0000  
#  Max.   :5.0000   Max.   :1.0000  
#  NA's   :29                       

paid=subset(loans,not.fully.paid == 0)
notpaid=subset(loans,not.fully.paid == 1)
nrow(notpaid)/(nrow(paid)+nrow(notpaid))
# [1] 0.1600543
table(loans$not.fully.paid)
# 
#    0    1 
# 8045 1533 

missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
# [1] 62

table(missing$not.fully.paid)
# 
#  0  1 
# 50 12 
#dim gives number of rows and columns
dim(missing)
names(loans)

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

imp_loans=read.csv("loans_imputed.csv")
str(loans)
str(imp_loans)
summary(loans)
summary(imp_loans)

loans=read.csv("loans_imputed.csv")

set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

summary(loans)

mod=glm(not.fully.paid ~ ., data=train,family="binomial")
summary(mod)
# 
# Call:
# glm(formula = not.fully.paid ~ ., family = "binomial", data = train)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.2049  -0.6205  -0.4951  -0.3606   2.6397  
# 
# Coefficients:
#                             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                9.187e+00  1.554e+00   5.910 3.42e-09 ***
# credit.policy             -3.368e-01  1.011e-01  -3.332 0.000861 ***
# purposecredit_card        -6.141e-01  1.344e-01  -4.568 4.93e-06 ***
# purposedebt_consolidation -3.212e-01  9.183e-02  -3.498 0.000469 ***
# purposeeducational         1.347e-01  1.753e-01   0.768 0.442201    
# purposehome_improvement    1.727e-01  1.480e-01   1.167 0.243135    
# purposemajor_purchase     -4.830e-01  2.009e-01  -2.404 0.016203 *  
# purposesmall_business      4.120e-01  1.419e-01   2.905 0.003678 ** 
# int.rate                   6.110e-01  2.085e+00   0.293 0.769446    
# installment                1.275e-03  2.092e-04   6.093 1.11e-09 ***
# log.annual.inc            -4.337e-01  7.148e-02  -6.067 1.30e-09 ***
# dti                        4.638e-03  5.502e-03   0.843 0.399288    
# fico                      -9.317e-03  1.710e-03  -5.448 5.08e-08 ***
# days.with.cr.line          2.371e-06  1.588e-05   0.149 0.881343    
# revol.bal                  3.085e-06  1.168e-06   2.641 0.008273 ** 
# revol.util                 1.839e-03  1.535e-03   1.199 0.230722    
# inq.last.6mths             8.437e-02  1.600e-02   5.275 1.33e-07 ***
# delinq.2yrs               -8.320e-02  6.561e-02  -1.268 0.204762    
# pub.rec                    3.300e-01  1.139e-01   2.898 0.003756 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 5896.6  on 6704  degrees of freedom
# Residual deviance: 5485.2  on 6686  degrees of freedom
# AIC: 5523.2
# 
# Number of Fisher Scoring iterations: 5


-9.317e-03*(700) - -9.317e-03*(710)
exp(-9.317e-03*(700))/exp(-9.317e-03*(710))

predicted.risk = predict(mod, newdata=test, type="response")
table(test$not.fully.paid,predicted.risk >.5)
#    
#     FALSE TRUE
#   0  2400   13
#   1   457    3
2403/(2403+470)
2413/(2403+470)

ROCRpred = prediction(predicted.risk,test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

mod2=glm(not.fully.paid ~ int.rate, data=train,family="binomial")
summary(mod2)
# 
# Call:
# glm(formula = not.fully.paid ~ int.rate, family = "binomial", 
#     data = train)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.0547  -0.6271  -0.5442  -0.4361   2.2914  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -3.6726     0.1688  -21.76   <2e-16 ***
# int.rate     15.9214     1.2702   12.54   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 5896.6  on 6704  degrees of freedom
# Residual deviance: 5734.8  on 6703  degrees of freedom
# AIC: 5738.8
# 
# Number of Fisher Scoring iterations: 4

set.seed(144)
testpred = predict(mod2,newdata=test,type="response")
table(test$not.fully.paid,testpred >.5)
#    
#     FALSE
#   0  2413
#   1   460
max(testpred)
# [1] 0.426624

ROCRpred2 = prediction(testpred,test$not.fully.paid)
as.numeric(performance(ROCRpred2, "auc")@y.values)
# [1] 0.6239081

10*(exp(.06*3))
# [1] 11.97217
c=10
r=.06
t=3

c * (exp(r*t) - 1)
# [1] 1.972174

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

r=max(test$int.rate)
# [1] 0.2121
c * (exp(r*t) - 1)
# [1] 8.894769

set.seed(144)
highInterest=subset(test,int.rate>.15)
nrow(highInterest)
# [1] 437

r=mean(highInterest$int.rate)
# 
r
# [1] 0.1650927
c=1
c * (exp(r*t) - 1)
# [1] 0.6409544

set.seed(144)
nrow(subset(highInterest, not.fully.paid == 1))
# [1] 110

# [1] 110
nrow(subset(highInterest, not.fully.paid == 1))/nrow(highInterest)
# [1] 0.2517162
mean(exp(highInterest$int.rate*t)-1)
# [1] 0.6422176
highInterest$interest = exp(highInterest$int.rate*t)-1
str(highInterest)
mean(highInterest$interest)
# [1] 0.6422176

sum(highInterest$profit)/nrow(highInterest)
str(highInterest)
set.seed(144)
mod=glm(not.fully.paid ~ ., data=highInterest,family="binomial")
#summary(mod)
highInterest$predicted.risk = predict(mod,newdata=highInterest,type="response")
set.seed(144)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
cutoff
# [1] 2.306521e-12

# Warning message:
# glm.fit: algorithm did not converge 
# [1] 2.900701e-12

# Warning message:
# glm.fit: algorithm did not converge 
# 
# Call:
# glm(formula = not.fully.paid ~ ., family = "binomial", data = highInterest)
# 
# Deviance Residuals: 
#        Min          1Q      Median          3Q         Max  
# -2.409e-06  -2.409e-06  -2.409e-06   2.409e-06   2.409e-06  
# 
# Coefficients:
#                             Estimate Std. Error z value Pr(>|z|)
# (Intercept)               -2.657e+01  2.262e+06       0        1
# credit.policy              3.092e-08  4.914e+04       0        1
# purposecredit_card        -5.581e-08  7.247e+04       0        1
# purposedebt_consolidation -2.454e-09  5.090e+04       0        1
# purposeeducational        -1.787e-07  1.032e+05       0        1
# purposehome_improvement   -2.235e-08  9.342e+04       0        1
# purposemajor_purchase     -2.631e-08  1.136e+05       0        1
# purposesmall_business     -4.947e-08  7.199e+04       0        1
# int.rate                  -3.410e-04  5.443e+07       0        1
# installment               -7.687e-11  9.487e+01       0        1
# log.annual.inc            -1.403e-08  3.888e+04       0        1
# dti                       -2.664e-10  2.590e+03       0        1
# fico                       5.035e-10  1.015e+03       0        1
# days.with.cr.line          3.097e-12  9.002e+00       0        1
# revol.bal                  2.195e-13  7.938e-01       0        1
# revol.util                 5.976e-10  7.263e+02       0        1
# inq.last.6mths            -5.646e-09  9.574e+03       0        1
# delinq.2yrs                5.839e-08  3.430e+04       0        1
# pub.rec                    6.994e-08  5.299e+04       0        1
# profit                     2.675e-05  5.974e+05       0        1
# interest                   4.107e-05  1.076e+07       0        1
# predicted.risk             5.313e+01  9.855e+05       0        1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 4.9312e+02  on 436  degrees of freedom
# Residual deviance: 2.5353e-09  on 415  degrees of freedom
# AIC: 44
# 
# Number of Fisher Scoring iterations: 25
# 
# [1] 2.900701e-12

selectedLoans = subset(highInterest,predicted.risk < 2.306521e-12)
nrow(selectedLoans)
# [1] 15

# [1] 100

# [1] 105
sum(selectedLoans$profit)
# [1] 71.11488

table(selectedLoans$not.fully.paid)
# 
#   0 
# 100 

# Error in table(selectLoans$not.fully.paid) : 
#   object 'selectLoans' not found

sum(selectedLoans$profit)
# [1] 71.11488

# [1] 0

selectedLoans = subset(highInterest, predicted.risk <= cutoff)
table(selectedLoans$not.fully.paid)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)