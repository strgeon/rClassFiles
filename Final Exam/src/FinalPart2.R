# TODO: Add comment
# 
# Author: Scott
###############################################################################



setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Final Exam/data")
getwd()

bank = read.csv("bank.csv")

str(bank)
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Final Exam/data"
# 'data.frame':	5000 obs. of  21 variables:
#  $ age           : int  52 49 25 27 44 31 51 41 59 34 ...
#  $ job           : Factor w/ 12 levels "admin.","blue-collar",..: 1 2 2 1 1 2 2 2 6 10 ...
#  $ marital       : Factor w/ 4 levels "divorced","married",..: 3 1 3 3 2 2 1 2 2 2 ...
#  $ education     : Factor w/ 8 levels "basic.4y","basic.6y",..: 7 4 3 7 7 1 3 2 7 3 ...
#  $ default       : Factor w/ 2 levels "no","unknown": 2 1 1 1 1 1 1 1 1 1 ...
#  $ housing       : Factor w/ 3 levels "no","unknown",..: 2 1 3 1 3 1 3 1 3 1 ...
#  $ loan          : Factor w/ 3 levels "no","unknown",..: 2 3 3 1 1 1 1 1 1 1 ...
#  $ contact       : Factor w/ 2 levels "cellular","telephone": 1 2 1 2 1 2 1 2 1 1 ...
#  $ month         : Factor w/ 10 levels "apr","aug","dec",..: 2 7 4 9 2 4 4 7 2 8 ...
#  $ day_of_week   : Factor w/ 5 levels "fri","mon","thu",..: 5 2 5 4 5 3 4 4 5 4 ...
#  $ duration      : int  138 742 322 540 113 317 61 592 160 110 ...
#  $ campaign      : int  3 2 2 1 1 2 1 1 3 1 ...
#  $ pdays         : int  999 999 999 999 999 999 999 999 999 999 ...
#  $ previous      : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ poutcome      : Factor w/ 3 levels "failure","nonexistent",..: 2 2 2 2 2 2 2 2 2 2 ...
#  $ emp.var.rate  : num  1.4 1.1 1.4 -0.1 -2.9 1.4 1.4 1.1 1.4 -0.1 ...
#  $ cons.price.idx: num  93.4 94 93.9 93.8 92.2 ...
#  $ cons.conf.idx : num  -36.1 -36.4 -42.7 -40.4 -31.4 -42.7 -42.7 -36.4 -36.1 -42 ...
#  $ euribor3m     : num  4.964 4.857 4.963 4.86 0.879 ...
#  $ nr.employed   : num  5228 5191 5228 5196 5076 ...
#  $ y             : int  0 0 0 1 0 0 0 0 0 0 ...

mean(bank$age)
# [1] 39.5814

boxplot(bank$duration ~ bank$job)

tapply(bank$duration, bank$job, mean)
#        admin.   blue-collar  entrepreneur     housemaid    management       retired self-employed      services 
#      256.5754      284.4000      279.7680      303.2542      261.0000      285.8770      287.3608      269.6022 
#       student    technician    unemployed       unknown 
#      255.7519      239.9052      232.3053      248.7222 

cor(bank[c("emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")])
#                emp.var.rate cons.price.idx cons.conf.idx euribor3m nr.employed
# emp.var.rate      1.0000000      0.7808786    0.20449051 0.9732989  0.91412988
# cons.price.idx    0.7808786      1.0000000    0.07921100 0.7000698  0.54522242
# cons.conf.idx     0.2044905      0.0792110    1.00000000 0.2822054  0.09970857
# euribor3m         0.9732989      0.7000698    0.28220537 1.0000000  0.94798135
# nr.employed       0.9141299      0.5452224    0.09970857 0.9479813  1.00000000

set.seed(201)
library(caTools)
spl = sample.split(bank$y, 0.7)
# 
training = subset(bank, spl==TRUE)
testing = subset(bank, spl==FALSE)


log=glm(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx, data=training, family="binomial")

summary(log)
# 
# Call:
# glm(formula = y ~ age + job + marital + education + default + 
#     housing + loan + contact + month + day_of_week + campaign + 
#     pdays + previous + poutcome + emp.var.rate + cons.price.idx + 
#     cons.conf.idx, family = "binomial", data = training)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.0070  -0.4412  -0.3237  -0.2409   2.7433  
# 
# Coefficients: (1 not defined because of singularities)
#                                Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                  -1.142e+02  1.847e+01  -6.183 6.28e-10 ***
# age                           1.567e-02  7.227e-03   2.169 0.030118 *  
# jobblue-collar                1.579e-01  2.193e-01   0.720 0.471487    
# jobentrepreneur              -5.453e-01  4.039e-01  -1.350 0.176968    
# jobhousemaid                 -1.103e-01  4.798e-01  -0.230 0.818152    
# jobmanagement                -2.475e-02  2.469e-01  -0.100 0.920141    
# jobretired                   -3.763e-02  3.272e-01  -0.115 0.908424    
# jobself-employed              3.930e-01  3.177e-01   1.237 0.216089    
# jobservices                   2.249e-03  2.553e-01   0.009 0.992971    
# jobstudent                    4.922e-01  3.297e-01   1.493 0.135466    
# jobtechnician                -7.562e-02  2.188e-01  -0.346 0.729563    
# jobunemployed                -6.709e-01  4.378e-01  -1.533 0.125371    
# jobunknown                   -1.076e-01  7.446e-01  -0.145 0.885084    
# maritalmarried                3.128e-01  2.224e-01   1.406 0.159601    
# maritalsingle                 3.625e-01  2.457e-01   1.475 0.140111    
# maritalunknown                5.848e-01  1.205e+00   0.485 0.627556    
# educationbasic.6y             7.062e-03  3.202e-01   0.022 0.982405    
# educationbasic.9y             9.914e-03  2.530e-01   0.039 0.968744    
# educationhigh.school         -5.576e-02  2.588e-01  -0.215 0.829383    
# educationilliterate           1.458e+01  3.247e+02   0.045 0.964179    
# educationprofessional.course  5.903e-02  2.879e-01   0.205 0.837523    
# educationuniversity.degree    1.464e-01  2.589e-01   0.566 0.571647    
# educationunknown             -6.803e-01  3.950e-01  -1.722 0.085020 .  
# defaultunknown               -4.383e-01  1.973e-01  -2.221 0.026339 *  
# housingunknown                1.361e-01  3.763e-01   0.362 0.717616    
# housingyes                    4.583e-02  1.206e-01   0.380 0.703955    
# loanunknown                          NA         NA      NA       NA    
# loanyes                      -2.102e-01  1.740e-01  -1.208 0.226947    
# contacttelephone             -5.525e-01  1.928e-01  -2.866 0.004163 ** 
# monthaug                      8.172e-01  3.156e-01   2.590 0.009602 ** 
# monthdec                      7.238e-01  6.343e-01   1.141 0.253835    
# monthjul                      2.274e-01  2.784e-01   0.817 0.413883    
# monthjun                     -4.160e-01  2.733e-01  -1.522 0.128027    
# monthmar                      1.286e+00  3.396e-01   3.786 0.000153 ***
# monthmay                     -2.559e-01  2.138e-01  -1.197 0.231302    
# monthnov                     -4.158e-03  2.838e-01  -0.015 0.988310    
# monthoct                      3.807e-01  4.121e-01   0.924 0.355577    
# monthsep                      1.329e-01  4.356e-01   0.305 0.760293    
# day_of_weekmon               -4.157e-01  1.986e-01  -2.093 0.036335 *  
# day_of_weekthu               -1.294e-02  1.884e-01  -0.069 0.945251    
# day_of_weektue                1.172e-01  1.905e-01   0.615 0.538331    
# day_of_weekwed                1.773e-01  1.863e-01   0.952 0.341066    
# campaign                     -5.324e-02  3.203e-02  -1.662 0.096460 .  
# pdays                        -1.469e-03  8.815e-04  -1.667 0.095581 .  
# previous                     -6.776e-02  2.017e-01  -0.336 0.736952    
# poutcomenonexistent           8.804e-01  2.999e-01   2.936 0.003325 ** 
# poutcomesuccess               4.368e-01  8.555e-01   0.511 0.609598    
# emp.var.rate                 -8.078e-01  7.625e-02 -10.594  < 2e-16 ***
# cons.price.idx                1.196e+00  1.985e-01   6.024 1.70e-09 ***
# cons.conf.idx                -5.433e-04  1.549e-02  -0.035 0.972028    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 2544.5  on 3499  degrees of freedom
# Residual deviance: 2031.3  on 3451  degrees of freedom
# AIC: 2129.3
# 
# Number of Fisher Scoring iterations: 11

pred = predict(log, newdata=testing, type="response")
table(testing$y, pred < .5)
# Warning message:
# In predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type ==  :
#   prediction from a rank-deficient fit may be misleading
#    
#     FALSE TRUE
#   0    50 1273
#   1    44  133

summary(pred)


library(ROCR)


predictTest = predict(log, type="response", newdata=testing)
ROCRpredTest = prediction(predictTest, testing$y)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
# 

str(auc)

ROCRperf=performance(ROCRpredTest, "tpr","fpr")
plot(ROCRperf, colorize=TRUE)


table(testing$y, pred > .9)
#    
#     FALSE TRUE
#   0  1322    1
#   1   177    0

#    
#     FALSE TRUE
#   0     7 1316
#   1     0  177

#    
#     FALSE TRUE
#   0  1316    7
#   1   177    0


library(caret)
library(e1071)
set.seed(201)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.001,0.05,0.001)) 

# Perform the cross validation
train(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx, data=training, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )


# Create a new CART model
cart = rpart(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx, data=training, method = "class", cp = 0.016)

prp(cart)
#summary(visitTreeCV)

predictit = predict(cart, type="class", newdata=testing)
predTest = predict(cart, newdata=testing)[,2]

summary(predictit[,2])

table(testing$y, predTest< .5)
#    
#     FALSE TRUE
#   0    30 1293
#   1    37  140

# < table of extent 2 x 0 >
# Warning message:
# In Ops.factor(predictit, 0.5) : '<' not meaningful for factors

(37+1293)/(37+1293+30+140)
# [1] 0.8866667






