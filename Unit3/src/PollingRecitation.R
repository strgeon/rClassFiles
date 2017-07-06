# TODO: Add comment
# 
# Author: Scott
###############################################################################

polling=read.csv("PollingData.csv")
str(polling)
# 'data.frame':	145 obs. of  7 variables:
#  $ State     : Factor w/ 50 levels "Alabama","Alaska",..: 1 1 2 2 3 3 3 4 4 4 ...
#  $ Year      : int  2004 2008 2004 2008 2004 2008 2012 2004 2008 2012 ...
#  $ Rasmussen : int  11 21 NA 16 5 5 8 7 10 NA ...
#  $ SurveyUSA : int  18 25 NA NA 15 NA NA 5 NA NA ...
#  $ DiffCount : int  5 5 1 6 8 9 4 8 5 2 ...
#  $ PropR     : num  1 1 1 1 1 ...
#  $ Republican: int  1 1 1 1 1 1 1 1 1 1 ...

table(polling$Year)
# 
# 2004 2008 2012 
#   50   50   45 
summary(polling)
#          State          Year        Rasmussen          SurveyUSA       
#  Arizona    :  3   Min.   :2004   Min.   :-41.0000   Min.   :-33.0000  
#  Arkansas   :  3   1st Qu.:2004   1st Qu.: -8.0000   1st Qu.:-11.7500  
#  California :  3   Median :2008   Median :  1.0000   Median : -2.0000  
#  Colorado   :  3   Mean   :2008   Mean   :  0.0404   Mean   : -0.8243  
#  Connecticut:  3   3rd Qu.:2012   3rd Qu.:  8.5000   3rd Qu.:  8.0000  
#  Florida    :  3   Max.   :2012   Max.   : 39.0000   Max.   : 30.0000  
#  (Other)    :127                  NA's   :46         NA's   :71        
#    DiffCount           PropR          Republican    
#  Min.   :-19.000   Min.   :0.0000   Min.   :0.0000  
#  1st Qu.: -6.000   1st Qu.:0.0000   1st Qu.:0.0000  
#  Median :  1.000   Median :0.6250   Median :1.0000  
#  Mean   : -1.269   Mean   :0.5259   Mean   :0.5103  
#  3rd Qu.:  4.000   3rd Qu.:1.0000   3rd Qu.:1.0000  
#  Max.   : 11.000   Max.   :1.0000   Max.   :1.0000  
#                                                     

#install.packages("mice, repos='http://cran.us.r-project.org'")
library("mice")

#imputed values for the N/As
simple = polling[c("Rasmussen", "SurveyUSA","PropR","DiffCount")]
set.seed(144)

imputed = complete(mice(simple))
# 
#  iter imp variable
#   1   1  Rasmussen  SurveyUSA
#   1   2  Rasmussen  SurveyUSA
#   1   3  Rasmussen  SurveyUSA
#   1   4  Rasmussen  SurveyUSA
#   1   5  Rasmussen  SurveyUSA
#   2   1  Rasmussen  SurveyUSA
#   2   2  Rasmussen  SurveyUSA
#   2   3  Rasmussen  SurveyUSA
#   2   4  Rasmussen  SurveyUSA
#   2   5  Rasmussen  SurveyUSA
#   3   1  Rasmussen  SurveyUSA
#   3   2  Rasmussen  SurveyUSA
#   3   3  Rasmussen  SurveyUSA
#   3   4  Rasmussen  SurveyUSA
#   3   5  Rasmussen  SurveyUSA
#   4   1  Rasmussen  SurveyUSA
#   4   2  Rasmussen  SurveyUSA
#   4   3  Rasmussen  SurveyUSA
#   4   4  Rasmussen  SurveyUSA
#   4   5  Rasmussen  SurveyUSA
#   5   1  Rasmussen  SurveyUSA
#   5   2  Rasmussen  SurveyUSA
#   5   3  Rasmussen  SurveyUSA
#   5   4  Rasmussen  SurveyUSA
#   5   5  Rasmussen  SurveyUSA

#values only updated in imputed dataframe
summary(simple)
summary(imputed)
summary(polling)

#Reassign the imputed variables back to the original dataframe
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA= imputed$SurveyUSA

Train=subset(polling,Year == 2004 | Year == 2008)
Test=subset(polling,Year==2012)

table(Train$Republican)
# 
#  0  1 
# 47 53 

table(sign(Train$Rasmussen))
# rerun after changing data model
# -1  0  1 
# 42  2 56 

# -1 are democrats, +1 are republicans, 0 is a tie
# sign changes all negative values to -1, all positive to 1
# -1  0  1 
# 42  3 55 

#didn't match the lecture, so having to load the imputed dataset
polling2=read.csv("PollingData_Imputed.csv")
summary(polling2)
Train=subset(polling2,Year == 2004 | Year == 2008)
Test=subset(polling2,Year==2012)

#compare rasmussen against actuals to create a baseline model
table(Train$Republican,sign(Train$Rasmussen))
#    
#     -1  0  1
#   0 42  1  4
#   1  0  1 52

#So in this table, the rows are the true outcome --
#1 is for Republican, 0 is for Democrat --
#and the columns are the smart baseline predictions, -1, 0,
#or 1.
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])
#            Rasmussen SurveyUSA     PropR DiffCount Republican
# Rasmussen  1.0000000 0.9365837 0.8431180 0.5109169  0.7929252
# SurveyUSA  0.9365837 1.0000000 0.8616478 0.5222585  0.8101645
# PropR      0.8431180 0.8616478 1.0000000 0.8273785  0.9484204
# DiffCount  0.5109169 0.5222585 0.8273785 1.0000000  0.8092777
# Republican 0.7929252 0.8101645 0.9484204 0.8092777  1.0000000

#logistic regression
mod1=glm(Republican ~ PropR, data= Train, family="binomial")
summary(mod1)
# 
# Call:
# glm(formula = Republican ~ PropR, family = "binomial", data = Train)
# 
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -2.22880  -0.06541   0.10260   0.10260   1.37392  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -6.146      1.977  -3.108 0.001882 ** 
# PropR         11.390      3.153   3.613 0.000303 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 138.269  on 99  degrees of freedom
# Residual deviance:  15.772  on 98  degrees of freedom
# AIC: 19.772
# 
# Number of Fisher Scoring iterations: 8

pred1=predict(mod1, type="response")
summary(pred1)

table(Train$Republican, pred1 >= .5 )
#    basically the same as the baseline model
#     FALSE TRUE
#   0    45    2
#   1     2   51

mod2=glm(Republican ~ SurveyUSA + DiffCount, data= Train, family="binomial")
summary(mod2)
pred2=predict(mod2, type="response")
summary(pred2)
table(Train$Republican, pred2 >= .5 )
# 
# Call:
# glm(formula = Republican ~ SurveyUSA + DiffCount, family = "binomial", 
#     data = Train)
# 
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -2.04741  -0.00977   0.00561   0.03751   1.32999  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  -0.6827     1.0468  -0.652   0.5143  
# SurveyUSA     0.3309     0.2226   1.487   0.1371  
# DiffCount     0.6619     0.3663   1.807   0.0708 .
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 138.269  on 99  degrees of freedom
# Residual deviance:  11.154  on 97  degrees of freedom
# AIC: 17.154
# 
# Number of Fisher Scoring iterations: 9
# 
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000001 0.0000618 0.8568860 0.5300000 0.9994949 0.9999932 
#    
#     FALSE TRUE
#   0    45    2
#   1     1   52

# now looking at the testing dataframe

table(Test$Republican,sign(Test$Rasmussen))
#    
#     -1  0  1
#   0 18  2  4
#   1  0  0 21

TestPrediction = predict(mod2, newdata = Test, type="response")
table (Test$Republican, TestPrediction > 0.5)
#    
#     FALSE TRUE
#   0    23    1
#   1     0   21

subset(Test, TestPrediction > 0.5 & Test$Republican == 0)
#      State Year Rasmussen SurveyUSA DiffCount     PropR Republican
# 24 Florida 2012         2         0         6 0.6666667          0

