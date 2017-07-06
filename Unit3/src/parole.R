# TODO: Add comment
# 
# Author: Scott
###############################################################################


getwd()
setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit3/src")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit3/src"

parole=read.csv("parole.csv")
str(parole)
summary(parole)
# 'data.frame':	675 obs. of  9 variables:
#  $ male             : int  1 0 1 1 1 1 1 0 0 1 ...
#  $ race             : int  1 1 2 1 2 2 1 1 1 2 ...
#  $ age              : num  33.2 39.7 29.5 22.4 21.6 46.7 31 24.6 32.6 29.1 ...
#  $ state            : int  1 1 1 1 1 1 1 1 1 1 ...
#  $ time.served      : num  5.5 5.4 5.6 5.7 5.4 6 6 4.8 4.5 4.7 ...
#  $ max.sentence     : int  18 12 12 18 12 18 18 12 13 12 ...
#  $ multiple.offenses: int  0 0 0 0 0 0 0 0 0 0 ...
#  $ crime            : int  4 3 3 1 1 4 3 1 3 2 ...
#  $ violator         : int  0 0 0 0 0 0 0 0 0 0 ...
#       male             race            age            state      
#  Min.   :0.0000   Min.   :1.000   Min.   :18.40   Min.   :1.000  
#  1st Qu.:1.0000   1st Qu.:1.000   1st Qu.:25.35   1st Qu.:2.000  
#  Median :1.0000   Median :1.000   Median :33.70   Median :3.000  
#  Mean   :0.8074   Mean   :1.424   Mean   :34.51   Mean   :2.887  
#  3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:42.55   3rd Qu.:4.000  
#  Max.   :1.0000   Max.   :2.000   Max.   :67.00   Max.   :4.000  
#   time.served     max.sentence   multiple.offenses     crime      
#  Min.   :0.000   Min.   : 1.00   Min.   :0.0000    Min.   :1.000  
#  1st Qu.:3.250   1st Qu.:12.00   1st Qu.:0.0000    1st Qu.:1.000  
#  Median :4.400   Median :12.00   Median :1.0000    Median :2.000  
#  Mean   :4.198   Mean   :13.06   Mean   :0.5363    Mean   :2.059  
#  3rd Qu.:5.200   3rd Qu.:15.00   3rd Qu.:1.0000    3rd Qu.:3.000  
#  Max.   :6.000   Max.   :18.00   Max.   :1.0000    Max.   :4.000  
#     violator     
#  Min.   :0.0000  
#  1st Qu.:0.0000  
#  Median :0.0000  
#  Mean   :0.1156  
#  3rd Qu.:0.0000  
#  Max.   :1.0000  
viol=subset(parole,violator==1)
nrow(viol)
# [1] 78
table(parole$violator)
# 
#   0   1 
# 597  78 
parole$crime=as.factor(parole$crime)
parole$state=as.factor(parole$state)
str(parole)
summary(parole)
#       male             race            age        state    time.served   
#  Min.   :0.0000   Min.   :1.000   Min.   :18.40   1:143   Min.   :0.000  
#  1st Qu.:1.0000   1st Qu.:1.000   1st Qu.:25.35   2:120   1st Qu.:3.250  
#  Median :1.0000   Median :1.000   Median :33.70   3: 82   Median :4.400  
#  Mean   :0.8074   Mean   :1.424   Mean   :34.51   4:330   Mean   :4.198  
#  3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:42.55           3rd Qu.:5.200  
#  Max.   :1.0000   Max.   :2.000   Max.   :67.00           Max.   :6.000  
#   max.sentence   multiple.offenses crime      violator     
#  Min.   : 1.00   Min.   :0.0000    1:315   Min.   :0.0000  
#  1st Qu.:12.00   1st Qu.:0.0000    2:106   1st Qu.:0.0000  
#  Median :12.00   Median :1.0000    3:153   Median :0.0000  
#  Mean   :13.06   Mean   :0.5363    4:101   Mean   :0.1156  
#  3rd Qu.:15.00   3rd Qu.:1.0000            3rd Qu.:0.0000  
#  Max.   :18.00   Max.   :1.0000            Max.   :1.0000  

# 'data.frame':	675 obs. of  9 variables:
#  $ male             : int  1 0 1 1 1 1 1 0 0 1 ...
#  $ race             : int  1 1 2 1 2 2 1 1 1 2 ...
#  $ age              : num  33.2 39.7 29.5 22.4 21.6 46.7 31 24.6 32.6 29.1 ...
#  $ state            : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
#  $ time.served      : num  5.5 5.4 5.6 5.7 5.4 6 6 4.8 4.5 4.7 ...
#  $ max.sentence     : int  18 12 12 18 12 18 18 12 13 12 ...
#  $ multiple.offenses: int  0 0 0 0 0 0 0 0 0 0 ...
#  $ crime            : Factor w/ 4 levels "1","2","3","4": 4 3 3 1 1 4 3 1 3 2 ...
#  $ violator         : int  0 0 0 0 0 0 0 0 0 0 ...

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
#rerunning without setting the seed to show that it gives a different result
split2 = sample.split(parole$violator, SplitRatio = 0.7)
train2 = subset(parole, split2 == TRUE)
test2 = subset(parole, split2 == FALSE)
sum(train!=train2)
# [1] 2057

paroleLog1=glm(violator ~ ., data=train,family="binomial")
summary(paroleLog1)
# 
# Call:
# glm(formula = violator ~ ., family = "binomial", data = train)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.7041  -0.4236  -0.2719  -0.1690   2.8375  
# 
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -4.2411574  1.2938852  -3.278  0.00105 ** 
# male               0.3869904  0.4379613   0.884  0.37690    
# race               0.8867192  0.3950660   2.244  0.02480 *  
# age               -0.0001756  0.0160852  -0.011  0.99129    
# state2             0.4433007  0.4816619   0.920  0.35739    
# state3             0.8349797  0.5562704   1.501  0.13335    
# state4            -3.3967878  0.6115860  -5.554 2.79e-08 ***
# time.served       -0.1238867  0.1204230  -1.029  0.30359    
# max.sentence       0.0802954  0.0553747   1.450  0.14705    
# multiple.offenses  1.6119919  0.3853050   4.184 2.87e-05 ***
# crime2             0.6837143  0.5003550   1.366  0.17180    
# crime3            -0.2781054  0.4328356  -0.643  0.52054    
# crime4            -0.0117627  0.5713035  -0.021  0.98357    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 340.04  on 472  degrees of freedom
# Residual deviance: 251.48  on 460  degrees of freedom
# AIC: 277.48
# 
# Number of Fisher Scoring iterations: 6

#Consider a parolee who is male, of white race, aged 50 years at 
#prison release, from the state of Maryland, served 3 months, had 
#a maximum sentence of 12 months, did not commit multiple offenses, 
#and committed a larceny

-4.2411574+
		exp(0.3869904*1)+exp(0.8867192)+exp(50*-0.0001756)+exp(3*-0.1238867)+exp(-0.1238867*12)+exp(-0.1238867)
# [1] 2.448997


predmod1=predict(paroleLog1,newdata=test,type="response")
table(test$violator,predmod1>.5)
#    
#     FALSE TRUE
#   0   167   12
#   1    11   12

12/23
# [1] 0.5217391

167/179
# [1] 0.9329609

179/(179+23)
# [1] 0.8861386

table(test$violator,predmod1>.3)
#    .3
#     FALSE TRUE
#   0   160   19
#   1     9   14

#    .7
#     FALSE TRUE
#   0   176    3
#   1    20    3



ROCRpred = prediction(predmod1,test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
# [1] 0.8945834

table(test$violator)

which.max(predmod1)
max(predmod1)

-4.2411574+exp(0.3869904*1)+exp(0.8867192)+exp(50*-0.0001756)+
		exp(3*-0.1238867)+exp(0.0802954*12)+exp(0.6837143)
# [1] 5.941579
-4.2411574 + 0.3869904*1 + 0.8867192*1- 0.0001756*50+ 0.4433007*0 + 0.8349797*0 - 3.3967878*0- 0.1238867*3 + 0.0802954*12 + 1.6119919*0 + 0.6837143*1 - 0.2781054*0 - 0.0117627*0
# [1] -1.700629
exp(-1.700629)


#From the logistic regression equation, we have 
#log(odds) = -4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age 
#+ 0.4433007*state2 + 0.8349797*state3 - 3.3967878*state4 
#- 0.1238867*time.served + 0.0802954*max.sentence + 
#1.6119919*multiple.offenses + 0.6837143*crime2 - 0.2781054*
#crime3 - 0.0117627*crime4. 
#This parolee has male=1, race=1, age=50, state2=0, state3=0, state4=0, 
#time.served=3, max.sentence=12, multiple.offenses=0, crime2=1, crime3=0, 
#crime4=0. We conclude that log(odds) = -1.700629.
#Therefore, the odds ratio is exp(-1.700629) = 0.183, and the predicted 
#probability of violation is 1/(1+exp(1.700629)) = 0.154.