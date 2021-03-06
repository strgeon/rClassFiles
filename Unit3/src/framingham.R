getwd()
setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit3/src")
getwd()

framingham=read.csv("framingham.csv")
str(framingham)
library(caTools)

#split the data into train and test
set.seed(1000)
split=sample.split(framingham$TenYearCHD,SplitRatio=0.65)

train=subset(framingham,split==TRUE)
test=subset(framingham,split==FALSE)

framinghamLog=glm(TenYearCHD ~ ., data=train,family=binomial)
summary(framinghamLog)
# 
# Call:
# glm(formula = TenYearCHD ~ ., family = binomial, data = train)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.8487  -0.6007  -0.4257  -0.2842   2.8369  
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -7.886574   0.890729  -8.854  < 2e-16 ***
# male             0.528457   0.135443   3.902 9.55e-05 ***
# age              0.062055   0.008343   7.438 1.02e-13 ***
# education       -0.058923   0.062430  -0.944  0.34525    
# currentSmoker    0.093240   0.194008   0.481  0.63080    
# cigsPerDay       0.015008   0.007826   1.918  0.05514 .  
# BPMeds           0.311221   0.287408   1.083  0.27887    
# prevalentStroke  1.165794   0.571215   2.041  0.04126 *  
# prevalentHyp     0.315818   0.171765   1.839  0.06596 .  
# diabetes        -0.421494   0.407990  -1.033  0.30156    
# totChol          0.003835   0.001377   2.786  0.00533 ** 
# sysBP            0.011344   0.004566   2.485  0.01297 *  
# diaBP           -0.004740   0.008001  -0.592  0.55353    
# BMI              0.010723   0.016157   0.664  0.50689    
# heartRate       -0.008099   0.005313  -1.524  0.12739    
# glucose          0.008935   0.002836   3.150  0.00163 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 2020.7  on 2384  degrees of freedom
# Residual deviance: 1792.3  on 2369  degrees of freedom
#   (371 observations deleted due to missingness)
# AIC: 1824.3
# 
# Number of Fisher Scoring iterations: 5

predictTest=predict(framinghamLog,type="response",newdata=test)
table(test$TenYearCHD,predictTest>.5)
#    
#     FALSE TRUE
#   0  1069    6
#   1   187   11

ROCRpred = prediction(predictTest,test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
# [1] 0.7421095
#sensitivity
11/(187+11)
# [1] 0.05555556
#specificity
1069/1075
# [1] 0.9944186





