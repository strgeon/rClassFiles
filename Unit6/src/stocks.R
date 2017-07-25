# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit6/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit6/data"

stocks = read.csv("StocksCluster.csv")
str(stocks)
# 'data.frame':	11580 obs. of  12 variables:
#  $ ReturnJan  : num  0.0807 -0.0107 0.0477 -0.074 -0.031 ...
#  $ ReturnFeb  : num  0.0663 0.1021 0.036 -0.0482 -0.2127 ...
#  $ ReturnMar  : num  0.0329 0.1455 0.0397 0.0182 0.0915 ...
#  $ ReturnApr  : num  0.1831 -0.0844 -0.1624 -0.0247 0.1893 ...
#  $ ReturnMay  : num  0.13033 -0.3273 -0.14743 -0.00604 -0.15385 ...
#  $ ReturnJune : num  -0.0176 -0.3593 0.0486 -0.0253 -0.1061 ...
#  $ ReturnJuly : num  -0.0205 -0.0253 -0.1354 -0.094 0.3553 ...
#  $ ReturnAug  : num  0.0247 0.2113 0.0334 0.0953 0.0568 ...
#  $ ReturnSep  : num  -0.0204 -0.58 0 0.0567 0.0336 ...
#  $ ReturnOct  : num  -0.1733 -0.2671 0.0917 -0.0963 0.0363 ...
#  $ ReturnNov  : num  -0.0254 -0.1512 -0.0596 -0.0405 -0.0853 ...
#  $ PositiveDec: int  0 0 0 1 1 1 1 0 0 0 ...
summary(stocks)
#    ReturnJan            ReturnFeb           ReturnMar        
#  Min.   :-0.7616205   Min.   :-0.690000   Min.   :-0.712994  
#  1st Qu.:-0.0691663   1st Qu.:-0.077748   1st Qu.:-0.046389  
#  Median : 0.0009965   Median :-0.010626   Median : 0.009878  
#  Mean   : 0.0126316   Mean   :-0.007605   Mean   : 0.019402  
#  3rd Qu.: 0.0732606   3rd Qu.: 0.043600   3rd Qu.: 0.077066  
#  Max.   : 3.0683060   Max.   : 6.943694   Max.   : 4.008621  
#    ReturnApr           ReturnMay          ReturnJune       
#  Min.   :-0.826503   Min.   :-0.92207   Min.   :-0.717920  
#  1st Qu.:-0.054468   1st Qu.:-0.04640   1st Qu.:-0.063966  
#  Median : 0.009059   Median : 0.01293   Median :-0.000880  
#  Mean   : 0.026308   Mean   : 0.02474   Mean   : 0.005938  
#  3rd Qu.: 0.085338   3rd Qu.: 0.08396   3rd Qu.: 0.061566  
#  Max.   : 2.528827   Max.   : 6.93013   Max.   : 4.339713  
#    ReturnJuly           ReturnAug           ReturnSep        
#  Min.   :-0.7613096   Min.   :-0.726800   Min.   :-0.839730  
#  1st Qu.:-0.0731917   1st Qu.:-0.046272   1st Qu.:-0.074648  
#  Median :-0.0008047   Median : 0.007205   Median :-0.007616  
#  Mean   : 0.0030509   Mean   : 0.016198   Mean   :-0.014721  
#  3rd Qu.: 0.0718205   3rd Qu.: 0.070783   3rd Qu.: 0.049476  
#  Max.   : 2.5500000   Max.   : 3.626609   Max.   : 5.863980  
#    ReturnOct           ReturnNov          PositiveDec    
#  Min.   :-0.685504   Min.   :-0.747171   Min.   :0.0000  
#  1st Qu.:-0.070915   1st Qu.:-0.054890   1st Qu.:0.0000  
#  Median : 0.002115   Median : 0.008522   Median :1.0000  
#  Mean   : 0.005651   Mean   : 0.011387   Mean   :0.5461  
#  3rd Qu.: 0.074542   3rd Qu.: 0.076576   3rd Qu.:1.0000  
#  Max.   : 5.665138   Max.   : 3.271676   Max.   :1.0000  

table(stocks$PositiveDec)
# 
#    0    1 
# 5256 6324 

6324/nrow(stocks)
# [1] 0.546114


cor(stocks[c("ReturnJan", "ReturnFeb", "ReturnMar", "ReturnApr", "ReturnMay", "ReturnJune", "ReturnJuly", "ReturnAug", "ReturnSep", "ReturnOct", "ReturnNov")])
#              ReturnJan   ReturnFeb    ReturnMar    ReturnApr    ReturnMay
# ReturnJan   1.00000000  0.06677458 -0.090496798 -0.037678006 -0.044411417
# ReturnFeb   0.06677458  1.00000000 -0.155983263 -0.191351924 -0.095520920
# ReturnMar  -0.09049680 -0.15598326  1.000000000  0.009726288 -0.003892789
# ReturnApr  -0.03767801 -0.19135192  0.009726288  1.000000000  0.063822504
# ReturnMay  -0.04441142 -0.09552092 -0.003892789  0.063822504  1.000000000
# ReturnJune  0.09223831  0.16999448 -0.085905486 -0.011027752 -0.021074539
# ReturnJuly -0.08142976 -0.06177851  0.003374160  0.080631932  0.090850264
# ReturnAug  -0.02279202  0.13155979 -0.022005400 -0.051756051 -0.033125658
# ReturnSep  -0.02643715  0.04350177  0.076518327 -0.028920972  0.021962862
# ReturnOct   0.14297723 -0.08732427 -0.011923758  0.048540025  0.017166728
# ReturnNov   0.06763233 -0.15465828  0.037323535  0.031761837  0.048046590
#             ReturnJune    ReturnJuly     ReturnAug     ReturnSep   ReturnOct
# ReturnJan   0.09223831 -0.0814297650 -0.0227920187 -0.0264371526  0.14297723
# ReturnFeb   0.16999448 -0.0617785094  0.1315597863  0.0435017706 -0.08732427
# ReturnMar  -0.08590549  0.0033741597 -0.0220053995  0.0765183267 -0.01192376
# ReturnApr  -0.01102775  0.0806319317 -0.0517560510 -0.0289209718  0.04854003
# ReturnMay  -0.02107454  0.0908502642 -0.0331256580  0.0219628623  0.01716673
# ReturnJune  1.00000000 -0.0291525996  0.0107105260  0.0447472692 -0.02263599
# ReturnJuly -0.02915260  1.0000000000  0.0007137558  0.0689478037 -0.05470891
# ReturnAug   0.01071053  0.0007137558  1.0000000000  0.0007407139 -0.07559456
# ReturnSep   0.04474727  0.0689478037  0.0007407139  1.0000000000 -0.05807924
# ReturnOct  -0.02263599 -0.0547089088 -0.0755945614 -0.0580792362  1.00000000
# ReturnNov  -0.06527054 -0.0483738369 -0.1164890345 -0.0197197998  0.19167279
#              ReturnNov
# ReturnJan   0.06763233
# ReturnFeb  -0.15465828
# ReturnMar   0.03732353
# ReturnApr   0.03176184
# ReturnMay   0.04804659
# ReturnJune -0.06527054
# ReturnJuly -0.04837384
# ReturnAug  -0.11648903
# ReturnSep  -0.01971980
# ReturnOct   0.19167279
# ReturnNov   1.00000000

#returns the same table
cor(stocks)

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

#Then, use the stocksTrain data frame to train a logistic regression model (name it StocksModel) to predict PositiveDec using all the other variables as independent variables. Don't forget to add the argument family=binomial to your glm command.

StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family="binomial")

summary(StocksModel)
# 
# Call:
# glm(formula = PositiveDec ~ ., family = "binomial", data = stocksTrain)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.4333  -1.2265   0.9102   1.1006   2.2611  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.14878    0.02384   6.240 4.37e-10 ***
# ReturnJan    0.31742    0.13906   2.283  0.02246 *  
# ReturnFeb   -0.29349    0.13113  -2.238  0.02521 *  
# ReturnMar    0.28716    0.14890   1.928  0.05380 .  
# ReturnApr    1.05849    0.14527   7.286 3.18e-13 ***
# ReturnMay    0.75472    0.16438   4.591 4.40e-06 ***
# ReturnJune   0.49435    0.15937   3.102  0.00192 ** 
# ReturnJuly   0.75114    0.16110   4.662 3.12e-06 ***
# ReturnAug    0.09395    0.17503   0.537  0.59142    
# ReturnSep    0.72669    0.17083   4.254 2.10e-05 ***
# ReturnOct   -0.60645    0.14452  -4.196 2.71e-05 ***
# ReturnNov   -0.84449    0.15698  -5.380 7.46e-08 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 11168  on 8105  degrees of freedom
# Residual deviance: 10951  on 8094  degrees of freedom
# AIC: 10975
# 
# Number of Fisher Scoring iterations: 4

# prediction on training set
predDec = predict(StocksModel, data=StocksTrain, type="response")
table(stocksTrain$PositiveDec, predDec > .5)
#    
#     FALSE TRUE
#   0   990 2689
#   1   787 3640

predTest = predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, predTest > .5)
#    
#     FALSE TRUE
#   0   417 1160
#   1   344 1553

#Now, let's cluster the stocks. The first step in this process is to remove the dependent variable using the following commands:
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#In the market segmentation assignment in this week's homework, you were introduced to the preProcess command from the caret package, which normalizes variables by subtracting by the mean and dividing by the standard deviation.
#In cases where we have a training and testing set, we'll want to normalize by the mean and standard deviation of the variables in the training set. We can do this by passing just the training set to the preProcess function:
		
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTrain)
#    ReturnJan          ReturnFeb          ReturnMar          ReturnApr      
#  Min.   :-4.57682   Min.   :-3.43004   Min.   :-4.54609   Min.   :-5.0227  
#  1st Qu.:-0.48271   1st Qu.:-0.35589   1st Qu.:-0.40758   1st Qu.:-0.4757  
#  Median :-0.07055   Median :-0.01875   Median :-0.05778   Median :-0.1104  
#  Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.0000  
#  3rd Qu.: 0.35898   3rd Qu.: 0.25337   3rd Qu.: 0.36106   3rd Qu.: 0.3400  
#  Max.   :18.06234   Max.   :34.92751   Max.   :24.77296   Max.   :14.6959  
#    ReturnMay          ReturnJune         ReturnJuly         ReturnAug       
#  Min.   :-4.96759   Min.   :-4.82957   Min.   :-5.19139   Min.   :-5.60378  
#  1st Qu.:-0.43045   1st Qu.:-0.45602   1st Qu.:-0.51832   1st Qu.:-0.47163  
#  Median :-0.06983   Median :-0.04354   Median :-0.02372   Median :-0.07393  
#  Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.00000  
#  3rd Qu.: 0.35906   3rd Qu.: 0.37273   3rd Qu.: 0.47735   3rd Qu.: 0.39967  
#  Max.   :42.69158   Max.   :10.84515   Max.   :17.33975   Max.   :27.14273  
#    ReturnSep          ReturnOct          ReturnNov       
#  Min.   :-5.47078   Min.   :-3.53719   Min.   :-4.31684  
#  1st Qu.:-0.39604   1st Qu.:-0.42176   1st Qu.:-0.43564  
#  Median : 0.04767   Median :-0.01891   Median :-0.01878  
#  Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.00000  
#  3rd Qu.: 0.42287   3rd Qu.: 0.37451   3rd Qu.: 0.42560  
#  Max.   :39.05435   Max.   :31.25996   Max.   :17.18255  

summary(normTest)
#    ReturnJan           ReturnFeb           ReturnMar          ReturnApr       
#  Min.   :-3.743836   Min.   :-3.251044   Min.   :-4.07731   Min.   :-4.47865  
#  1st Qu.:-0.485690   1st Qu.:-0.348951   1st Qu.:-0.40662   1st Qu.:-0.51121  
#  Median :-0.066856   Median :-0.006860   Median :-0.05674   Median :-0.11414  
#  Mean   :-0.000419   Mean   :-0.003862   Mean   : 0.00583   Mean   :-0.03638  
#  3rd Qu.: 0.357729   3rd Qu.: 0.264647   3rd Qu.: 0.35653   3rd Qu.: 0.32742  
#  Max.   : 8.412973   Max.   : 9.552365   Max.   : 9.00982   Max.   : 6.84589  
#    ReturnMay          ReturnJune         ReturnJuly          ReturnAug       
#  Min.   :-5.84445   Min.   :-4.73628   Min.   :-5.201454   Min.   :-4.62097  
#  1st Qu.:-0.43819   1st Qu.:-0.44968   1st Qu.:-0.512039   1st Qu.:-0.51546  
#  Median :-0.05346   Median :-0.02678   Median :-0.026576   Median :-0.10277  
#  Mean   : 0.02651   Mean   : 0.04315   Mean   : 0.006016   Mean   :-0.04973  
#  3rd Qu.: 0.42290   3rd Qu.: 0.43010   3rd Qu.: 0.457193   3rd Qu.: 0.38781  
#  Max.   : 7.21362   Max.   :29.00534   Max.   :12.790901   Max.   : 6.66889  
#    ReturnSep          ReturnOct           ReturnNov        
#  Min.   :-3.57222   Min.   :-3.807577   Min.   :-4.881463  
#  1st Qu.:-0.38067   1st Qu.:-0.393856   1st Qu.:-0.396764  
#  Median : 0.08215   Median : 0.006783   Median :-0.002337  
#  Mean   : 0.02939   Mean   : 0.029672   Mean   : 0.017128  
#  3rd Qu.: 0.45847   3rd Qu.: 0.419005   3rd Qu.: 0.424617  
#  Max.   : 7.09106   Max.   : 7.428466   Max.   :21.007786  

#From mean(stocksTrain$ReturnJan) and mean(stocksTest$ReturnJan), we see that the average return in January is slightly higher in the training set than in the testing set. Since normTest was constructed by subtracting by the mean ReturnJan value from the training set, this explains why the mean value of ReturnJan is slightly negative in normTest.

k = 3

# Run k-means
set.seed(144)
KMC = kmeans(normTrain,k)
str(KMC)
# List of 9
#  $ cluster     : Named int [1:8106] 1 1 1 3 1 3 2 2 1 3 ...
#   ..- attr(*, "names")= chr [1:8106] "1" "2" "4" "6" ...
#  $ centers     : num [1:3, 1:11] -0.4523 0.2641 0.7425 -0.1497 -0.0415 ...
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ : chr [1:3] "1" "2" "3"
#   .. ..$ : chr [1:11] "ReturnJan" "ReturnFeb" "ReturnMar" "ReturnApr" ...
#  $ totss       : num 89155
#  $ withinss    : num [1:3] 31204 38032 9937
#  $ tot.withinss: num 79173
#  $ betweenss   : num 9982
#  $ size        : int [1:3] 3157 4696 253
#  $ iter        : int 5
#  $ ifault      : int 0
#  - attr(*, "class")= chr "kmeans"

kaircl = split(normTrain, KMC$cluster)

table(KMC$cluster)
# 
#    1    2    3 
# 3157 4696  253 

KMC$size
# [1] 3157 4696  253

# TODO need to study this!
library(flexclust)
km.kcca = as.kcca(KMC, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
str(clusterTest)
#  Named int [1:3474] 2 1 3 1 3 2 2 3 3 3 ...
#  - attr(*, "names")= chr [1:3474] "3" "5" "15" "17" ...

table(clusterTest)
# clusterTest
#    1    2    3 
# 1298 2080   96 

str(clusterTrain)
#  Named int [1:8106] 1 1 1 3 1 3 2 2 1 3 ...
#  - attr(*, "names")= chr [1:8106] "1" "2" "4" "6" ...

summary(clusterTrain)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.000   1.000   2.000   1.642   2.000   3.000 

stocksTrain1=subset(stocksTrain,clusterTrain==1)
stocksTrain2=subset(stocksTrain,clusterTrain==2)
stocksTrain3=subset(stocksTrain,clusterTrain==3)
stocksTest1=subset(stocksTest,clusterTest==1)
stocksTest2=subset(stocksTest,clusterTest==2)
stocksTest3=subset(stocksTest,clusterTest==3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)
# [1] 0.6024707
# [1] 0.5140545
# [1] 0.4387352

StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family="binomial")
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family="binomial")
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family="binomial")

summary(StocksModel1)
# 
# Call:
# glm(formula = PositiveDec ~ ., family = "binomial", data = stocksTrain1)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.7307  -1.2910   0.8878   1.0280   1.5023  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.17224    0.06302   2.733  0.00628 ** 
# ReturnJan    0.02498    0.29306   0.085  0.93206    
# ReturnFeb   -0.37207    0.29123  -1.278  0.20139    
# ReturnMar    0.59555    0.23325   2.553  0.01067 *  
# ReturnApr    1.19048    0.22439   5.305 1.12e-07 ***
# ReturnMay    0.30421    0.22845   1.332  0.18298    
# ReturnJune  -0.01165    0.29993  -0.039  0.96901    
# ReturnJuly   0.19769    0.27790   0.711  0.47685    
# ReturnAug    0.51273    0.30858   1.662  0.09660 .  
# ReturnSep    0.58833    0.28133   2.091  0.03651 *  
# ReturnOct   -1.02254    0.26007  -3.932 8.43e-05 ***
# ReturnNov   -0.74847    0.28280  -2.647  0.00813 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 4243.0  on 3156  degrees of freedom
# Residual deviance: 4172.9  on 3145  degrees of freedom
# AIC: 4196.9
# 
# Number of Fisher Scoring iterations: 4

summary(StocksModel2)
# 
# Call:
# glm(formula = PositiveDec ~ ., family = "binomial", data = stocksTrain2)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.2012  -1.1941   0.8583   1.1334   1.9424  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.10293    0.03785   2.719 0.006540 ** 
# ReturnJan    0.88451    0.20276   4.362 1.29e-05 ***
# ReturnFeb    0.31762    0.26624   1.193 0.232878    
# ReturnMar   -0.37978    0.24045  -1.579 0.114231    
# ReturnApr    0.49291    0.22460   2.195 0.028189 *  
# ReturnMay    0.89655    0.25492   3.517 0.000436 ***
# ReturnJune   1.50088    0.26014   5.770 7.95e-09 ***
# ReturnJuly   0.78315    0.26864   2.915 0.003554 ** 
# ReturnAug   -0.24486    0.27080  -0.904 0.365876    
# ReturnSep    0.73685    0.24820   2.969 0.002989 ** 
# ReturnOct   -0.27756    0.18400  -1.509 0.131419    
# ReturnNov   -0.78747    0.22458  -3.506 0.000454 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 6506.3  on 4695  degrees of freedom
# Residual deviance: 6362.2  on 4684  degrees of freedom
# AIC: 6386.2
# 
# Number of Fisher Scoring iterations: 4


summary(StocksModel3)
# 
# Call:
# glm(formula = PositiveDec ~ ., family = "binomial", data = stocksTrain3)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.9146  -1.0393  -0.7689   1.1921   1.6939  
# 
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)  
# (Intercept) -0.181896   0.325182  -0.559   0.5759  
# ReturnJan   -0.009789   0.448943  -0.022   0.9826  
# ReturnFeb   -0.046883   0.213432  -0.220   0.8261  
# ReturnMar    0.674179   0.564790   1.194   0.2326  
# ReturnApr    1.281466   0.602672   2.126   0.0335 *
# ReturnMay    0.762512   0.647783   1.177   0.2392  
# ReturnJune   0.329434   0.408038   0.807   0.4195  
# ReturnJuly   0.774164   0.729360   1.061   0.2885  
# ReturnAug    0.982605   0.533158   1.843   0.0653 .
# ReturnSep    0.363807   0.627774   0.580   0.5622  
# ReturnOct    0.782242   0.733123   1.067   0.2860  
# ReturnNov   -0.873752   0.738480  -1.183   0.2367  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 346.92  on 252  degrees of freedom
# Residual deviance: 328.29  on 241  degrees of freedom
# AIC: 352.29
# 
# Number of Fisher Scoring iterations: 4

PredictTest1 = predict(StocksModel1,newdata = stocksTest1, type="response")
PredictTest2 = predict(StocksModel2,newdata = stocksTest2, type="response")
PredictTest3 = predict(StocksModel3,newdata = stocksTest3, type="response")

table(stocksTest1$PositiveDec,PredictTest1 > .5)
#    
#     FALSE TRUE
#   0    30  471
#   1    23  774

table(stocksTest2$PositiveDec,PredictTest2 > .5)
#    
#     FALSE TRUE
#   0   388  626
#   1   309  757


table(stocksTest3$PositiveDec,PredictTest3 > .5)
#    
#     FALSE TRUE
#   0    49   13
#   1    21   13

#To compute the overall test-set accuracy of the cluster-then-predict approach, we can combine all the test-set predictions into a single vector and all the true outcomes into a single vector:
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
AllPredictions2 = as.data.frame(c(PredictTest1, PredictTest2, PredictTest3))
AllOutcomes2 = as.data.frame(c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec))

table(AllOutcomes, AllPredictions > .5)
#            
# AllOutcomes FALSE TRUE
#           0   467 1110
#           1   353 1544


str(AllPredictions)
#  Named num [1:3474] 0.653 0.624 0.602 0.502 0.576 ...
#  - attr(*, "names")= chr [1:3474] "5" "17" "61" "67" ...

str(AllOutcomes)
#  int [1:3474] 1 0 1 0 1 0 1 1 1 1 ...

str(stocksTest1)
# 'data.frame':	1298 obs. of  12 variables:
#  $ ReturnJan  : num  -0.031 -0.1133 -0.2579 -0.0868 -0.1168 ...
#  $ ReturnFeb  : num  -0.2127 -0.0829 -0.0704 0.0468 -0.1633 ...
#  $ ReturnMar  : num  0.0915 0.1077 0.2223 0.0306 0.1714 ...
#  $ ReturnApr  : num  0.1893 0.0816 -0.0491 -0.0868 -0.1353 ...
#  $ ReturnMay  : num  -0.15385 0.02326 -0.112 0.22 -0.00934 ...
#  $ ReturnJune : num  -0.1061 -0.0251 0.1081 -0.1111 -0.1403 ...
#  $ ReturnJuly : num  0.3553 -0.079 -0.0696 0.0588 0.0397 ...
#  $ ReturnAug  : num  0.0568 0.178 0.2167 0.1972 0.0946 ...
#  $ ReturnSep  : num  0.0336 0.0973 -0.1505 -0.062 0.0347 ...
#  $ ReturnOct  : num  0.0363 -0.0105 -0.0676 0.0845 -0.0813 ...
#  $ ReturnNov  : num  -0.0853 0.0106 -0.1384 0.1599 0.0312 ...
#  $ PositiveDec: int  1 0 1 0 1 0 1 1 1 1 ...
