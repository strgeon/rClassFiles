# TODO: Add comment
# 
# Author: Scott
###############################################################################

setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit7/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit7/data"


library("ggplot2")
library("maps")
library("ggmap")

statesMap = map_data("state")

str(statesMap)
# 'data.frame':	15537 obs. of  6 variables:
#  $ long     : num  -87.5 -87.5 -87.5 -87.5 -87.6 ...
#  $ lat      : num  30.4 30.4 30.4 30.3 30.3 ...
#  $ group    : num  1 1 1 1 1 1 1 1 1 1 ...
#  $ order    : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ region   : chr  "alabama" "alabama" "alabama" "alabama" ...
#  $ subregion: chr  NA NA NA NA ...

table(statesMap$group)
# 
#    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
#  202  149  312  516   79   91   94   10  872  381  233  329  257  256  113  397 
#   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32 
#  650  399  566   36  220   30  460  370  373  382  315  238  208   70  125  205 
#   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48 
#   78   16  290   21  168   37  733   12  105  238  284  236  172   66  304  166 
#   49   50   51   52   53   54   55   56   57   58   59   60   61   62   63 
#  289 1088   59  129   96   15  623   17   17   19   44  448  373  388   68 

length(table(statesMap$group)) 
# [1] 63

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")
# 

polling =  read.csv("PollingImputed.csv")
str(polling)
# 'data.frame':	145 obs. of  7 variables:
#  $ State     : Factor w/ 50 levels "Alabama","Alaska",..: 1 1 2 2 3 3 3 4 4 4 ...
#  $ Year      : int  2004 2008 2004 2008 2004 2008 2012 2004 2008 2012 ...
#  $ Rasmussen : int  11 21 19 16 5 5 8 7 10 13 ...
#  $ SurveyUSA : int  18 25 21 18 15 3 5 5 7 21 ...
#  $ DiffCount : int  5 5 1 6 8 9 4 8 5 2 ...
#  $ PropR     : num  1 1 1 1 1 ...
#  $ Republican: int  1 1 1 1 1 1 1 1 1 1 ...

train=subset(polling,Year < 2012)
test=subset(polling,Year == 2012)

nrow(train)
nrow(test)
nrow(polling)
# [1] 100
# [1] 45
# [1] 145

mod2 = glm(Republican~SurveyUSA+DiffCount, data=train, family="binomial")
TestPrediction = predict(mod2, newdata=test, type="response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)
str(TestPredictionBinary)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, test$State)
str(predictionDataFrame)
# 'data.frame':	45 obs. of  3 variables:
#  $ TestPrediction      : num  9.74e-01 9.99e-01 9.26e-05 9.43e-03 3.43e-05 ...
#  $ TestPredictionBinary: num  1 1 0 0 0 1 1 0 1 0 ...
#  $ test.State          : Factor w/ 50 levels "Alabama","Alaska",..: 3 4 5 6 7 9 10 11 12 13 ...

table(predictionDataFrame$TestPredictionBinary)
# 
#  0  1 
# 23 22 
table(predictionDataFrame$TestPrediction > .5)
# 
# FALSE  TRUE 
#    23    22 

str(predictionDataFrame)
# 'data.frame':	45 obs. of  3 variables:
#  $ TestPrediction      : num  9.74e-01 9.99e-01 9.26e-05 9.43e-03 3.43e-05 ...
#  $ TestPredictionBinary: num  1 1 0 0 0 1 1 0 1 0 ...
#  $ test.State          : Factor w/ 50 levels "Alabama","Alaska",..: 3 4 5 6 7 9 10 11 12 13 ...

summary(predictionDataFrame)
#  TestPrediction      TestPredictionBinary       test.State
#  Min.   :0.0000001   Min.   :0.0000       Arizona    : 1  
#  1st Qu.:0.0000926   1st Qu.:0.0000       Arkansas   : 1  
#  Median :0.0648667   Median :0.0000       California : 1  
#  Mean   :0.4852626   Mean   :0.4889       Colorado   : 1  
#  3rd Qu.:0.9986385   3rd Qu.:1.0000       Connecticut: 1  
#  Max.   :0.9998655   Max.   :1.0000       Florida    : 1  
#                                           (Other)    :39  

predictionDataFrame$region = tolower(predictionDataFrame$test.State)
#Now, merge the two data frames using the following command:
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
#Lastly, we need to make sure the observations are in order so that the map is drawn properly, by typing the following:
predictionMap = predictionMap[order(predictionMap$order),]

str(predictionMap)
# 'data.frame':	15034 obs. of  9 variables:
#  $ region              : chr  "arizona" "arizona" "arizona" "arizona" ...
#  $ long                : num  -115 -115 -115 -115 -115 ...
#  $ lat                 : num  35 35.1 35.1 35.2 35.2 ...
#  $ group               : num  2 2 2 2 2 2 2 2 2 2 ...
#  $ order               : int  204 205 206 207 208 209 210 211 212 213 ...
#  $ subregion           : chr  NA NA NA NA ...
#  $ TestPrediction      : num  0.974 0.974 0.974 0.974 0.974 ...
#  $ TestPredictionBinary: num  1 1 1 1 1 1 1 1 1 1 ...
#  $ test.State          : Factor w/ 50 levels "Alabama","Alaska",..: 3 3 3 3 3 3 3 3 3 3 ...

str(statesMap)
# 'data.frame':	15537 obs. of  6 variables:
#  $ long     : num  -87.5 -87.5 -87.5 -87.5 -87.6 ...
#  $ lat      : num  30.4 30.4 30.4 30.3 30.3 ...
#  $ group    : num  1 1 1 1 1 1 1 1 1 1 ...
#  $ order    : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ region   : chr  "alabama" "alabama" "alabama" "alabama" ...
#  $ subregion: chr  NA NA NA NA ...

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "grey", high = "black", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

tapply(predictionMap$TestPrediction, predictionMap$region == "florida", mean)
#     FALSE      TRUE 
# 0.4963901 0.9640395 

#                       
#                        FALSE TRUE
#   1.23697003202363e-07   286    0
#   1.25446252676014e-06   495    0
#   2.4296500373943e-06    238    0
#   2.43174763250483e-06   566    0
#   1.27154899595146e-05   205    0
#   1.77016906360533e-05   830    0
#   2.46484286030632e-05   545    0
#   3.43162705174894e-05    91    0
#   6.65144780263592e-05   125    0
#   9.25686131971531e-05   172    0
#   9.26152191331423e-05   516    0
#   9.2621879039047e-05    329    0
#   0.000179452619729018    70    0
#   0.000484304667422046   373    0
#   0.000484409112186808    66    0
#   0.000673996395883846   388    0
#   0.000938253566266085   399    0
#   0.00181718566508558     78    0
#   0.00351658088380677    236    0
#   0.0094329668020387      79    0
#   0.0181251615887525     734    0
#   0.0648667160642481     256    0
#   0.932548932501224      382    0
#   0.950613728930272      113    0
#   0.950620480776034      782    0
#   0.964039495344072        0  872
#   0.973902795869031      149    0
#   0.99016589378291       397    0
#   0.99016799431119       381    0
#   0.994902264916468      166    0
#   0.997364112948086     1088    0
#   0.998104918440956      373    0
#   0.998638492491219      238    0
#   0.999021911813457      315    0
#   0.999296925853894       59    0
#   0.999297026896568      257    0
#   0.999494906107201      304    0
#   0.999494942410613      962    0
#   0.999637156891794      806    0
#   0.999865524949068      208    0
#   0.999865544286216      105    0

?geom_polygon

