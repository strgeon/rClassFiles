# TODO: Add comment
# 
# Author: Scott
###############################################################################


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
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

table(parole$male, parole$violator)
#    
#       0   1
#   0 116  14
#   1 481  64
14/78
# [1] 0.1794872

table(parole$state, parole$crime)
#    
#       1   2   3   4
#   1  66   9  34  34
#   2  42  10  64   4
#   3  42  15  20   5
#   4 165  72  35  58

library(ggplot2)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, boundary = 0, color = 'black', fill = 'cornflowerblue')
# 

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, boundary = 0,color="blue", color = 'black', fill = 'cornflowerblue')

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth=5, color="blue")

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(.~male)

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)

colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5) + scale_fill_manual(values=colorPalette)

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, alpha=0.5, position="identity") + scale_fill_manual(values=colorPalette)

ggplot(parole, aes(x = age, fill = male)) + geom_histogram(boundary=0,binwidth = 5, position = "identity", alpha = 0.5) + scale_fill_manual(values=colorPalette)


ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1, boundary = 0,color="blue", color = 'black', fill = 'cornflowerblue')

ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1, boundary = 0,color="blue", color = 'black', fill = 'cornflowerblue')+facet_grid(crime ~ .)

ggplot(data = parole, aes(x = time.served, fill=crime)) + geom_histogram(binwidth = 1, position="identity", alpha=0.5)



