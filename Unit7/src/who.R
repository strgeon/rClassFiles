# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit7/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit7/data"

who = read.csv("WHO.csv")
str(who)
# 'data.frame':	194 obs. of  13 variables:
#  $ Country                      : Factor w/ 194 levels "Afghanistan",..: 1 2 3 4 5 6 7 8 9 10 ...
#  $ Region                       : Factor w/ 6 levels "Africa","Americas",..: 3 4 1 4 1 2 2 4 6 4 ...
#  $ Population                   : int  29825 3162 38482 78 20821 89 41087 2969 23050 8464 ...
#  $ Under15                      : num  47.4 21.3 27.4 15.2 47.6 ...
#  $ Over60                       : num  3.82 14.93 7.17 22.86 3.84 ...
#  $ FertilityRate                : num  5.4 1.75 2.83 NA 6.1 2.12 2.2 1.74 1.89 1.44 ...
#  $ LifeExpectancy               : int  60 74 73 82 51 75 76 71 82 81 ...
#  $ ChildMortality               : num  98.5 16.7 20 3.2 163.5 ...
#  $ CellularSubscribers          : num  54.3 96.4 99 75.5 48.4 ...
#  $ LiteracyRate                 : num  NA NA NA NA 70.1 99 97.8 99.6 NA NA ...
#  $ GNI                          : num  1140 8820 8310 NA 5230 ...
#  $ PrimarySchoolEnrollmentMale  : num  NA NA 98.2 78.4 93.1 91.1 NA NA 96.9 NA ...
#  $ PrimarySchoolEnrollmentFemale: num  NA NA 96.4 79.4 78.2 84.5 NA NA 97.5 NA ...
plot(who$GNI, who$FertilityRate)

install.packages("ggplot2")
library("ggplot2")

scatterplot=ggplot(who,aes(x = GNI,y = FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()

scatterplot + geom_point(color="blue", size=3, shape=17)
fertilityGNIplot = scatterplot + geom_point(color="darkred", size=3, shape=8) + ggtitle("Fertility Rate vs. Gross National Income")
fertilityGNIplot

pdf("MyPlot.pdf")
print(fertilityGNIplot)
dev.off()

sink("colors.txt", append=FALSE, split=FALSE)
colors()
sink()

scatterplot + geom_point(color="darkred", size=3, shape=15) 

ggplot(who,aes(x = GNI,y = FertilityRate, color=Region)) + geom_point()
ggplot(who,aes(x = GNI,y = FertilityRate, color=LifeExpectancy)) + geom_point()

ggplot(who,aes(x = GNI,y = FertilityRate, color=LifeExpectancy)) + geom_point()

# Is the fertility rate of a country was a good predictor of the percentage of the population under 15?
ggplot(who, aes(x = FertilityRate, y = Under15)) + geom_point()

# Let's try a log transformation:
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point()

# Simple linear regression model to predict the percentage of the population under 15, using the log of the fertility rate:
mod = lm(Under15 ~ log(FertilityRate), data = who)
summary(mod)
# 
# Call:
# lm(formula = Under15 ~ log(FertilityRate), data = who)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -10.3131  -1.7742   0.0446   1.7440   7.7174 
# 
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          7.6540     0.4478   17.09   <2e-16 ***
# log(FertilityRate)  22.0547     0.4175   52.82   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.65 on 181 degrees of freedom
#   (11 observations deleted due to missingness)
# Multiple R-squared:  0.9391,	Adjusted R-squared:  0.9387 
# F-statistic:  2790 on 1 and 181 DF,  p-value: < 2.2e-16


# Add this regression line to our plot:
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm")

# 99% confidence interval
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)

# No confidence interval in the plot
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = FALSE)

# Change the color of the regression line:
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", color = "orange")

ggplot(who, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() +scale_color_brewer(palette="Dark2")

