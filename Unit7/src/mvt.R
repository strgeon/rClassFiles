# TODO: Add comment
# 
# Author: Scott
###############################################################################



setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit7/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit7/data"


mvt = read.csv("mvt.csv", stringsAsFactors = FALSE)
str(mvt)
# 'data.frame':	191641 obs. of  3 variables:
#  $ Date     : chr  "12/31/12 23:15" "12/31/12 22:00" "12/31/12 22:00" "12/31/12 22:00" ...
#  $ Latitude : num  41.8 41.9 42 41.8 41.8 ...
#  $ Longitude: num  -87.6 -87.7 -87.8 -87.7 -87.6 ...

mvt$Date = strptime(mvt$Date, "%m/%d/%y %H:%M")
str(mvt)
# 'data.frame':	191641 obs. of  3 variables:
#  $ Date     : POSIXlt, format: "2012-12-31 23:15:00" "2012-12-31 22:00:00" ...
#  $ Latitude : num  41.8 41.9 42 41.8 41.8 ...
#  $ Longitude: num  -87.6 -87.7 -87.8 -87.7 -87.6 ...

mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)
# 'data.frame':	191641 obs. of  5 variables:
#  $ Date     : POSIXlt, format: "2012-12-31 23:15:00" "2012-12-31 22:00:00" ...
#  $ Latitude : num  41.8 41.9 42 41.8 41.8 ...
#  $ Longitude: num  -87.6 -87.7 -87.8 -87.7 -87.6 ...
#  $ Weekday  : chr  "Monday" "Monday" "Monday" "Monday" ...
#  $ Hour     : int  23 22 22 22 21 20 20 20 19 18 ...

# Create a simple line plot - need the total number of crimes on each day of the week. We can get this information by creating a table:
table(mvt$Weekday)
# 
#    Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
#     29284     27397     27118     26316     27319     26791     27416 

# Save this table as a data frame:
WeekdayCounts = as.data.frame(table(mvt$Weekday))

str(WeekdayCounts) 
# 'data.frame':	7 obs. of  2 variables:
#  $ Var1: Factor w/ 7 levels "Friday","Monday",..: 1 2 3 4 5 6 7
#  $ Freq: int  29284 27397 27118 26316 27319 26791 27416

# Load the ggplot2 library:
library(ggplot2)

# Create our plot
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))  

# Make the "Var1" variable an ORDERED factor variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday"))

# Try again:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))
# Change our x and y labels:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), linetype=2) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

# VIDEO 4 - Adding the Hour of the Day

# Create a counts table for the weekday and hour:
table(mvt$Weekday, mvt$Hour)
#            
#                0    1    2    3    4    5    6    7    8    9   10   11   12
#   Friday    1873  932  743  560  473  602  839 1203 1268 1286  938  822 1207
#   Monday    1900  825  712  527  415  542  772 1123 1323 1235  971  737 1129
#   Saturday  2050 1267  985  836  652  508  541  650  858 1039  946  789 1204
#   Sunday    2028 1236 1019  838  607  461  478  483  615  864  884  787 1192
#   Thursday  1856  816  696  508  400  534  799 1135 1298 1301  932  731 1093
#   Tuesday   1691  777  603  464  414  520  845 1118 1175 1174  948  786 1108
#   Wednesday 1814  790  619  469  396  561  862 1140 1329 1237  947  763 1225
#            
#               13   14   15   16   17   18   19   20   21   22   23
#   Friday     857  937 1140 1165 1318 1623 1652 1736 1881 2308 1921
#   Monday     824  958 1059 1136 1252 1518 1503 1622 1815 2009 1490
#   Saturday   767  963 1086 1055 1084 1348 1390 1570 1702 2078 1750
#   Sunday     789  959 1037 1083 1160 1389 1342 1706 1696 2079 1584
#   Thursday   752  831 1044 1131 1258 1510 1537 1668 1776 2134 1579
#   Tuesday    762  908 1071 1090 1274 1553 1496 1696 1816 2044 1458
#   Wednesday  804  863 1075 1076 1289 1580 1507 1718 1748 2093 1511


# Save this to a data frame:
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))

str(DayHourCounts)
# 'data.frame':	168 obs. of  3 variables:
#  $ Var1: Factor w/ 7 levels "Friday","Monday",..: 1 2 3 4 5 6 7 1 2 3 ...
#  $ Var2: Factor w/ 24 levels "0","1","2","3",..: 1 1 1 1 1 1 1 2 2 2 ...
#  $ Freq: int  1873 1900 2050 2028 1856 1691 1814 932 825 1267 ...


# Convert the second variable, Var2, to numbers and call it Hour:
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
str(DayHourCounts)
# 'data.frame':	168 obs. of  4 variables:
#  $ Var1: Factor w/ 7 levels "Friday","Monday",..: 1 2 3 4 5 6 7 1 2 3 ...
#  $ Var2: Factor w/ 24 levels "0","1","2","3",..: 1 1 1 1 1 1 1 2 2 2 ...
#  $ Freq: int  1873 1900 2050 2028 1856 1691 1814 932 825 1267 ...
#  $ Hour: num  0 0 0 0 0 0 0 1 1 1 ...

# Create out plot:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))

# Change the colors
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=2)
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday"))

ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_line(aes(group=Var1, color=Var1), size=2)

# Separate the weekends from the weekdays:
DayHourCounts$Type = ifelse((DayHourCounts$Var1 == "Sunday") | (DayHourCounts$Var1 == "Saturday"), "Weekend", "Weekday")

# Redo our plot, this time coloring by Type:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2) 

# Make the lines a little transparent:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2, alpha=0.5) 

# Make a heatmap:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

# Change the label on the legend, and get rid of the y-label:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())

# Change the color scheme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y = element_blank())

install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)
# Load a map of Chicago into R:
chicago = get_map(location = "chicago", zoom = 11)

# Look at the map
ggmap(chicago)

# Plot the first 100 motor vehicle thefts:
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))


# Round our latitude and longitude to 2 digits of accuracy, and create a crime counts data frame for each area:
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))

str(LatLonCounts)
# 'data.frame':	1638 obs. of  3 variables:
#  $ Var1: Factor w/ 42 levels "-87.93","-87.92",..: 1 2 3 4 5 6 7 8 9 10 ...
#  $ Var2: Factor w/ 39 levels "41.64","41.65",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ Freq: int  0 0 0 0 0 0 0 0 0 0 ...

# Convert our Longitude and Latitude variable to numbers:
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))
str(LatLonCounts)
# 'data.frame':	1638 obs. of  5 variables:
#  $ Var1: Factor w/ 42 levels "-87.93","-87.92",..: 1 2 3 4 5 6 7 8 9 10 ...
#  $ Var2: Factor w/ 39 levels "41.64","41.65",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ Freq: int  0 0 0 0 0 0 0 0 0 0 ...
#  $ Long: num  -87.9 -87.9 -87.9 -87.9 -87.9 ...
#  $ Lat : num  41.6 41.6 41.6 41.6 41.6 ...

# Plot these points on our map:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq))

# Change the color scheme:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq)) + scale_colour_gradient(low="yellow", high="red")

# We can also use the geom_tile geometry
ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")

LatLonCounts2=subset(LatLonCounts,Freq>0)
str(LatLonCounts)
# 'data.frame':	1638 obs. of  5 variables:
#  $ Var1: Factor w/ 42 levels "-87.93","-87.92",..: 1 2 3 4 5 6 7 8 9 10 ...
#  $ Var2: Factor w/ 39 levels "41.64","41.65",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ Freq: int  0 0 0 0 0 0 0 0 0 0 ...
#  $ Long: num  -87.9 -87.9 -87.9 -87.9 -87.9 ...
#  $ Lat : num  41.6 41.6 41.6 41.6 41.6 ...

str(LatLonCounts2)
# 'data.frame':	686 obs. of  5 variables:
#  $ Var1: Factor w/ 42 levels "-87.93","-87.92",..: 33 40 32 33 34 35 38 39 40 41 ...
#  $ Var2: Factor w/ 39 levels "41.64","41.65",..: 1 1 2 2 2 2 2 2 2 2 ...
#  $ Freq: int  3 1 54 60 28 1 8 63 81 8 ...
#  $ Long: num  -87.6 -87.5 -87.6 -87.6 -87.6 ...
#  $ Lat : num  41.6 41.6 41.6 41.6 41.6 ...

ggmap(chicago) + geom_tile(data = LatLonCounts2, aes(x = Long, y = Lat, alpha = Freq), fill="red")

nrow(LatLonCounts)-nrow(LatLonCounts2)
# [1] 952

