# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit7/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit7/data"

edges=read.csv("edges.csv")
users=read.csv("users.csv")

str(users)
# 'data.frame':	59 obs. of  4 variables:
#  $ id    : int  3981 3982 3983 3984 3985 3986 3987 3988 3989 3990 ...
#  $ gender: Factor w/ 3 levels "","A","B": 2 3 3 3 3 3 2 3 3 2 ...
#  $ school: Factor w/ 3 levels "","A","AB": 2 1 1 1 1 2 1 1 2 1 ...
#  $ locale: Factor w/ 3 levels "","A","B": 3 3 3 3 3 3 2 3 3 2 ...

str(edges)
# 'data.frame':	146 obs. of  2 variables:
#  $ V1: int  4019 4023 4023 4027 3988 3982 3994 3998 3993 3982 ...
#  $ V2: int  4026 4031 4030 4032 4021 3986 3998 3999 3995 4021 ...

vx = edges$V1 + edges$v2
nrow(vx)

s1=edges$V1

s2=append(s1, edges$V2, after = length(edges$V1))

mean(table(s2))
mean(edges$V1)

table(users$school)
# 
#     A AB 
# 40 17  2 

users$school

table(users$locale, users$school)
#    
#         A AB
#      3  0  0
#   A  6  0  0
#   B 31 17  2

table(users$gender, users$school)
#    
#         A AB
#      1  1  0
#   A 11  3  1
#   B 28 13  1

install.packages("igraph")
library(igraph)
?graph.data.frame

g = graph.data.frame(edges, FALSE, users)
str(g)
ggplot(g)
igraph(g)

g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)

degree(g)
# 3981 3982 3983 3984 3985 3986 3987 3988 3989 3990 3991 3992 3993 3994 3995  594 
#    7   13    1    0    5    8    1    6    5    3    2    2    5   10    8    3 
# 3996 3997 3998 3999 4000 4001 4002 4003 4004 4005 4006 4007 4008 4009 4010 4011 
#    3   10   13    3    8    1    6    4    9    2    1    3    0    9    0    3 
# 4012 4013 4014 4015 4016 4017 4018 4019 4020 4021 4022 4023 4024 4025 4026 4027 
#    1    5   11    0    3    8    6    7    7   10    0   17    0    3    8    6 
# 4028 4029 4030 4031 4032 4033 4034 4035 4036 4037 4038 
#    1    1   18   10    1    2    1    0    1    3    8 

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

table(V(g)$size)
# 
#    2  2.5    3  3.5    4  4.5    5  5.5    6  6.5    7  7.5  8.5 10.5   11 
#    7   10    4    9    1    4    4    3    6    2    4    1    2    1    1 

V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "B"] = "gray"
V(g)$color[V(g)$school == "AB"] = "yellow"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
