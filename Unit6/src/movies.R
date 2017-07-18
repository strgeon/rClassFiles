# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit6/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit6/data"


movies = read.csv("movieLens.txt", header=FALSE, sep="|", quote="\"")

str(movies)

colnames(movies) = c("ID", "Title","ReleaseDate", "VideoReleaseDate", "IMDB","Unknown","Action", "Adventure","Animation", "Childrens", "Comedy", "Crime","Documentary", "Drama", "Fantasy", "FilmNoir","Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller","War", "Western")
str(movies)
# 'data.frame':	1682 obs. of  24 variables:
#  $ ID              : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ Title           : Factor w/ 1664 levels "'Til There Was You (1997)",..: 1525 618 555 594 344 1318 1545 111 391 1240 ...
#  $ ReleaseDate     : Factor w/ 241 levels "","01-Aug-1997",..: 71 71 71 71 71 71 71 71 71 182 ...
#  $ VideoReleaseDate: logi  NA NA NA NA NA NA ...
#  $ IMDB            : Factor w/ 1661 levels "","http://us.imdb.com/M/title-exact/Independence%20(1997)",..: 1431 565 505 543 310 1661 1453 103 357 1183 ...
#  $ Unknown         : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ Action          : int  0 1 0 1 0 0 0 0 0 0 ...
#  $ Adventure       : int  0 1 0 0 0 0 0 0 0 0 ...
#  $ Animation       : int  1 0 0 0 0 0 0 0 0 0 ...
#  $ Childrens       : int  1 0 0 0 0 0 0 1 0 0 ...
#  $ Comedy          : int  1 0 0 1 0 0 0 1 0 0 ...
#  $ Crime           : int  0 0 0 0 1 0 0 0 0 0 ...
#  $ Documentary     : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ Drama           : int  0 0 0 1 1 1 1 1 1 1 ...
#  $ Fantasy         : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ FilmNoir        : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ Horror          : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ Musical         : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ Mystery         : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ Romance         : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ SciFi           : int  0 0 0 0 0 0 1 0 0 0 ...
#  $ Thriller        : int  0 1 1 0 1 0 0 0 0 0 ...
#  $ War             : int  0 0 0 0 0 0 0 0 0 1 ...
#  $ Western         : int  0 0 0 0 0 0 0 0 0 0 ...

movies$ID = NULL
movies$IMDB = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL

str(movies)

movies = unique(movies)

table(movies$Comedy == 1)
# 
# FALSE  TRUE 
#  1162   502 
table(movies$Western == 1)
# 
# FALSE  TRUE 
#  1637    27 
table(movies$Romance == 1 & movies$Drama == 1)
# 
# FALSE  TRUE 
#  1567    97 
table(movies$Romance == 1,movies$Drama == 1)
#        
#         FALSE TRUE
#   FALSE   801  619
#   TRUE    147   97

rm(split)


distances = dist(movies[2:20], method = "euclidean")
clusterMovies = hclust(distances, method = "ward.D") 
plot(clusterMovies)

clusterGroups = cutree(clusterMovies, k=10)

#see spreadsheet where she looked and named all these clusters based on what movies they contained
tapply(movies$Action, clusterGroups, mean)
#         1         2         3         4         5         6         7         8 
# 0.1784512 0.7839196 0.1238532 0.0000000 0.0000000 0.1015625 0.0000000 0.0000000 
#         9        10 
# 0.0000000 0.0000000 

tapply(movies$Romance, clusterGroups, mean)
#          1          2          3          4          5          6          7 
# 0.10437710 0.04522613 0.03669725 0.00000000 0.00000000 1.00000000 1.00000000 
#          8          9         10 
# 0.00000000 0.00000000 0.00000000 

subset(movies, Title=="Men in Black (1997)")
#                   Title Unknown Action Adventure Animation Childrens Comedy
# 257 Men in Black (1997)       0      1         1         0         0      1
#     Crime Documentary Drama Fantasy FilmNoir Horror Musical Mystery Romance
# 257     0           0     0       0        0      0       0       0       0
#     SciFi Thriller War Western
# 257     1        0   0       0

clusterGroups[257]
# 257 
#   2 

cluster2=subset(movies, clusterGroups==2)
cluster2$Title[1:10]
#  [1] GoldenEye (1995)                              
#  [2] Bad Boys (1995)                               
#  [3] Apollo 13 (1995)                              
#  [4] Net, The (1995)                               
#  [5] Natural Born Killers (1994)                   
#  [6] Outbreak (1995)                               
#  [7] Stargate (1994)                               
#  [8] Fugitive, The (1993)                          
#  [9] Jurassic Park (1993)                          
# [10] Robert A. Heinlein's The Puppet Masters (1994)
# 1664 Levels: 'Til There Was You (1997) 1-900 (1994) ... Zeus and Roxanne (1997)

#a shorter way to do it:
#In this video, we explain how you can find the cluster centroids by using the function "tapply" for each variable in the dataset. While this approach works and is familiar to us, it can be a little tedious when there are a lot of variables. An alternative approach is to use the colMeans function. With this approach, you only have one command for each cluster instead of one command for each variable. If you run the following command in your R console, you can get all of the column (variable) means for cluster 1:
colMeans(subset(movies[2:20], clusterGroups == 1))
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
# 0.006734007 0.178451178 0.185185185 0.134680135 0.393939394 0.363636364 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
# 0.033670034 0.010101010 0.306397306 0.070707071 0.000000000 0.016835017 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
# 0.188552189 0.000000000 0.104377104 0.074074074 0.040404040 0.225589226 
#     Western 
# 0.090909091 
#You can repeat this for each cluster by changing the clusterGroups number. However, if you also have a lot of clusters, this approach is not that much more efficient than just using the tapply function.
colMeans(subset(movies[2:20], clusterGroups == 2))
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
# 0.000000000 0.783919598 0.351758794 0.010050251 0.005025126 0.065326633 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
# 0.005025126 0.000000000 0.110552764 0.000000000 0.000000000 0.080402010 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
# 0.000000000 0.000000000 0.045226131 0.346733668 0.376884422 0.015075377 
#     Western 
# 0.000000000 

#A more advanced approach uses the "split" and "lapply" functions. The following command will split the data into subsets based on the clusters:
spl = split(movies[2:20], clusterGroups)

#Then you can use spl to access the different clusters, because
spl[[1]]

#is the same as
#subset(movies[2:20], clusterGroups == 1)

colMeans(spl[[1]])
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
# 0.006734007 0.178451178 0.185185185 0.134680135 0.393939394 0.363636364 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
# 0.033670034 0.010101010 0.306397306 0.070707071 0.000000000 0.016835017 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
# 0.188552189 0.000000000 0.104377104 0.074074074 0.040404040 0.225589226 
#     Western 
# 0.090909091 


lapply(spl, colMeans)
#see output at the bottom

clusterGroups2 = cutree(clusterMovies, k=2)
spl2 = split(movies[2:20], clusterGroups2)
lapply(spl2, colMeans)
# $`1`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
# 0.001545595 0.192426584 0.102782071 0.032457496 0.092735703 0.387944359 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
# 0.082689335 0.038639876 0.267387944 0.017001546 0.018547141 0.069551777 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
# 0.043276662 0.046367852 0.188562597 0.077279753 0.191653787 0.054868624 
#     Western 
# 0.020865533 
# 
# $`2`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
#           0           0           0           0           0           0 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
#           0           0           1           0           0           0 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
#           0           0           0           0           0           0 
#     Western 
#           0 





#output from colmeans(spl[[1]]) from the 10 cluster split
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
# 0.006734007 0.178451178 0.185185185 0.134680135 0.393939394 0.363636364 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
# 0.033670034 0.010101010 0.306397306 0.070707071 0.000000000 0.016835017 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
# 0.188552189 0.000000000 0.104377104 0.074074074 0.040404040 0.225589226 
#     Western 
# 0.090909091 

#so colMeans(spl[[1]]) will output the centroid of cluster 1. But an even easier approach uses the lapply function. The following command will output the cluster centroids for all clusters:

lapply(spl, colMeans)
# $`1`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
# 0.006734007 0.178451178 0.185185185 0.134680135 0.393939394 0.363636364 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
# 0.033670034 0.010101010 0.306397306 0.070707071 0.000000000 0.016835017 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
# 0.188552189 0.000000000 0.104377104 0.074074074 0.040404040 0.225589226 
#     Western 
# 0.090909091 
# 
# $`2`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
# 0.000000000 0.783919598 0.351758794 0.010050251 0.005025126 0.065326633 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
# 0.005025126 0.000000000 0.110552764 0.000000000 0.000000000 0.080402010 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
# 0.000000000 0.000000000 0.045226131 0.346733668 0.376884422 0.015075377 
#     Western 
# 0.000000000 
# 
# $`3`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
# 0.000000000 0.123853211 0.036697248 0.000000000 0.009174312 0.064220183 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
# 0.412844037 0.000000000 0.380733945 0.004587156 0.105504587 0.018348624 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
# 0.000000000 0.275229358 0.036697248 0.041284404 0.610091743 0.000000000 
#     Western 
# 0.000000000 
# 
# $`4`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
#           0           0           0           0           0           0 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
#           0           0           1           0           0           0 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
#           0           0           0           0           0           0 
#     Western 
#           0 
# 
# $`5`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
#           0           0           0           0           0           1 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
#           0           0           0           0           0           0 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
#           0           0           0           0           0           0 
#     Western 
#           0 
# 
# $`6`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
#   0.0000000   0.1015625   0.0000000   0.0000000   0.0000000   0.1093750 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
#   0.0468750   0.0000000   0.6640625   0.0000000   0.0078125   0.0156250 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
#   0.0000000   0.0000000   1.0000000   0.0000000   0.1406250   0.0000000 
#     Western 
#   0.0000000 
# 
# $`7`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
#           0           0           0           0           0           1 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
#           0           0           0           0           0           0 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
#           0           0           1           0           0           0 
#     Western 
#           0 
# 
# $`8`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
#   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0212766 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
#   0.0000000   1.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
#   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0212766 
#     Western 
#   0.0000000 
# 
# $`9`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
#           0           0           0           0           0           1 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
#           0           0           1           0           0           0 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
#           0           0           0           0           0           0 
#     Western 
#           0 
# 
# $`10`
#     Unknown      Action   Adventure   Animation   Childrens      Comedy 
#   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.1587302 
#       Crime Documentary       Drama     Fantasy    FilmNoir      Horror 
#   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   1.0000000 
#     Musical     Mystery     Romance       SciFi    Thriller         War 
#   0.0000000   0.0000000   0.0000000   0.0000000   0.1587302   0.0000000 
#     Western 
#   0.0000000 


#The lapply function runs the second argument (colMeans) on each element of the first argument (each cluster subset in spl). So instead of using 19 tapply commands, or 10 colMeans commands, we can output our centroids with just two commands: one to define spl, and then the lapply command.
#Note that if you have a variable called "split" in your current R session, you will need to remove it with rm(split) so that you can use the split function.

#Run the cutree function again to create the cluster groups, but this time pick k = 2 clusters. It turns out that the algorithm groups all of the movies that only belong to one specific genre in one cluster (cluster 2), and puts all of the other movies in the other cluster (cluster 1). What is the genre that all of the movies in cluster 2 belong to?