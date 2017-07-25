# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit6/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit6/data"

dailykos = read.csv("dailykos.csv")

distance = dist(dailykos, method = "euclidean")

str(dailykos)

kosHierClust = hclust(distance, method="ward.D")

#plot the dendrogram
plot(kosHierClust)

kosClusters = cutree(kosHierClust, k=7)

cluster1=subset(dailykos, kosClusters==1)
cluster2=subset(dailykos, kosClusters==2)
cluster3=subset(dailykos, kosClusters==3)
cluster4=subset(dailykos, kosClusters==4)
cluster5=subset(dailykos, kosClusters==5)
cluster6=subset(dailykos, kosClusters==6)
cluster7=subset(dailykos, kosClusters==7)
# 
str(cluster2)
nrow(dailykos)
nrow(cluster2)
# [1] 3430
# [1] 321
nrow(cluster1)
nrow(cluster2)
nrow(cluster3)
nrow(cluster4)
nrow(cluster5)
nrow(cluster6)
nrow(cluster7)
# [1] 1266
# [1] 321
# [1] 374
# [1] 139
# [1] 407
# [1] 714
# [1] 209

#more sophistocated way of performing the clustering subsets
HierCluster = split(dailykos, kosClusters)

for(i in 1:7) {
	print(nrow(HierCluster[[i]]))
}

tail(sort(colMeans(cluster1)))
#      state republican       poll   democrat      kerry       bush 
#  0.7575039  0.7590837  0.9036335  0.9194313  1.0624013  1.7053712 

for(i in 1:7) {
	print(tail(sort(colMeans(HierCluster[[i]]))))
}
#      state republican       poll   democrat      kerry       bush 
#  0.7575039  0.7590837  0.9036335  0.9194313  1.0624013  1.7053712 
#      bush  democrat challenge      vote      poll  november 
#  2.847352  2.850467  4.096573  4.398754  4.847352 10.339564 
#      elect    parties      state republican   democrat       bush 
#   1.647059   1.665775   2.320856   2.524064   3.823529   4.406417 
# campaign    voter presided     poll     bush    kerry 
# 1.431655 1.539568 1.625899 3.589928 7.834532 8.438849 
#       american       presided administration            war           iraq 
#       1.090909       1.120393       1.230958       1.776413       2.427518 
#           bush 
#       3.941032 
#      race      bush     kerry     elect  democrat      poll 
# 0.4579832 0.4887955 0.5168067 0.5350140 0.5644258 0.5812325 
# democrat    clark   edward     poll    kerry     dean 
# 2.148325 2.497608 2.607656 2.765550 3.952153 5.803828 

# Specify number of clusters
k = 7

# Run k-means
set.seed(1000)
KMC = kmeans(dailykos,k)
str(KMC)
# List of 9
#  $ cluster     : int [1:3430] 4 4 6 4 1 4 7 4 4 4 ...
#  $ centers     : num [1:7, 1:1545] 0.0342 0.0556 0.0253 0.0136 0.0491 ...
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ : chr [1:7] "1" "2" "3" "4" ...
#   .. ..$ : chr [1:1545] "abandon" "abc" "ability" "abortion" ...
#  $ totss       : num 896461
#  $ withinss    : num [1:7] 76583 52693 99504 258927 88632 ...
#  $ tot.withinss: num 730632
#  $ betweenss   : num 165829
#  $ size        : int [1:7] 146 144 277 2063 163 329 308
#  $ iter        : int 7
#  $ ifault      : int 0
#  - attr(*, "class")= chr "kmeans"


# Extract clusters
kosKClusters = KMC$cluster
KMC$centers[2]
# [1] 0.05555556


kcluster1=subset(dailykos, kosKClusters==1)
nrow(kcluster1)
# [1] 146


kclusts = split(dailykos, KMC$cluster)

nrow(kclusts[[1]])
# [1] 146


for(i in 1:7) {
	print(nrow(kclusts[[i]]))
}
# [1] 146
# [1] 144
# [1] 277
# [1] 2063
# [1] 163
# [1] 329
# [1] 308

tail(sort(colMeans(kcluster1)))
#          state           iraq          kerry administration       presided 
#       1.609589       1.616438       1.636986       2.664384       2.767123 
#           bush 
#      11.431507 

for(i in 1:7) {
	print(tail(sort(colMeans(kclusts[[i]]))))
}
#          state           iraq          kerry administration       presided 
#       1.609589       1.616438       1.636986       2.664384       2.767123 
#           bush 
#      11.431507 
# primaries  democrat    edward     clark     kerry      dean 
#  2.319444  2.694444  2.798611  3.090278  4.979167  8.277778 
# administration          iraqi       american           bush            war 
#       1.389892       1.610108       1.685921       2.610108       3.025271 
#           iraq 
#       4.093863 
#      elect republican      kerry       poll   democrat       bush 
#  0.6010664  0.6175473  0.6495395  0.7474552  0.7891420  1.1473582 
#       race     senate      state    parties republican   democrat 
#   2.484663   2.650307   3.521472   3.619632   4.638037   6.993865 
#  democrat      bush challenge      vote      poll  november 
#  2.899696  2.960486  4.121581  4.446809  4.872340 10.370821 
# presided    voter campaign     poll     bush    kerry 
# 1.324675 1.334416 1.383117 2.788961 5.970779 6.480519 

# Use the table function to compare the cluster assignment of hierarchical clustering to the cluster assignment of k-means clustering.
#compare hierarchical to kmeans results
table(kosClusters, KMC$cluster)
#            
# kosClusters    1    2    3    4    5    6    7
#           1    3   11   64 1045   32    0  111
#           2    0    0    0    0    0  320    1
#           3   85   10   42   79  126    8   24
#           4   10    5    0    0    1    0  123
#           5   48    0  171  145    3    1   39
#           6    0    2    0  712    0    0    0
#           7    0  116    0   82    1    0   10

#From "table(hierGroups, KmeansCluster$cluster)", we read that 116 (80.6%) of the observations in K-Means Cluster 2 also fall in Hierarchical Cluster 7.
116/(116+2+5+10+11)
# [1] 0.8055556
1045+79+145+712+82
# [1] 2063

