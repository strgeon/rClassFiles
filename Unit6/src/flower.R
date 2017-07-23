# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit6/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit6/data"

flower = read.csv("flower.csv", header=FALSE)
str(flower)

# Change the data type to matrix
flowerMatrix = as.matrix(flower)
str(flowerMatrix)
#  num [1:50, 1:50] 0.0991 0.0991 0.1034 0.1034 0.1034 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr [1:50] "V1" "V2" "V3" "V4" ...


# Turn matrix into a vector
flowerVector = as.vector(flowerMatrix)
str(flowerVector)
#  num [1:2500] 0.0991 0.0991 0.1034 0.1034 0.1034 ...

#this doesn't work, she was just showing why you had to go to a matrix first
flowerVector2 = as.vector(flower)
str(flowerVector2)

# Compute distances
distance = dist(flowerVector, method = "euclidean")
clusterIntensity = hclust(distance, method="ward.D")
plot(clusterIntensity)
rect.hclust(clusterIntensity, k=3, border = "red")
flowerClusters = cutree(clusterIntensity, k=3)
flowerClusters

tapply(flowerVector, flowerClusters, mean)
#          1          2          3 
# 0.08574315 0.50826255 0.93147713 

dim(flowerClusters) = c(50,50)
flowerClusters
image(flowerClusters, axes=FALSE)
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))
?seq

