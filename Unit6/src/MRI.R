# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit6/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit6/data"

healthy = read.csv("healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)
#  num [1:566, 1:646] 0.00427 0.00855 0.01282 0.01282 0.01282 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr [1:646] "V1" "V2" "V3" "V4" ...

# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

# Hierarchial clustering
healthyVector = as.vector(healthyMatrix)
#distance = dist(healthyVector, method = "euclidean")
# blows up with a error, too much memory

# We have an error - why?
str(healthyVector)
#  num [1:365636] 0.00427 0.00855 0.01282 0.01282 0.01282 ...

#advanced scree plotting they are not teaching in the course
SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))
NumClusters = seq(2,10,1)
plot(NumClusters, SumWithinss, type="b")

# Specify number of clusters
k = 5

# Run k-means
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

# Extract clusters
healthyClusters = KMC$cluster
KMC$centers[2]

# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

#cannot plot this image on eclipse, loads on RStudio ok
#image(healthyClusters, axes = FALSE, col=rainbow(k))

tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

str(tumorMatrix)
#  num [1:571, 1:512] 0 0 0 0 0 0 0 0 0 0 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr [1:512] "V1" "V2" "V3" "V4" ...

str(tumorVector)
#  num [1:292352] 0 0 0 0 0 0 0 0 0 0 ...

install.packages("flexclust")
library(flexclust)

KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)

# Visualize the clusters
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))

#can't show this image in eclipse
#image(tumorClusters, axes = FALSE, col=rainbow(k))

