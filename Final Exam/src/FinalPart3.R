# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Final Exam/data")
getwd()

orders = read.csv("orders.csv")

str(orders)
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Final Exam/data"
# 'data.frame':	5000 obs. of  138 variables:
#  $ order_id                     : int  1597 2011 2822 2889 3971 4111 4801 5719 5846 7615 ...
#  $ order_dow                    : int  1 4 0 1 2 5 5 3 3 6 ...
#  $ order_hour_of_day            : int  8 10 8 15 18 20 17 15 10 19 ...
#  $ days_since_prior_order       : int  4 30 29 8 8 17 10 10 30 7 ...
#  $ air.fresheners.candles       : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ asian.foods                  : int  0 0 0 0 0 0 0 0 2 0 ...
#  $ baby.accessories             : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ baby.bath.body.care          : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ baby.food.formula            : int  0 0 1 0 0 0 0 0 0 0 ...

table(orders$order_hour_of_day)
# 
#   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23 
#  33  27  12   4   9  23  50 125 245 325 408 382 378 417 442 428 369 361 326 220 152 116  91  57 

mean(orders$days_since_prior_order)
# [1] 17.093
cor(orders$fresh.fruit,orders$fresh.vegetables)
# [1] 0.3955114

table(orders$frozen.pizza > 0)
# 
# FALSE  TRUE 
#  4739   261 
261/(4739+261)
# [1] 0.0522

orders.aisle = orders[, 5:ncol(orders)]
# 
set.seed(200)

library(caret)

preproc = preProcess(orders.aisle)

ordersNorm = predict(preproc, orders.aisle)
# 
max(ordersNorm$frozen.dessert)
# [1] 11.74144
min(ordersNorm$soft.drinks)
# [1] -0.2873327

distances <- dist(ordersNorm, method = "euclidean")

ClusterProducts <- hclust(distances, method = "ward.D")

plot(ClusterProducts, labels = FALSE)

set.seed(200)
clust = cutree(ClusterProducts, k=4)
# 
cluster1=subset(ordersNorm, clust==1)
cluster2=subset(ordersNorm, clust==2)
cluster3=subset(ordersNorm, clust==3)
cluster4=subset(ordersNorm, clust==4)

nrow(cluster1)
nrow(cluster2)
nrow(cluster3)
nrow(cluster4)
# [1] 2073
# [1] 443
# [1] 1843
# [1] 641


# Specify number of clusters
k = 4

# Run k-means
set.seed(200)
KMC = kmeans(ordersNorm,k)
str(KMC)
# List of 9
#  $ cluster     : int [1:5000] 3 3 2 2 3 3 2 1 2 1 ...
#  $ centers     : num [1:4, 1:134] 0.6371 -0.0516 -0.06 0.1984 -0.0171 ...
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ : chr [1:4] "1" "2" "3" "4"
#   .. ..$ : chr [1:134] "air.fresheners.candles" "asian.foods" "baby.accessories" "baby.bath.body.care" ...
#  $ totss       : num 669866
#  $ withinss    : num [1:4] 150508 230406 257077 7147
#  $ tot.withinss: num 645138
#  $ betweenss   : num 24728
#  $ size        : int [1:4] 403 1152 3409 36
#  $ iter        : int 5
#  $ ifault      : int 0
#  - attr(*, "class")= chr "kmeans"

# Extract clusters
ordKClusters = KMC$cluster
KMC$centers[2]
# [1] -0.05159538

kclusts = split(ordersNorm, KMC$cluster)
# 


nrow(kclusts[[1]])
# [1] 403

for(i in 1:7) {
	print(nrow(kclusts[[i]]))
}
# [1] 403
# [1] 1152
# [1] 3409
# [1] 36
# Error in kclusts[[i]] : subscript out of bounds


for(i in 1:4) {
	print(tail(sort(colMeans(kclusts[[i]]))))
	print("---------")
}
#           laundry    chips.pretzels       facial.care     cookies.cakes       paper.goods body.lotions.soap 
#         0.7611845         0.7693375         0.8060653         0.8575045         1.1017668         1.1180811 
# [1] "---------"
#                pasta.sauce                fresh.herbs            packaged.cheese               fresh.fruits 
#                  0.5147132                  0.5208811                  0.6405145                  0.7699809 
# packaged.vegetables.fruits           fresh.vegetables 
#                  0.8092633                  0.8942010 
# [1] "---------"
#           red.wines baby.bath.body.care           skin.care         white.wines             spirits 
#         0.006488456         0.006753500         0.007872597         0.007917013         0.011110078 
#    packaged.produce 
#         0.033738924 
# [1] "---------"
#        hot.dogs.bacon.sausage                 cookies.cakes                chips.pretzels 
#                     0.6303632                     0.6337265                     0.7812282 
#                 ice.cream.ice refrigerated.pudding.desserts                frozen.dessert 
#                     0.9941437                     1.0725432                    11.7414356 
# [1] "---------"


str(kclusts[[1]])

for(i in 1:4) {
	kclusts[1]$order_hour_of_day = ordersNorm$order_hour_of_day 
}

str(kclusts[1])

tapply(ordersNorm$order_hour_of_day,kclusts,max)
# Error in tapply(kclusts, ordersNorm$order_hour_of_day, max) : 
#   arguments must have same length

# Error in sort.list(y) : 'x' must be atomic for 'sort.list'
# Have you called 'sort' on a list?



tapply(flowerVector, flowerClusters, mean)
