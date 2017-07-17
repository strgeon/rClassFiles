# TODO: Add comment
# 
# Author: Scott
###############################################################################


getwd()
setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit5/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit5/data"

sw = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "would", "should", "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're", "they're", "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", "what's", "here's", "there's", "when's", "where's", "why's", "how's", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very")

tweets=read.csv("tweets.csv",stringsAsFactors=FALSE)
str(tweets)
# 'data.frame':	1181 obs. of  2 variables:
#  $ Tweet: chr  "I have to say, Apple has by far the best customer care service I have ever received! @Apple @AppStore" "iOS 7 is so fricking smooth & beautiful!! #ThanxApple @Apple" "LOVE U @APPLE" "Thank you @apple, loving my new iPhone 5S!!!!!  #apple #iphone5S pic.twitter.com/XmHJCU4pcb" ...
#  $ Avg  : num  2 2 1.8 1.8 1.8 1.8 1.8 1.6 1.6 1.6 ...

tweets$Negative = as.factor(tweets$Avg <= -1 )
table(tweets$Negative) 
# 
# FALSE  TRUE 
#   999   182 

install.packages("tm")
install.packages("SnowballC")
library("tm")
library("SnowballC")


corpus=Corpus(VectorSource(tweets$Tweet))
corpus
# <<SimpleCorpus>>
# Metadata:  corpus specific: 1, document level (indexed): 0
# Content:  documents: 1181
corpus[[1]]$content
# [1] "I have to say, Apple has by far the best customer care service I have ever received! @Apple @AppStore"
corpus = tm_map(corpus,tolower)
corpus[[1]]$content 
# [1] "i have to say, apple has by far the best customer care service i have ever received! @apple @appstore"

corpus = tm_map(corpus,removePunctuation)
corpus[[1]]$content 
# [1] "i have to say apple has by far the best customer care service i have ever received apple appstore"

stopwords("english")[1:10]
#  [1] "i"         "me"        "my"        "myself"    "we"        "our"      
#  [7] "ours"      "ourselves" "you"       "your"     

corpus = tm_map(corpus,removeWords, c("apple", stopwords("english")))
corpus[[1]]$content
# [1] "   say    far  best customer care service   ever received  appstore"

corpus = tm_map(corpus,stemDocument)
corpus[[1]]$content
# [1] "say far best custom care servic ever receiv appstor"

frequencies=DocumentTermMatrix(corpus)
str(frequencies)
# List of 6
#  $ i       : int [1:8980] 1 1 1 1 1 1 1 1 1 2 ...
#  $ j       : int [1:8980] 1 2 3 4 5 6 7 8 9 10 ...
#  $ v       : num [1:8980] 1 1 1 1 1 1 1 1 1 1 ...
#  $ nrow    : int 1181
#  $ ncol    : int 3289
#  $ dimnames:List of 2
#   ..$ Docs : chr [1:1181] "1" "2" "3" "4" ...
#   ..$ Terms: chr [1:3289] "appstor" "best" "care" "custom" ...
#  - attr(*, "class")= chr [1:2] "DocumentTermMatrix" "simple_triplet_matrix"
#  - attr(*, "weighting")= chr [1:2] "term frequency" "tf"

inspect(frequencies[1000:1005,505:515])
# <<DocumentTermMatrix (documents: 6, terms: 11)>>
# Non-/sparse entries: 1/65
# Sparsity           : 98%
# Maximal term length: 23
# Weighting          : term frequency (tf)
# Sample             :
#       Terms
# Docs   asap courtsideassistappforio current follow idea kickbutt preinstal save
#   1000    0                       0       0      0    1        0         0    0
#   1001    0                       0       0      0    0        0         0    0
#   1002    0                       0       0      0    0        0         0    0
#   1003    0                       0       0      0    0        0         0    0
#   1004    0                       0       0      0    0        0         0    0
#   1005    0                       0       0      0    0        0         0    0
#       Terms
# Docs   ssd support
#   1000   0       0
#   1001   0       0
#   1002   0       0
#   1003   0       0
#   1004   0       0
#   1005   0       0

findFreqTerms(frequencies,lowfreq=20)
#  [1] "say"                  "love"                 "iphon"               
#  [4] "iphone5"              "new"                  "thank"               
#  [7] "phone"                "can"                  "make"                
# [10] "market"               "one"                  "will"                
# [13] "cant"                 "get"                  "just"                
# [16] "updat"                "fingerprint"          "iphone5c"            
# [19] "store"                "time"                 "come"                
# [22] "now"                  "use"                  "back"                
# [25] "anyon"                "work"                 "app"                 
# [28] "android"              "think"                "ipad"                
# [31] "well"                 "freak"                "dont"                
# [34] "via"                  "better"               "like"                
# [37] "pleas"                "samsung"              "want"                
# [40] "batteri"              "ios7"                 "microsoft"           
# [43] "itun"                 "buy"                  "releas"              
# [46] "look"                 "appl"                 "need"                
# [49] "googl"                "twitter"              "ipod"                
# [52] "ipodplayerpromo"      "promoipodplayerpromo" "lol"                 
# [55] "realli"               "promo"               

sparce=removeSparseTerms(frequencies, 0.995)
str(sparce)
# List of 6
#  $ i       : int [1:4669] 1 1 1 1 1 1 1 3 4 4 ...
#  $ j       : int [1:4669] 1 2 3 4 5 6 7 8 9 10 ...
#  $ v       : num [1:4669] 1 1 1 1 1 1 1 1 1 1 ...
#  $ nrow    : int 1181
#  $ ncol    : int 309
#  $ dimnames:List of 2
#   ..$ Docs : chr [1:1181] "1" "2" "3" "4" ...
#   ..$ Terms: chr [1:309] "appstor" "best" "care" "custom" ...
#  - attr(*, "class")= chr [1:2] "DocumentTermMatrix" "simple_triplet_matrix"
#  - attr(*, "weighting")= chr [1:2] "term frequency" "tf"
sparce
# <<DocumentTermMatrix (documents: 1181, terms: 309)>>
# Non-/sparse entries: 4669/360260
# Sparsity           : 99%
# Maximal term length: 20
# Weighting          : term frequency (tf)

tweetSparse=as.data.frame(as.matrix(sparce))
str(tweetSparse)

colnames(tweetSparse) = make.names(colnames(tweetSparse))
tweetSparse$Negative = tweets$Negative

library(caTools)
set.seed(123)

split = sample.split(tweetSparse$Negative, SplitRatio=0.7)
trainSparse = subset(tweetSparse, split==TRUE)
testSparse = subset(tweetSparse, split==FALSE)
findFreqTerms(frequencies,lowfreq=100)
# [1] "iphon" "new"   "itun" 

library("rpart")
library("rpart.plot")

tweetCART=rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

predictCART=predict(tweetCART,newdata=testSparse,type="class")

table(testSparse$Negative,predictCART)
#        predictCART
#         FALSE TRUE
#   FALSE   294    6
#   TRUE     37   18

(294+18)/(294+18+37+6)
# [1] 0.8788732
# 


table(testSparse$Negative)
# 
# FALSE  TRUE 
#   300    55 

#the baseline model above assumes non-negative by the following calc
300/355
# [1] 0.8450704

library("randomForest")
set.seed(123)

tweetRF=randomForest(Negative ~ ., data=trainSparse)
predictRF=predict(tweetRF,newdata=testSparse)
table(testSparse$Negative,predictRF)
#        predictRF
#         FALSE TRUE
#   FALSE   293    7
#   TRUE     34   21
(293+21)/(293+21+7+34)
# [1] 0.884507

tweetLog=glm(Negative ~ ., data=trainSparse,family="binomial")
predictions = predict(tweetLog, newdata=testSparse, type="response")

table(testSparse$Negative,predictions>0.5)
#        
#         FALSE TRUE
#   FALSE   251   49
#   TRUE     19   36

(251+36)/(251+36+19+49)
# [1] 0.8084507
