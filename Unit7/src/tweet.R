# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit7/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit7/data"

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE )
str(tweets)
# 'data.frame':	1181 obs. of  2 variables:
#  $ Tweet: chr  "I have to say, Apple has by far the best customer care service I have ever received! @Apple @AppStore" "iOS 7 is so fricking smooth & beautiful!! #ThanxApple @Apple" "LOVE U @APPLE" "Thank you @apple, loving my new iPhone 5S!!!!!  #apple #iphone5S pic.twitter.com/XmHJCU4pcb" ...
#  $ Avg  : num  2 2 1.8 1.8 1.8 1.8 1.8 1.6 1.6 1.6 ...

library("tm")
corpusTweet = VCorpus(VectorSource(tweets$Tweet))
corpusTweet = tm_map(corpusTweet , content_transformer(tolower))
corpusTweet = tm_map(corpusTweet , removePunctuation)
corpusTweet = tm_map(corpusTweet ,removeWords, stopwords("english"))
dtm = DocumentTermMatrix(corpusTweet)

#other stuff I'm added from reading other materials
findFreqTerms(dtm, 45)
#  [1] "apple"           "can"             "dont"            "get"            
#  [5] "ipad"            "iphone"          "ipod"            "ipodplayerpromo"
#  [9] "itunes"          "just"            "like"            "new"            
# [13] "now"             "phone"           "will"           

findAssocs(dtm, "iphone", 0.15)
# $iphone
#       thoughts          5same         choose           date httpowly22kxvv 
#           0.19           0.17           0.17           0.17           0.17 
#         5cheap           nano            new       samssung         vulgar 
#           0.16           0.16           0.16           0.16           0.16 

inspect(DocumentTermMatrix(corpusTweet,list(dictionary = c("apple", "iphone", "nano"))))
# <<DocumentTermMatrix (documents: 1181, terms: 3)>>
# Non-/sparse entries: 1387/2156
# Sparsity           : 61%
# Maximal term length: 6
# Weighting          : term frequency (tf)
# Sample             :
#      Terms
# Docs  apple iphone nano
#   13      1      1    0
#   17      1      1    0
#   20      1      1    0
#   21      1      1    0
#   22      1      1    0
#   4       1      1    0
#   7       1      1    0
#   777     1      1    1
#   8       1      1    0
#   951     1      1    1




allTweets = as.data.frame(as.matrix(dtm))
str(allTweets)
# 'data.frame':	1181 obs. of  3780 variables:
print(corpusTweet)
# <<VCorpus>>
# Metadata:  corpus specific: 0, document level (indexed): 0
# Content:  documents: 1181

inspect(corpusTweet[2:4])
# <<VCorpus>>
# Metadata:  corpus specific: 0, document level (indexed): 0
# Content:  documents: 3
# 
# [[1]]
# <<PlainTextDocument>>
# Metadata:  7
# Content:  chars: 51
# 
# [[2]]
# <<PlainTextDocument>>
# Metadata:  7
# Content:  chars: 12
# 
# [[3]]
# <<PlainTextDocument>>
# Metadata:  7
# Content:  chars: 74

# Error in print(corpusTweetRV) : object 'corpusTweetRV' not found

meta(corpusTweet[[4]])
#   author       : character(0)
#   datetimestamp: 2017-08-04 20:45:37
#   description  : character(0)
#   heading      : character(0)
#   id           : 4
#   language     : en
#   origin       : character(0)

# <<PlainTextDocument>>
# Metadata:  7
# Content:  chars: 74

inspect(corpusTweet[[4]])
# <<PlainTextDocument>>
# Metadata:  7
# Content:  chars: 74
# 
# thank  apple loving  new iphone 5s  apple iphone5s pictwittercomxmhjcu4pcb


#install.packages("wordcloud")
library(wordcloud)

#my call to wordcloud was wrong
#wordcloud(allTweets,colSums(allTweets), scale=c(3, 0.3))

#table(colSums(allTweets))
#str(allTweets)

wordcloud(colnames(allTweets), colSums(allTweets))
#random.order set to FALSE

twt = subset(tweets,Avg < 0)
corpusTweet2 = VCorpus(VectorSource(twt$Tweet))
corpusTweet2 = tm_map(corpusTweet , content_transformer(tolower))
corpusTweet2 = tm_map(corpusTweet , removePunctuation)
corpusTweet2 = tm_map(corpusTweet ,removeWords, stopwords("english"))
corpusTweet2 = tm_map(corpusTweet ,removeWords, c("apple"))
corpusTweet2 = tm_map(corpusTweet ,removeWords, c("apple", stopwords("english")))
dtm2 = DocumentTermMatrix(corpusTweet2)
allTweets2 = as.data.frame(as.matrix(dtm2))

wordcloud(colnames(allTweets2), colSums(allTweets2))

str(allTweets)
str(corpusTweet)


corpusTweet = VCorpus(VectorSource(tweets$Tweet))
corpusTweet = tm_map(corpusTweet , content_transformer(tolower))
corpusTweet = tm_map(corpusTweet , removePunctuation)
corpusTweet = tm_map(corpusTweet ,removeWords, stopwords("english"))
dtm = DocumentTermMatrix(corpusTweet)

allTweets = as.data.frame(as.matrix(dtm))

negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))

wordcloud(colnames(allTweets), colSums(allTweets),random.order=FALSE)
wordcloud(colnames(allTweets), colSums(allTweets),rot.per=.1)

install.packages("RColorBrewer")
library(RColorBrewer)

brewer.pal(Accent)
display.brewer.all()
pal <- brewer.pal(6,"Set2")
wordcloud(colnames(allTweets), colSums(allTweets),pal)
