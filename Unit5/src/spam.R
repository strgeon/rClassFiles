# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit5/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit5/data"


#Problem 1.1 - Loading the Dataset
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
#5728

#Problem 1.2 - Loading the Dataset
#1368
#This can be read from table(emails$spam).
table(emails$spam)
#Problem 1.3 - Loading the Dataset
#subject
emails$text[1]
emails$text[1000]
# [1] "Subject: naturally irresistible your corporate identity  lt is really hard to recollect a company : the  market is full of suqgestions and the information isoverwhelminq ; but a good  catchy logo , stylish statlonery and outstanding website  will make the task much easier .  we do not promise that havinq ordered a iogo your  company will automaticaily become a world ieader : it isguite ciear that  without good products , effective business organization and practicable aim it  will be hotat nowadays market ; but we do promise that your marketing efforts  will become much more effective . here is the list of clear  benefits : creativeness : hand - made , original logos , specially done  to reflect your distinctive company image . convenience : logo and stationery  are provided in all formats ; easy - to - use content management system letsyou  change your website content and even its structure . promptness : you  will see logo drafts within three business days . affordability : your  marketing break - through shouldn ' t make gaps in your budget . 100 % satisfaction  guaranteed : we provide unlimited amount of changes with no extra fees for you to  be surethat you will love the result of this collaboration . have a look at our  portfolio _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ not interested . . . _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _"
# [1] "Subject: 70 percent off your life insurance get a free quote instantly .  question :  are you paying too much for life insurance ?  most  likely the answer is yes !  here ' s why . fact . . . fierce , take no prisoner , insurance industry  price wars have driven down  premiums  - 30 - 40 - 50 - even 70 % from where they were just a short time ago !  that ' s why your insurance company doesn ' t want you to read this . . .  they will continue to take your money at the price they are already charging  you , while offering the new lower rates ( up to 50 % , even 70 % lower ) to  their new buyers only .  but , don ' t take our word for it . . . click  hereand request a free online quote . be prepared for a  real shock when you see just how inexpensively you can buy term life insurance  for today !  removal  instructions : this message is sent in compliance with the proposed bill  section 301 , paragraph ( a ) ( 2 ) ( c ) of s . 1618 . we obtain our list data from  a variety of online sources , including opt - in lists . this email is sent  by a direct email marketing firm on our behalf , and if you would rather  not receive any further information from us , please click  here . in this way , you can instantly opt - out from the list  your email address was obtained from , whether this was an opt - in  or otherwise . please accept our apologies if this message has reached you  in error . please allow 5 - 10 business days for your email address to be removed  from all lists in our control . meanwhile , simply delete any duplicate emails  that you may receive and rest assured that your request to be taken off  this list will be honored . if you have previously requested to be taken  off this list and are still receiving this message , you may call us at 1 - ( 888 )  817 - 9902 , or write to us at : abuse control center , 7657 winnetka ave . , canoga  park , ca 91306 "


#Problem 1.4 - Loading the Dataset
#Problem 1.5 - Loading the Dataset
#43952
max(nchar(emails$text))
# [1] 43952

#Problem 1.6 - Loading the Dataset
#1992
min(nchar(emails$text))
which(nchar(emails$text) == 13)
# [1] 13
# [1] 1992

which.min(nchar(emails$text))
# [1] 1992


#Problem 2.1 - Preparing the Corpus
#1) Build a new corpus variable called corpus.
#2) Using tm_map, convert the text to lowercase.
#3) Using tm_map, remove all punctuation from the corpus.
#4) Using tm_map, remove all English stopwords from the corpus.
#5) Using tm_map, stem the words in the corpus.
#6) Build a document term matrix from the corpus, called dtm.
#How many terms are in dtm?
#28687
library("tm")
corpus = VCorpus(VectorSource(emails$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm
# <<DocumentTermMatrix (documents: 5728, terms: 28687)>>
# Non-/sparse entries: 481719/163837417
# Sparsity           : 100%
# Maximal term length: 24
# Weighting          : term frequency (tf)



#Problem 2.2 - Preparing the Corpus
#330
spdtm = removeSparseTerms(dtm, 0.95)
spdtm
# <<DocumentTermMatrix (documents: 5728, terms: 330)>>
# Non-/sparse entries: 213551/1676689
# Sparsity           : 89%
# Maximal term length: 10
# Weighting          : term frequency (tf)


#Problem 2.3 - Preparing the Corpus
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
#enron
sort(colSums(emailsSparse))
which.max(colSums(emailsSparse))
# enron 
#    92 


#Problem 2.4 - Preparing the Corpus
emailsSparse$spam = emails$spam
6
sort(colSums(subset(emailsSparse, spam == 0)))
#     pleas   kaminski      X2000        hou       will       vinc    subject 
#      4494       4801       4935       5569       6802       8531       8625 
#       ect      enron 
#     11417      13388

#"enron", "ect", "subject", "vinc", "will", and "hou" appear at least 5000 times in the ham dataset.

#Problem 2.5 - Preparing the Corpus
3
subset(emailsSparse, spam == 1)
sort(colSums(subset(emailsSparse, spam == 1)))
#		can      email       busi       mail        com    compani       spam 
#       831        865        897        917        999       1065       1368 
#      will    subject 
#      1450       1577 

#Problem 2.6 - Preparing the Corpus
#Problem 2.7 - Preparing the Corpus
#Problem 3.1 - Building machine learning models
emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library("randomForest")
set.seed(123)

spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)
spamLog = glm(spam~., data=train, family="binomial")
spamCART = rpart(spam~., data=train, method="class")
set.seed(123)
spamRF = randomForest(spam~., data=train)
# Warning message:
# package 'rpart.plot' was built under R version 3.4.1 
# Loading required package: gplots
# 
# Attaching package: 'gplots'
# 
# The following object is masked from 'package:stats':
# 
#     lowess
# 
# randomForest 4.6-12
# Type rfNews() to see new features/changes/bug fixes.
# Warning message:
# package 'randomForest' was built under R version 3.4.1 
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 


predTrainLog = predict(spamLog, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]
#3046
#954
#10
table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)
# 
# FALSE  TRUE 
#   964  3046 
# 
# FALSE  TRUE 
#  3056   954 
# 
# FALSE  TRUE 
#  4000    10 


#Problem 3.2 - Building Machine Learning Models
#0
summary(spamLog)

#Problem 3.3 - Building Machine Learning Models
#2

prp(spamCART)

#Problem 3.4 - Building Machine Learning Models
#0.9990025

table(train$spam, predTrainLog > 0.5)
#    
#     FALSE TRUE
#   0  3052    0
#   1     4  954

(3052+954)/nrow(train)
# [1] 0.9990025


#Problem 3.5 - Building Machine Learning Models
#0.9999959
predictionTrainLog = prediction(predTrainLog, train$spam)
as.numeric(performance(predictionTrainLog, "auc")@y.values)
# [1] 0.9999959


#Problem 3.6 - Building Machine Learning Models
#0.942394
table(train$spam, predTrainCART > 0.5)
(2885+894)/nrow(train)
#    
#     FALSE TRUE
#   0  2885  167
#   1    64  894
# [1] 0.942394


#Problem 3.7 - Building Machine Learning Models
#0.9696044
predictionTrainCART = prediction(predTrainCART, train$spam)
as.numeric(performance(predictionTrainCART, "auc")@y.values)
# [1] 0.9696044


#Problem 3.8 - Building Machine Learning Models
#0.9793017
table(train$spam, predTrainRF > 0.5)
(3013+914)/nrow(train)
#    
#     FALSE TRUE
#   0  3013   39
#   1    44  914
# [1] 0.9793017


#Problem 3.9 - Building Machine Learning Models
#0.9979116
predictionTrainRF = prediction(predTrainRF, train$spam)
as.numeric(performance(predictionTrainRF, "auc")@y.values)
# [1] 0.9979116


#Problem 3.10 - Building Machine Learning Models
#Problem 4.1 - Evaluating on the Test Set
predTestLog = predict(spamLog, newdata=test, type="response")
predTestCART = predict(spamCART, newdata=test)[,2]
predTestRF = predict(spamRF, newdata=test, type="prob")[,2]
#0.9505239

table(test$spam, predTestLog > 0.5)
(1257+376)/nrow(test)
#    
#     FALSE TRUE
#   0  1257   51
#   1    34  376
# [1] 0.9505239


#Problem 4.2 - Evaluating on the Test Set
#0.9627517
predictionTestLog = prediction(predTestLog, test$spam)
as.numeric(performance(predictionTestLog, "auc")@y.values)
# [1] 0.9627517


#Problem 4.3 - Evaluating on the Test Set
#0.9394645
table(test$spam, predTestCART > 0.5)
(1228+386)/nrow(test)
#    
#     FALSE TRUE
#   0  1228   80
#   1    24  386
# [1] 0.9394645


#Problem 4.4 - Evaluating on the Test Set
#0.963176
predictionTestCART = prediction(predTestCART, test$spam)
as.numeric(performance(predictionTestCART, "auc")@y.values)
# [1] 0.963176


#Problem 4.5 - Evaluating on the Test Set
#0.975553
table(test$spam, predTestRF > 0.5)
(1290+385)/nrow(test)
#    
#     FALSE TRUE
#   0  1290   18
#   1    25  385
# [1] 0.9749709


#Problem 4.6 - Evaluating on the Test Set
#0.9975656
predictionTestRF = prediction(predTestRF, test$spam)
as.numeric(performance(predictionTestRF, "auc")@y.values)
# [1] 0.9975656


#Problem 4.7 - Evaluating on the Test Set
#Problem 4.8 - Evaluating on the Test Set

#Problem 6.1 - Integrating Word Count Information
wordCount = rowSums(as.matrix(dtm))
# Error: cannot allocate vector of size 1.2 Gb
#IMPORTANT NOTE: If you received an error message when running the command above, it might be because your computer ran out of memory when trying to convert dtm to a matrix. If this happened to you, try running the following lines of code instead to create wordCount (if you didn't get an error, you don't need to run these lines). This code is a little more cryptic, but is more memory efficient.

library(slam)
wordCount = rollup(dtm, 2, FUN=sum)$v
# 
hist(wordCount)
hist(log(wordCount))

emailsSparse$logWordCount = log(wordCount)
boxplot(emailsSparse$logWordCount~emailsSparse$spam)

train2 = subset(emailsSparse, spl == TRUE)
test2 = subset(emailsSparse, spl == FALSE)

spam2CART = rpart(spam~., data=train2, method="class")
set.seed(123)
spam2RF = randomForest(spam~., data=train2)

prp(spam2CART)

predTest2CART = predict(spam2CART, newdata=test2)[,2]
predTest2RF = predict(spam2RF, newdata=test2, type="prob")[,2]
table(test2$spam, predTest2CART > 0.5)
#    
#     FALSE TRUE
#   0  1214   94
#   1    26  384
#0.930151339
predictionTest2CART = prediction(predTest2CART, test2$spam)
as.numeric(performance(predictionTest2CART, "auc")@y.values)
# [1] 0.9582438
table(test2$spam, predTest2RF > 0.5)
#0.977881257
#     FALSE TRUE
#   0  1298   10
#   1    28  382

predictionTest2RF = prediction(predTest2RF, test2$spam)
as.numeric(performance(predictionTest2RF, "auc")@y.values)
# [1] 0.9980905

