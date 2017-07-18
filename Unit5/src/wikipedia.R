# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit5/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit5/data"

wiki=read.csv("wiki.csv",stringsAsFactors=FALSE)
str(wiki)
# 'data.frame':	3876 obs. of  7 variables:
#  $ X.1     : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ X       : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ Vandal  : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ Minor   : int  1 1 0 1 1 0 0 0 1 0 ...
#  $ Loggedin: int  1 1 1 0 1 1 1 1 1 0 ...
#  $ Added   : chr  "  represent psycholinguisticspsycholinguistics orthographyorthography help text all actions through human ethno"| __truncated__ " website external links" " " " afghanistan used iran mostly that farsiis is countries some xmlspacepreservepersian parts tajikestan region" ...
#  $ Removed : chr  " " " talklanguagetalk" " regarded as technologytechnologies human first" "  represent psycholinguisticspsycholinguistics orthographyorthography help all actions through ethnologue relat"| __truncated__ ...
wiki$Vandal = as.factor(wiki$Vandal)
 
table(wiki$Vandal)
# 
#    0    1 
# 2061 1815 

library("tm")
library("SnowballC")

#WRONG, need to call VCorpus
#corpusAdded=Corpus(VectorSource(wiki$Added))
corpusAdded
# <<SimpleCorpus>>
# Metadata:  corpus specific: 1, document level (indexed): 0
# Content:  documents: 3876

corpusAdded[[1]]$content
# [1] "  represent psycholinguisticspsycholinguistics orthographyorthography help text all actions through human ethnologue relationships linguistics regarded writing languages to other listing xmlspacepreservelanguages metaverse formal term philology common each including phonologyphonology often ten list humans affiliation see computer are speechpathologyspeech our what for ways dialects please artificial written body be of quite hypothesis found alone refers by about language profanity study programming priorities rosenfelders technologytechnologies makes or first among useful languagephilosophy one sounds use area create phrases mark their genetic basic families complete but sapirwhorfhypothesissapirwhorf with talklanguagetalk population animals this science up vocal can concepts called at and topics locations as numbers have in pathology different develop 4000 things ideas grouped complex animal mathematics fairly literature httpwwwzompistcom philosophy most important meaningful a historicallinguisticsorphilologyhistorical semanticssemantics patterns the oral"

length(stopwords("english"))
# [1] 174

#corpusAdded = tm_map(corpusAdded,removeWords, c(stopwords("english")))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded[[1]]$content
# [1] "  represent psycholinguisticspsycholinguistics orthographyorthography help text  actions  human ethnologue relationships linguistics regarded writing languages   listing xmlspacepreservelanguages metaverse formal term philology common  including phonologyphonology often ten list humans affiliation see computer  speechpathologyspeech    ways dialects please artificial written body   quite hypothesis found alone refers   language profanity study programming priorities rosenfelders technologytechnologies makes  first among useful languagephilosophy one sounds use area create phrases mark  genetic basic families complete  sapirwhorfhypothesissapirwhorf  talklanguagetalk population animals  science  vocal can concepts called   topics locations  numbers   pathology different develop 4000 things ideas grouped complex animal mathematics fairly literature httpwwwzompistcom philosophy  important meaningful  historicallinguisticsorphilologyhistorical semanticssemantics patterns  oral"

corpusAdded = tm_map(corpusAdded,stemDocument)
corpusAdded[[1]]$content
# [1] "repres psycholinguisticspsycholinguist orthographyorthographi help text action human ethnologu relationship linguist regard write languag list xmlspacepreservelanguag metavers formal term philolog common includ phonologyphonolog often ten list human affili see comput speechpathologyspeech way dialect pleas artifici written bodi quit hypothesi found alon refer languag profan studi program prioriti rosenfeld technologytechnolog make first among use languagephilosophi one sound use area creat phrase mark genet basic famili complet sapirwhorfhypothesissapirwhorf talklanguagetalk popul anim scienc vocal can concept call topic locat number patholog differ develop 4000 thing idea group complex anim mathemat fair literatur httpwwwzompistcom philosophi import meaning historicallinguisticsorphilologyhistor semanticssemant pattern oral"

dtmAdded=DocumentTermMatrix(corpusAdded)
dtmAdded
# <<DocumentTermMatrix (documents: 3876, terms: 6676)>>
# Non-/sparse entries: 15369/25860807
# Sparsity           : 100%
# Maximal term length: 784
# Weighting          : term frequency (tf)
str(dtmAdded)

# VCorpus instead of Corpus, didn't need to run removeSparceTerms yet
corpusAdded = VCorpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
# <<DocumentTermMatrix (documents: 3876, terms: 6675)>>
# Non-/sparse entries: 15368/25856932
# Sparsity           : 100%
# Maximal term length: 784
# Weighting          : term frequency (tf)


#dtmAdded = removeSparseTerms(dtmAdded, 0.97)
#dtmAdded
# <<DocumentTermMatrix (documents: 3876, terms: 0)>>
# Non-/sparse entries: 0/0
# Sparsity           : 100%
# Maximal term length: 0
# Weighting          : term frequency (tf)

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded
# <<DocumentTermMatrix (documents: 3876, terms: 166)>>
# Non-/sparse entries: 2681/640735
# Sparsity           : 100%
# Maximal term length: 28
# Weighting          : term frequency (tf)

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = VCorpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
# <<DocumentTermMatrix (documents: 3876, terms: 5403)>>
# Non-/sparse entries: 13293/20928735
# Sparsity           : 100%
# Maximal term length: 784
# Weighting          : term frequency (tf)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
# <<DocumentTermMatrix (documents: 3876, terms: 162)>>
# Non-/sparse entries: 2552/625360
# Sparsity           : 100%
# Maximal term length: 28
# Weighting          : term frequency (tf)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)
# [1] 162
wikiWords = cbind(wordsAdded, wordsRemoved)
# 
#tweetsSparse$Negative = tweets$Negative
wikiWords$Vandal = wiki$Vandal

library(caTools)
set.seed(123)

split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)

trainWiki = subset(wikiWords, split==TRUE)
testWiki = subset(wikiWords, split==FALSE)

table(testWiki$Vandal)
# 
#   0   1 
# 618 545 

618/(618+545)
# [1] 0.5313844

library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~ ., data=trainWiki, method="class")

prp(wikiCART)

# Evaluate the performance of the model
predictCART = predict(wikiCART, newdata=testWiki, type="class")

#What is the accuracy of the model on the test set, using a threshold of 0.5? (Remember that if you add the argument type="class" when making predictions, the output of predict will automatically use a threshold of 0.5.)
table(testWiki$Vandal, predictCART)
#    predictCART
#       0   1
#   0 618   0
#   1 533  12

630/(630+533)
# [1] 0.5417025

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)
# 
#    0    1 
# 3659  217 

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")

prp(wikiCART2)

# Evaluate the performance of the model
predictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")

#What is the accuracy of the model on the test set, using a threshold of 0.5? (Remember that if you add the argument type="class" when making predictions, the output of predict will automatically use a threshold of 0.5.)
table(wikiTest2$Vandal, predictCART2)
#    predictCART2
#       0   1
#   0 609   9
#   1 488  57

(609+57)/(609+57+488+9)
# [1] 0.5726569

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(table(wikiWords2$NumWordsAdded))
# [1] 44.55172
sum(table(wikiWords2$NumWordsAdded))/nrow(wikiWords2)
# [1] 1

# [1] 3876
nrow(wikiWords2)
# [1] 3876

table(wikiWords2$numWordsAdded)
# 
#    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
# 1508  975  386  202  132   98   82   59   53   50   37   24   25   19   19   19 
#   16   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31 
#   15    4   14    7    5   14    6    2    6    5    5    5    1    3    2    3 
#   32   33   34   35   36   37   38   39   40   42   43   45   46   47   48   49 
#    3    2    1    2    2    3    3    1    3    3    2    3    1    4    4    2 
#   50   51   52   53   54   55   58   61   62   63   64   65   66   67   69   74 
#    4    2    1    1    1    1    1    1    3    1    3    1    1    2    1    1 
#   76   77   80   88   89   93   94   96   97  102  109  116  118  138  140  178 
#    1    1    1    1    2    1    1    1    2    1    1    3    1    1    1    1 
#  181  216  219  221  226  241  259 
#    1    1    1    1    1    1    1 

table(wikiWords2$NumWordsRemoved)

str(wikiWords2)

mean(wikiWords2$NumWordsAdded)
# [1] 4.050052

#rebuild it with the new model
wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)

wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")

prp(wikiCART3)

# Evaluate the performance of the model
predictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")

#What is the accuracy of the model on the test set, using a threshold of 0.5? (Remember that if you add the argument type="class" when making predictions, the output of predict will automatically use a threshold of 0.5.)
table(wikiTest3$Vandal, predictCART3)
#    predictCART3
#       0   1
#   0 514 104
#   1 297 248
(514+248)/(514+248+297+104)
# [1] 0.6552021

wikiWords3 = wikiWords2

#Then add the two original variables Minor and Loggedin to this new data frame:
		
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, split==TRUE)
wikiTest4 = subset(wikiWords3, split==FALSE)

wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")

prp(wikiCART4)

# Evaluate the performance of the model
predictCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")

#What is the accuracy of the model on the test set, using a threshold of 0.5? (Remember that if you add the argument type="class" when making predictions, the output of predict will automatically use a threshold of 0.5.)
table(wikiTest4$Vandal, predictCART4)
#    predictCART4
#       0   1
#   0 595  23
#   1 304 241

