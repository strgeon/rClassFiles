# TODO: Add comment
# 
# Author: Scott
###############################################################################


setwd("C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit5/data")
getwd()
# [1] "C:/Users/Scott/SkyDrive/Documents/Courses/AnalyticsEdge/Unit5/data"

trials=read.csv("clinical_trial.csv",stringsAsFactors=FALSE)
str(trials)
summary(trials)
# 'data.frame':	1860 obs. of  3 variables:
#  $ title   : chr  "Treatment of Hodgkin's disease and other cancers with 1,3-bis(2-chloroethyl)-1-nitrosourea (BCNU; NSC-409962)." "Cell mediated immune status in malignancy--pretherapy and post-therapy assessment." "Neoadjuvant vinorelbine-capecitabine versus docetaxel-doxorubicin-cyclophosphamide in early nonresponsive breas"| __truncated__ "Randomized phase 3 trial of fluorouracil, epirubicin, and cyclophosphamide alone or followed by Paclitaxel for "| __truncated__ ...
#  $ abstract: chr  "" "Twenty-eight cases of malignancies of different kinds were studied to assess T-cell activity and population bef"| __truncated__ "BACKGROUND: Among breast cancer patients, nonresponse to initial neoadjuvant chemotherapy is associated with un"| __truncated__ "BACKGROUND: Taxanes are among the most active drugs for the treatment of metastatic breast cancer, and, as a co"| __truncated__ ...
#  $ trial   : int  1 0 1 1 1 0 1 0 0 0 ...
#     title             abstract             trial       
#  Length:1860        Length:1860        Min.   :0.0000  
#  Class :character   Class :character   1st Qu.:0.0000  
#  Mode  :character   Mode  :character   Median :0.0000  
#                                        Mean   :0.4392  
#                                        3rd Qu.:1.0000  
#                                        Max.   :1.0000  

nchar(trials$abstract[2])
# [1] 1354
max(nchar(trials$abstract))
# [1] 3708

summary(nchar(trials$abstract))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       0    1196    1583    1481    1821    3708 

table(nchar(trials$abstract)==0)
# 
# FALSE  TRUE 
#  1748   112 

sum(nchar(trials$abstract) == 0)
# [1] 112

min(nchar(trials$title))
# [1] 28
which.min(nchar(trials$title))
# [1] 1258
trials$title[1258]
# [1] "A decade of letrozole: FACE."


nchar(trials$title[2])
# [1] 82
trials$title[2]
# [1] "Cell mediated immune status in malignancy--pretherapy and post-therapy assessment."

library("tm")
corpusTitle = VCorpus(VectorSource(trials$title))
corpusAbstract = VCorpus(VectorSource(trials$abstract))

#2) Convert corpusTitle and corpusAbstract to lowercase.
#3) Remove the punctuation in corpusTitle and corpusAbstract.
#4) Remove the English language stop words from corpusTitle and corpusAbstract.
#5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
#6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.

corpusTitle = VCorpus(VectorSource(trials$title))
corpusAbstract = VCorpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle , content_transformer(tolower))
corpusTitle = tm_map(corpusTitle , removePunctuation)
corpusTitle = tm_map(corpusTitle , removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle , stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)

corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))
corpusAbstract= tm_map(corpusAbstract, removePunctuation)
corpusAbstract= tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract= tm_map(corpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

#7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
#8) Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and dtmAbstract).

sparseTitle = removeSparseTerms(dtmTitle, 0.95)
sparseTitle
# <<DocumentTermMatrix (documents: 1860, terms: 31)>>
# Non-/sparse entries: 10683/46977
# Sparsity           : 81%
# Maximal term length: 15
# Weighting          : term frequency (tf)

dtmTitle = as.data.frame(as.matrix(sparseTitle))

sparseAbstract = removeSparseTerms(dtmAbstract, 0.95)
sparseAbstract 
# <<DocumentTermMatrix (documents: 1860, terms: 335)>>
# Non-/sparse entries: 91969/531131
# Sparsity           : 85%
# Maximal term length: 15
# Weighting          : term frequency (tf)

dtmAbstract= as.data.frame(as.matrix(sparseAbstract ))
summary(dtmAbstract)

#What is the most frequent word stem across all the abstracts?
which.max(colSums(dtmAbstract))

#We want to combine dtmTitle and dtmAbstract into a single data frame to make predictions. However, some of the variables in these data frames have the same names.
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

#Using cbind(), combine dtmTitle and dtmAbstract into a single data frame called dtm:
dtm = cbind(dtmTitle, dtmAbstract)

#add the dependent variable "trial" to dtm, copying it from the original data frame called trials

dtm$trial = trials$trial

str(dtm)
ncol(dtm)
# [1] 367

library(caTools)
set.seed(144)

# Now that we have prepared our data frame, it's time to split it into a training and testing set and to build regression models. Set the random seed to 144 and use the sample.split function from the caTools package to split dtm into data frames named "train" and "test", putting 70% of the data in the training set.
split = sample.split(dtm$trial, SplitRatio = 0.7)

train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)

table(train$trial)
# 
#   0   1 
# 730 572 

730/(730+572)
# [1] 0.5606759

library(rpart)
library(rpart.plot)

#Build a CART model called trialCART, using all the independent variables in the training set to train the model, and then plot the CART model. Just use the default parameters to build the model (don't add a minbucket or cp value). Remember to add the method="class" argument, since this is a classification problem.
trialCART = rpart(trial ~ ., data=train, method="class")

prp(trialCART)

#Obtain the training set predictions for the model (do not yet predict on the test set). Extract the predicted probability of a result being a trial (recall that this involves not setting a type argument, and keeping only the second column of the predict output). What is the maximum predicted probability for any result?
pred = predict(trialCART)
#pred[1:10,]
pred.prob = pred[,2]
pred.prob
max(pred.prob)
# [1] 0.8718861

#the way they did it:
predTrain = predict(trialCART)[,2]
summary(predTrain)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.05455 0.13636 0.28750 0.43932 0.78231 0.87189 

#For these questions, use a threshold probability of 0.5 to predict that an observation is a clinical trial.
#first on the trial data
predictCART = predict(trialCART, data=trial, type="class")

table(train$trial, predictCART)
#    predictCART
#       0   1
#   0 631  99
#   1 131 441


#then on the test set
predictCART2 = predict(trialCART, newdata=test, type="class")

table(test$trial, predictCART2)
#    predictCART2
#       0   1
#   0 261  52
#   1  83 162

#    predictCART
#       0   1
#   0 261  52
#   1  83 162

#accuracy
(261+162)/(261+162+83+52)
# [1] 0.7580645

#sensitivity TP/TP+FN
162/(162+83)
# [1] 0.6612245

#specificity TN/FN+TN
261/(261+52)
# [1] 0.8338658

#Using the ROCR package, what is the testing set AUC of the prediction model?
CARTd = rpart(over50k ~ ., data=train, method="class")
pred2=predict(CARTd,newdata=test)
ROCRpred = prediction(pred2[,2],test$over50k)

library(ROCR)
trialCART = rpart(trial ~ ., data=train, method="class")
pred2 = predict(trialCART, newdata=test, type="class")

#the problem I had was when I used type="class", the ROCR prediction didn't work
pred3 = predict(trialCART, newdata=test)
max(pred3)
summary(pred3)
summary(pred3[,2])
table(test$trial,pred3)
# Error in table(test$trial, pred3) : 
#   all arguments must have the same length

#    pred2
#       0   1
#   0 261  52
#   1  83 162


summary(test$trial)
predROCR = prediction((pred3[,2]), test$trial)


perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values
# [[1]]
# [1] 0.8371063

#this is what the answer said
library(ROCR)
#predTest = predict(trialCART, newdata=test)
predTest = predict(trialCART, newdata=test)[,2]
table(test$trial, predTest >= 0.5)
#    
#     FALSE TRUE
#   0   261   52
#   1    83  162


pred = prediction(predTest, test$trial)
as.numeric(performance(pred, "auc")@y.values)


#i have build a cart model namely trialCart whose code is trialCART = rpart(trial ~ ., data=train, method="class") and by using this model i have made a vector predTest whose code is predTest = predict(trialCART, newdata=test,type="class") but i am not able to find the accuracy

xyzCART = rpart(trial ~ ., data=train, method="class")
xyzpred = predict(xyzCART, newdata=test,type="class")
table(xyzpred)